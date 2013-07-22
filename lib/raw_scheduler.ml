open Core.Std
open Import

module Monitor = Raw_monitor

let debug = Debug.scheduler

module T = struct
  type t =
    {(* [check_access] optionally holds a function to run to check whether access to [t]
        is currently allowed.  It is used to detect invalid access to the scheduler from a
        thread. *)
      mutable check_access : (unit -> unit) option;
      jobs : Jobs.t;
      mutable main_execution_context    : Execution_context.t;
      mutable current_execution_context : Execution_context.t;
      (* [uncaught_exn] is set to [Some error] as soon as an exception bubbles to the top
         of the monitor tree without being handled.  We guarantee to never run another job
         after this by clearing [jobs] and never adding another job. *)
      mutable uncaught_exn : Error.t option;
      mutable global_kill_index : Kill_index.t;
      mutable num_jobs_run : int;
      mutable cycle_count : int;
      mutable cycle_start : Time.t;
      mutable run_every_cycle_start : (unit -> unit) list;
      mutable last_cycle_time : Time.Span.t;
      mutable last_cycle_num_jobs : int;
      events : Job.t Timing_wheel.t;
      (* OCaml finalizers can run at any time and in any thread.  When an OCaml finalizer
         fires, we have it put a job that runs the async finalizer in the thread-safe
         queue of [finalizer_jobs], and call [thread_safe_finalizer_hook].  In
         [run_cycle], we pull the jobs off [finalizer_jobs] and add them to the
         scheduler. *)
      finalizer_jobs : Job.t Thread_safe_queue.t sexp_opaque;
      mutable thread_safe_finalizer_hook : (unit -> unit);

      (* configuration *)
      mutable check_invariants : bool;
      mutable max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t;
      mutable record_backtraces : bool;
    }
  with fields, sexp_of
end

include T

let invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~check_access:ignore
      ~jobs:(check Jobs.invariant)
      ~main_execution_context:(check Execution_context.invariant)
      ~current_execution_context:(check Execution_context.invariant)
      ~uncaught_exn:(check (fun uncaught_exn ->
        if is_some uncaught_exn then assert (Jobs.is_empty t.jobs)))
      ~global_kill_index:(check (fun kill_index ->
        Kill_index.invariant kill_index;
        assert (not (Kill_index.equal kill_index Kill_index.dead))))
      ~num_jobs_run:(check (fun num_jobs_run -> assert (num_jobs_run >= 0)))
      ~cycle_count:(check (fun cycle_count -> assert (cycle_count >= 0)))
      ~cycle_start:ignore
      ~run_every_cycle_start:ignore
      ~last_cycle_time:ignore
      ~last_cycle_num_jobs:(check (fun last_cycle_num_jobs ->
        assert (last_cycle_num_jobs >= 0)))
      ~events:(check (Timing_wheel.invariant Job.invariant))
      ~finalizer_jobs:ignore
      ~thread_safe_finalizer_hook:ignore
      ~check_invariants:ignore
      ~max_num_jobs_per_priority_per_cycle:ignore
      ~record_backtraces:ignore
    ;
  with exn ->
    failwiths "Scheduler.invariant failed" (exn, t) <:sexp_of< exn * t >>
;;

let create () =
  let now = Time.now () in
  let events =
    Timing_wheel.create
      ~start:now
      ~alarm_precision:Config.alarm_precision
      ~level_bits:Config.timing_wheel_level_bits
      ~dummy:Job.do_nothing
      ()
  in
  { check_access = None;
    jobs = Jobs.create ();
    current_execution_context = Execution_context.main;
    main_execution_context = Execution_context.main;
    uncaught_exn = None;
    global_kill_index = Kill_index.initial;
    num_jobs_run = 0;
    cycle_start = now;
    cycle_count = 0;
    run_every_cycle_start = [];
    last_cycle_time = sec 0.;
    last_cycle_num_jobs = 0;
    events;
    finalizer_jobs = Thread_safe_queue.create ();
    thread_safe_finalizer_hook = ignore;
    (* configuration *)
    check_invariants                    = Config.check_invariants;
    max_num_jobs_per_priority_per_cycle = Config.max_num_jobs_per_priority_per_cycle;
    record_backtraces                   = Config.record_backtraces;
  }
;;

let is_dead t = is_some t.uncaught_exn

let set_check_access t f = t.check_access <- f

let t_ref =
  match Result.try_with create with
  | Ok t -> ref t
  | Error exn ->
    eprintf "%s\n" (Exn.to_string exn);
    exit 1;
;;

let check_access t =
  match t.check_access with
  | None -> ()
  | Some f -> f ()
;;

let t () =
  let t = !t_ref in
  check_access t;
  t
;;

let current_execution_context t =
  let execution_context = t.current_execution_context in
  if t.record_backtraces
  then Execution_context.record_backtrace execution_context
  else execution_context
;;

let set_execution_context t execution_context =
  (* Avoid a caml_modify in most cases. *)
  if not (phys_equal t.current_execution_context execution_context)
  then t.current_execution_context <- execution_context;
;;

let with_execution_context t tmp_context ~f =
  let old_context = current_execution_context t in
  set_execution_context t tmp_context;
  protect ~f ~finally:(fun () -> set_execution_context t old_context);
;;

let add_job t job =
  let priority = (Job.execution_context job).Execution_context.priority in
  if debug then Debug.log "enqueing job" priority <:sexp_of< Priority.t >>;
  (* If there's been an uncaught exn, we don't add the job, since we don't want
     any jobs to run once there's been an uncaught exn. *)
  if is_none t.uncaught_exn then Jobs.add t.jobs priority job;
;;

let got_uncaught_exn t error =
  if debug then Debug.log "got_uncaught_exn" error <:sexp_of< Error.t >>;
  Jobs.clear t.jobs;
  t.uncaught_exn <- Some error;
;;

let monitor_is_alive t monitor =
  Monitor.update_kill_index monitor ~global_kill_index:t.global_kill_index;
  Kill_index.equal monitor.Monitor.kill_index t.global_kill_index
;;

let execution_context_is_alive t execution_context =
  let module E = Execution_context in
  Kill_index.equal execution_context.E.kill_index t.global_kill_index
  || (not (Kill_index.equal execution_context.E.kill_index Kill_index.dead)
      && let monitor = execution_context.E.monitor in
         let b = monitor_is_alive t monitor in
         execution_context.E.kill_index <- monitor.Monitor.kill_index;
         b)
;;

let kill_monitor t monitor =
  if Debug.monitor then
    Debug.log "kill_monitor" monitor <:sexp_of< Monitor.t >>;
  monitor.Monitor.kill_index <- Kill_index.dead;
  t.global_kill_index <- Kill_index.next t.global_kill_index;
;;

let stabilize t =
  let jobs = t.jobs in
  Jobs.start_cycle jobs
    ~max_num_jobs_per_priority:(Max_num_jobs_per_priority_per_cycle.create_exn Int.max_value);
  Jobs.run_all jobs Job.run;
;;
