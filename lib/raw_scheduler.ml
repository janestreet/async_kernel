open Core.Std

module Execution_context = Execution_context
module Clock_event = Raw_clock_event
module Ivar = Raw_ivar
module Tail = Raw_tail

let debug = Debug.scheduler

type 'a tail = ('a, Execution_context.t) Tail.t with sexp_of

module T = struct
  type t =
    {(* [check_access] optionally holds a function to run to check whether access to [t]
        is currently allowed.  It is used to detect invalid access to the scheduler from a
        thread. *)
      mutable check_access : (unit -> unit) option;
      jobs : Execution_context.t Job.t Jobs.t sexp_opaque;
      mutable main_execution_context    : Execution_context.t;
      mutable current_execution_context : Execution_context.t;
      mutable max_num_jobs_per_priority_per_cycle : int;
      (* [uncaught_exn] is set to [Some error] as soon as an exception bubbles to the top
         of the monitor tree without being handled.  We guarantee to never run another job
         after this by clearing [jobs] and never adding another job. *)
      mutable uncaught_exn : Error.t option;
      mutable num_jobs_run : int;
      mutable cycle_count : int;
      mutable cycle_start : Time.t;
      mutable last_cycle_time : Time.Span.t;
      cycle_times : Time.Span.t tail;
      mutable last_cycle_num_jobs : int;
      cycle_num_jobs : int tail;
      events : Execution_context.t Clock_event.t Events.t;
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
      ~max_num_jobs_per_priority_per_cycle:
      (check (fun max_num_jobs_per_priority_per_cycle ->
        assert (max_num_jobs_per_priority_per_cycle > 0)))
      ~uncaught_exn:(check (fun uncaught_exn ->
        if is_some uncaught_exn then assert (Jobs.is_empty t.jobs)))
      ~num_jobs_run:(check (fun num_jobs_run -> assert (num_jobs_run >= 0)))
      ~cycle_count:(check (fun cycle_count -> assert (cycle_count >= 0)))
      ~cycle_start:ignore
      ~last_cycle_time:ignore
      ~cycle_times:ignore
      ~last_cycle_num_jobs:(check (fun last_cycle_num_jobs ->
        assert (last_cycle_num_jobs >= 0)))
      ~cycle_num_jobs:ignore
      ~events:(check (fun events ->
        Events.invariant events;
        try
          Events.iter events ~f:(fun events_event ->
            let event = Events.Event.value events_event in
            let module E = Clock_event in
            match event.E.state with
            | E.Waiting { E. event = events_event'; ready } ->
              assert (phys_equal events_event events_event');
              assert (Ivar.is_empty ready);
            | _ -> assert false);
        with exn ->
          failwiths "events problem" (exn, events)
            (<:sexp_of< exn * Execution_context.t Clock_event.t Events.t >>)));
  with exn ->
    failwiths "Scheduler.invariant failed" (exn, t) <:sexp_of< exn * t >>
;;

let create () =
  let now = Time.epoch in
  { check_access = None;
    jobs = Jobs.create ~dummy:(Job.create Execution_context.main Fn.ignore ());
    current_execution_context = Execution_context.main;
    main_execution_context = Execution_context.main;
    max_num_jobs_per_priority_per_cycle = 500;
    uncaught_exn = None;
    num_jobs_run = 0;
    cycle_start = now;
    cycle_count = 0;
    last_cycle_time = sec 0.;
    cycle_times = Tail.create ();
    last_cycle_num_jobs = 0;
    cycle_num_jobs = Tail.create ();
    events = Events.create ~now;
  }
;;

let set_check_access t f = t.check_access <- Some f

let t_ref = ref (create ())

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
  if Config.record_backtraces
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
  Jobs.clear t.jobs;
  t.uncaught_exn <- Some error;
;;
