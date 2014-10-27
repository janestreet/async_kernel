open Core.Std
open Import    let _ = _squelch_unused_module_warning_

module Monitor = Raw_monitor

let debug = Debug.scheduler

module Job = Jobs.Job

module T = struct
  type t =
    {(* [check_access] optionally holds a function to run to check whether access to [t]
        is currently allowed.  It is used to detect invalid access to the scheduler from a
        thread. *)
      mutable check_access                        : (unit -> unit) option
    ; jobs                                        : Jobs.t
    ; mutable main_execution_context              : Execution_context.t
    ; mutable cycle_count                         : int
    ; mutable cycle_start                         : Time.t
    ; mutable run_every_cycle_start               : (unit -> unit) list
    ; mutable last_cycle_time                     : Time.Span.t
    ; mutable last_cycle_num_jobs                 : int
    ; events                                      : Job.t Timing_wheel.t
    (* [external_actions] is a queue of actions sent from outside of async.  This is for
       the case where we want to schedule a job or fill an ivar from a context where it is
       not safe to run async code, because the async lock isn't held.  For instance: - in
       an OCaml finalizer, as they can run at any time in any thread.

       The way to do it is to queue a thunk in [external_actions] and call
       [thread_safe_external_action_hook], which is responsible for notifying the
       scheduler that new actions are available.  [thread_safe_external_action_hook] is
       set in [Async_unix] to call [Interruptor.thread_safe_interrupt], which will wake up
       the [Async_unix] scheduler and run a cycle.

       When running a cycle, we pull external actions at every job and perform them
       immediately. *)
    ; external_actions                            : (unit -> unit) Thread_safe_queue.t sexp_opaque
    ; mutable thread_safe_external_action_hook    : (unit -> unit)

    (* configuration*)
    ; mutable check_invariants                    : bool
    ; mutable max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t
    ; mutable record_backtraces                   : bool
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
      ~cycle_count:(check (fun cycle_count -> assert (cycle_count >= 0)))
      ~cycle_start:ignore
      ~run_every_cycle_start:ignore
      ~last_cycle_time:ignore
      ~last_cycle_num_jobs:(check (fun last_cycle_num_jobs ->
        assert (last_cycle_num_jobs >= 0)))
      ~events:(check (Timing_wheel.invariant Job.invariant))
      ~external_actions:ignore
      ~thread_safe_external_action_hook:ignore
      ~check_invariants:ignore
      ~max_num_jobs_per_priority_per_cycle:ignore
      ~record_backtraces:ignore
    ;
  with exn ->
    failwiths "Scheduler.invariant failed" (exn, t) <:sexp_of< exn * t >>
;;

let create () =
  let now = Time.now () in
  let jobs = Jobs.create () in
  let events =
    Timing_wheel.create
      ~config:Config.timing_wheel_config
      ~start:now
  in
  { check_access                        = None
  ; jobs
  ; main_execution_context              = Execution_context.main
  ; cycle_start                         = now
  ; cycle_count                         = 0
  ; run_every_cycle_start               = []
  ; last_cycle_time                     = sec 0.
  ; last_cycle_num_jobs                 = 0
  ; events
  ; external_actions                    = Thread_safe_queue.create ()
  ; thread_safe_external_action_hook    = ignore
  (* configuration*)
  ; check_invariants                    = Config.check_invariants
  ; max_num_jobs_per_priority_per_cycle = Config.max_num_jobs_per_priority_per_cycle
  ; record_backtraces                   = Config.record_backtraces;
  }
;;

let is_dead t = is_some (Jobs.uncaught_exn t.jobs)

let set_check_access t f = t.check_access <- f

let t_ref =
  match Result.try_with create with
  | Ok t -> ref t
  | Error exn ->
    Debug.log "Async cannot create its raw scheduler" exn <:sexp_of< exn >>;
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
  let execution_context = Jobs.current_execution_context t.jobs in
  if t.record_backtraces
  then Execution_context.record_backtrace execution_context
  else execution_context
;;

let set_execution_context t execution_context =
  Jobs.set_execution_context t.jobs execution_context
;;

let with_execution_context t tmp_context ~f =
  let old_context = current_execution_context t in
  set_execution_context t tmp_context;
  protect ~f ~finally:(fun () -> set_execution_context t old_context);
;;

let create_job t e f a = Jobs.create_job t.jobs e f a

let enqueue t execution_context f a = Jobs.enqueue t.jobs execution_context f a

let enqueue_job t job = Jobs.enqueue_job t.jobs job

let global_kill_index t = Jobs.global_kill_index t.jobs

let monitor_is_alive t monitor =
  Raw_monitor.is_alive monitor ~global_kill_index:(global_kill_index t)
;;

let kill_monitor t monitor =
  if Debug.monitor then Debug.log "kill_monitor" monitor <:sexp_of< Monitor.t >>;
  monitor.kill_index <- Kill_index.dead;
  Jobs.inc_global_kill_index t.jobs;
;;

let got_uncaught_exn t exn = Jobs.got_uncaught_exn t.jobs exn

let num_jobs_run t = Jobs.num_jobs_run t.jobs

let stabilize t =
  let jobs = t.jobs in
  Jobs.start_cycle jobs
    ~max_num_jobs_per_priority:(Max_num_jobs_per_priority_per_cycle.create_exn
                                  Int.max_value);
  Jobs.run_all jobs ~external_actions:t.external_actions;
;;
