open Core_kernel.Std
open Import    let _ = _squelch_unused_module_warning_

module Scheduler = Types.Scheduler

let debug = Debug.scheduler

type t = Scheduler.t =
  { (* [check_access] optionally holds a function to run to check whether access to [t] is
       currently allowed.  It is used to detect invalid access to the scheduler from a
       thread. *)
    mutable check_access                        : (unit -> unit) option
  ; mutable job_pool                            : Job_pool.t
  ; normal_priority_jobs                        : Job_queue.t
  ; low_priority_jobs                           : Job_queue.t
  ; mutable main_execution_context              : Execution_context.t
  ; mutable current_execution_context           : Execution_context.t
  ; mutable global_kill_index                   : Kill_index.t
  (* The scheduler calls [got_uncaught_exn] when an exception bubbles to the top of the
     monitor tree without being handled.  This function guarantees to never run another
     job after this by calling [clear] and because [enqueue_job] will never add another
     job. *)
  ; mutable uncaught_exn                        : Error.t option
  ; mutable cycle_count                         : int
  ; mutable cycle_start                         : Time_ns.t
  ; mutable run_every_cycle_start               : (unit -> unit) list
  ; mutable last_cycle_time                     : Time_ns.Span.t
  ; mutable last_cycle_num_jobs                 : int
  ; events                                      : Job.t Timing_wheel_ns.t
  (* [external_jobs] is a queue of actions sent from outside of async.  This is for the
     case where we want to schedule a job or fill an ivar from a context where it is not
     safe to run async code, because the async lock isn't held.  For instance: - in an
     OCaml finalizer, as they can run at any time in any thread.

     The way to do it is to queue a thunk in [external_jobs] and call
     [thread_safe_external_job_hook], which is responsible for notifying the scheduler
     that new actions are available.  [thread_safe_external_job_hook] is set in
     [Async_unix] to call [Interruptor.thread_safe_interrupt], which will wake up the
     [Async_unix] scheduler and run a cycle.

     When running a cycle, we pull external actions at every job and perform them
     immediately. *)
  ; external_jobs                               : External_job.t Thread_safe_queue.t
  ; mutable thread_safe_external_job_hook       : (unit -> unit)

  ; mutable yield_ivar                          : unit Types.Ivar.t sexp_opaque option

  (* configuration*)
  ; mutable check_invariants                    : bool
  ; mutable max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t
  ; mutable record_backtraces                   : bool
  }
with fields, sexp_of

let num_pending_jobs t =
  Job_queue.length t.normal_priority_jobs + Job_queue.length t.low_priority_jobs
;;

let num_jobs_run t =
  Job_queue.num_jobs_run t.normal_priority_jobs
  + Job_queue.num_jobs_run t.low_priority_jobs
;;

let invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~check_access:ignore
      ~job_pool:(check Job_pool.invariant)
      ~normal_priority_jobs:(check Job_queue.invariant)
      ~low_priority_jobs:   (check Job_queue.invariant)
      ~main_execution_context:(check Execution_context.invariant)
      ~current_execution_context:(check Execution_context.invariant)
      ~global_kill_index:(check (fun kill_index ->
        Kill_index.invariant kill_index;
        assert (not (Kill_index.equal kill_index Kill_index.dead))))
      ~uncaught_exn:(check (fun uncaught_exn ->
        if is_some uncaught_exn then assert (num_pending_jobs t = 0)))
      ~cycle_count:(check (fun cycle_count -> assert (cycle_count >= 0)))
      ~cycle_start:ignore
      ~run_every_cycle_start:ignore
      ~last_cycle_time:ignore
      ~last_cycle_num_jobs:(check (fun last_cycle_num_jobs ->
        assert (last_cycle_num_jobs >= 0)))
      ~events:(check (Timing_wheel_ns.invariant Job.invariant))
      ~external_jobs:ignore
      ~thread_safe_external_job_hook:ignore
      ~yield_ivar:ignore
      ~check_invariants:ignore
      ~max_num_jobs_per_priority_per_cycle:ignore
      ~record_backtraces:ignore
    ;
  with exn ->
    failwiths "Scheduler.invariant failed" (exn, t) <:sexp_of< exn * t >>
;;

let create () =
  let now = Time_ns.now () in
  let events =
    Timing_wheel_ns.create
      ~config:Config.timing_wheel_config
      ~start:now
  in
  let t =
    { check_access                        = None
    ; job_pool                            = Job_pool.create ()
    ; normal_priority_jobs                = Job_queue.create ()
    ; low_priority_jobs                   = Job_queue.create ()
    ; main_execution_context              = Execution_context.main
    ; current_execution_context           = Execution_context.main
    ; global_kill_index                   = Kill_index.initial
    ; uncaught_exn                        = None
    ; cycle_start                         = now
    ; cycle_count                         = 0
    ; run_every_cycle_start               = []
    ; last_cycle_time                     = sec 0.
    ; last_cycle_num_jobs                 = 0
    ; events
    ; external_jobs                       = Thread_safe_queue.create ()
    ; thread_safe_external_job_hook       = ignore
    ; yield_ivar                          = None
    (* configuration*)
    ; check_invariants                    = Config.check_invariants
    ; max_num_jobs_per_priority_per_cycle = Config.max_num_jobs_per_priority_per_cycle
    ; record_backtraces                   = Config.record_backtraces;
    }
  in
  t
;;

let is_dead t = is_some t.uncaught_exn

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
  if t.record_backtraces
  then Execution_context.record_backtrace t.current_execution_context
  else t.current_execution_context
;;

let set_execution_context t execution_context =
  Scheduler.set_execution_context t execution_context;
;;

let with_execution_context t tmp_context ~f =
  let old_context = current_execution_context t in
  set_execution_context t tmp_context;
  protect ~f ~finally:(fun () -> set_execution_context t old_context);
;;

let create_job (type a) t execution_context f a =
  if Pool.is_full t.job_pool then t.job_pool <- Pool.grow t.job_pool;
  Pool.new3 t.job_pool execution_context
    (Obj.magic (f : a -> unit) : Obj.t -> unit)
    (Obj.repr (a : a));
;;

let free_job t job = Pool.free t.job_pool job

let enqueue t (execution_context : Execution_context.t) f a =
  (* If there's been an uncaught exn, we don't add the job, since we don't want any jobs
     to run once there's been an uncaught exn. *)
  if is_none t.uncaught_exn then begin
    let job_queue =
      match execution_context.priority with
      | Normal -> t.normal_priority_jobs
      | Low    -> t.low_priority_jobs
    in
    Job_queue.enqueue job_queue execution_context f a
  end;
;;

let enqueue_job t job ~free_job =
  let job_pool = t.job_pool in
  enqueue t
    (Pool.get job_pool job Pool.Slot.t0)
    (Pool.get job_pool job Pool.Slot.t1)
    (Pool.get job_pool job Pool.Slot.t2);
  if free_job then Pool.free t.job_pool job;
;;

let monitor_is_alive t monitor =
  Raw_monitor.is_alive monitor ~global_kill_index:(global_kill_index t)
;;

let kill_monitor t monitor =
  if Debug.monitor then Debug.log "kill_monitor" monitor <:sexp_of< Raw_monitor.t >>;
  monitor.kill_index <- Kill_index.dead;
  t.global_kill_index <- Kill_index.next t.global_kill_index;
;;

let got_uncaught_exn t error =
  if debug then Debug.log "got_uncaught_exn" error <:sexp_of< Error.t >>;
  List.iter [ t.normal_priority_jobs ; t. low_priority_jobs ] ~f:Job_queue.clear;
  t.uncaught_exn <- Some error;
;;

(** [start_cycle t ~max_num_jobs_per_priority] enables subsequent calls of [run_jobs]
    to run up to [max_num_jobs_per_priority] jobs of each priority level. *)
let start_cycle t ~max_num_jobs_per_priority =
  let n = Max_num_jobs_per_priority_per_cycle.raw max_num_jobs_per_priority in
  Job_queue.set_jobs_left_this_cycle t.normal_priority_jobs n;
  Job_queue.set_jobs_left_this_cycle t.low_priority_jobs    n;
;;

(** [run_jobs t] removes jobs from [t] one at a time and runs them, stopping as soon
    as an unhandled exception is raised, or when no more jobs can be run at any priority,
    as per [~max_num_jobs_per_priority]. *)
let rec run_jobs t =
  match Job_queue.run_jobs t.normal_priority_jobs t with
  | Error _ as e -> e
  | Ok () ->
    match Job_queue.run_jobs t.low_priority_jobs t with
    | Error _ as e -> e
    | Ok () ->
      if Job_queue.can_run_a_job t.normal_priority_jobs
         || Job_queue.can_run_a_job t.low_priority_jobs
      then run_jobs t
      else Result.ok_unit
;;

let stabilize t =
  start_cycle t
    ~max_num_jobs_per_priority:(Max_num_jobs_per_priority_per_cycle.create_exn
                                  Int.max_value);
  run_jobs t;
;;
