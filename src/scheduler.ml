open Core_kernel.Std
open Import

module Deferred  = Deferred1
module Scheduler = Scheduler1
module Stream    = Async_stream

include (Scheduler : (module type of Scheduler
                       with module Time_source := Scheduler.Time_source))

let t = Scheduler.t

include struct
  open Scheduler
  let check_access              = check_access
  let check_access              = check_access
  let create_job                = create_job
  let current_execution_context = current_execution_context
  let enqueue                   = enqueue
  let enqueue_job               = enqueue_job
  let free_job                  = free_job
  let invariant                 = invariant
  let is_dead                   = is_dead
  let num_pending_jobs          = num_pending_jobs
  let num_jobs_run              = num_jobs_run
  let set_check_access          = set_check_access
  let set_check_access          = set_check_access
  let set_execution_context     = set_execution_context
  let uncaught_exn              = uncaught_exn
  let with_execution_context    = with_execution_context
end

include Monitor.Exported_for_scheduler

let find_local key =
  Execution_context.find_local (current_execution_context (t ())) key
;;

let with_local key value ~f =
  let t = t () in
  let execution_context =
    Execution_context.with_local (current_execution_context t) key value
  in
  with_execution_context t execution_context ~f
;;

let main_execution_context = (t ()).main_execution_context

let can_run_a_job t = num_pending_jobs t > 0 || Option.is_some t.yield_ivar

let has_upcoming_event t = not (Timing_wheel_ns.is_empty (events t))

let next_upcoming_event t = Timing_wheel_ns.next_alarm_fires_at (events t)

let next_upcoming_event_exn t = Timing_wheel_ns.next_alarm_fires_at_exn (events t)

let event_precision t = Timing_wheel_ns.alarm_precision (events t)

let cycle_start t = t.cycle_start

let run_every_cycle_start t ~f =
  t.run_every_cycle_start <- f :: t.run_every_cycle_start;
;;

let map_cycle_times t ~f =
  Stream.create (fun tail ->
    run_every_cycle_start t ~f:(fun () -> Tail.extend tail (f t.last_cycle_time)));
;;

let cycle_num_jobs t =
  Stream.create (fun tail ->
    run_every_cycle_start t ~f:(fun () -> Tail.extend tail t.last_cycle_num_jobs));
;;

let cycle_count t = t.cycle_count

let set_max_num_jobs_per_priority_per_cycle t int =
  t.max_num_jobs_per_priority_per_cycle <-
    Max_num_jobs_per_priority_per_cycle.create_exn int;
;;

let set_thread_safe_external_job_hook t f = t.thread_safe_external_job_hook <- f

let thread_safe_enqueue_external_job t execution_context f a =
  Thread_safe_queue.enqueue t.external_jobs (External_job.T (execution_context, f, a));
  t.thread_safe_external_job_hook ();
;;

let set_event_added_hook t f = t.event_added_hook <- Some f
let set_job_queued_hook  t f = t.job_queued_hook  <- Some f

let create_alarm t f =
  let execution_context = current_execution_context t in
  Gc.Expert.Alarm.create (fun () ->
    thread_safe_enqueue_external_job t execution_context f ());
;;

let add_finalizer t heap_block f =
  let execution_context = current_execution_context t in
  let finalizer heap_block =
    (* Here we can be in any thread, and may not be holding the async lock.  So, we can
       only do thread-safe things.

       By putting [heap_block] in [external_jobs], we are keeping it alive until the next
       time the async scheduler gets around to dequeueing it.  Calling
       [t.thread_safe_external_job_hook] ensures that will happen in short order.  Thus,
       we are not dramatically increasing the lifetime of [heap_block], since the OCaml
       runtime already resurrected [heap_block] so that we could refer to it here.  The
       OCaml runtime already removed the finalizer function when it noticed [heap_block]
       could be finalized, so there is no infinite loop in which we are causing the
       finalizer to run again.  Also, OCaml does not impose any requirement on finalizer
       functions that they need to dispose of the block, so it's fine that we keep
       [heap_block] around until later. *)
    if Debug.finalizers then Debug.log_string "enqueueing finalizer";
    thread_safe_enqueue_external_job t execution_context f heap_block;
  in
  if Debug.finalizers then Debug.log_string "adding finalizer";
  (* We use [Caml.Gc.finalise] instead of [Core_kernel.Std.Gc.add_finalizer] because the latter
     has its own wrapper around [Caml.Gc.finalise] to run finalizers synchronously. *)
  Caml.Gc.finalise finalizer heap_block;
;;

let add_finalizer_exn t x f =
  add_finalizer t (Heap_block.create_exn x)
    (fun heap_block -> f (Heap_block.value heap_block))
;;

(** [force_current_cycle_to_end] sets the number of normal jobs allowed to run in this
    cycle to zero.  Thus, after the currently running job completes, the scheduler will
    switch to low priority jobs and then end the current cycle. *)
let force_current_cycle_to_end t =
  Job_queue.set_jobs_left_this_cycle t.normal_priority_jobs 0
;;

let advance_clock t ~now = Time_source.advance t.time_source ~to_:now

let run_cycle t =
  if debug then Debug.log "run_cycle starting" t [%sexp_of: t];
  let now = Time_ns.now () in
  t.cycle_count <- t.cycle_count + 1;
  t.cycle_start <- now;
  begin match t.yield_ivar with
  | None -> ()
  | Some ivar -> Ivar.fill ivar (); t.yield_ivar <- None;
  end;
  let num_jobs_run_at_start_of_cycle = num_jobs_run t in
  List.iter t.run_every_cycle_start ~f:(fun f -> f ());
  advance_clock t ~now;
  start_cycle t ~max_num_jobs_per_priority:t.max_num_jobs_per_priority_per_cycle;
  let rec run_jobs () =
    match Scheduler.run_jobs t with
    | Ok () -> ()
    | Error (exn, backtrace) ->
      Monitor.send_exn (Monitor.current ()) exn ~backtrace:(`This backtrace);
      (* [run_jobs] stopped due to an exn.  There may still be jobs that could be run
         this cycle, so [run_jobs] again. *)
      run_jobs ()
  in
  run_jobs ();
  t.last_cycle_time <- Time_ns.diff (Time_ns.now ()) t.cycle_start;
  t.last_cycle_num_jobs <- num_jobs_run t - num_jobs_run_at_start_of_cycle;
  if debug
  then Debug.log "run_cycle finished"
         (uncaught_exn t, is_some (next_upcoming_event t))
         [%sexp_of: Error.t option * bool];
;;

let run_cycles_until_no_jobs_remain () =
  if debug then Debug.log_string "run_cycles_until_no_jobs_remain starting";
  let t = t () in
  if is_dead t
  then failwiths "run_cycles_until_no_jobs_remain cannot proceed -- scheduler is dead" t
         [%sexp_of: t];
  let rec loop () =
    run_cycle t;
    advance_clock t ~now:(Time_ns.now ());
    (* We [fire_past_alarms] just before checking if there are pending jobs, so that clock
       events that fire become jobs, and thus cause an additional [loop]. *)
    Time_source.fire_past_alarms t.time_source;
    if can_run_a_job t then loop ()
  in
  loop ();
  (* Reset the current execution context to maintain the invariant that when we're not in
     a job, [current_execution_context = main_execution_context]. *)
  set_execution_context t t.main_execution_context;
  if debug then Debug.log_string "run_cycles_until_no_jobs_remain finished";
  Option.iter (uncaught_exn t) ~f:Error.raise;
;;

let reset_in_forked_process () =
  if debug then Debug.log_string "reset_in_forked_process";
  (* There is no need to empty [main_monitor_hole]. *)
  Scheduler.(t_ref := create ());
;;

let check_invariants t = t.check_invariants

let set_check_invariants t b = t.check_invariants <- b

let set_record_backtraces t b = t.record_backtraces <- b

let yield t =
  let ivar =
    match t.yield_ivar with
    | Some ivar -> ivar
    | None ->
      let ivar = Ivar.create () in
      t.yield_ivar <- Some ivar;
      ivar
  in
  Ivar.read ivar
;;

let yield_every ~n =
  if n <= 0
  then failwiths "Scheduler.yield_every got nonpositive count" n [%sexp_of: int]
  else if n = 1
  then stage (fun t -> yield t)
  else
    let count_until_yield = ref n in
    stage (fun t ->
      decr count_until_yield;
      if !count_until_yield > 0
      then Deferred.unit
      else begin
        count_until_yield := n;
        yield t;
      end)
;;

let%test_module _ = (module struct

  (* [Monitor.catch_stream]. *)
  let%test_unit _ =
    let d = Stream.next (Monitor.catch_stream (fun () -> failwith "")) in
    run_cycles_until_no_jobs_remain ();
    assert (is_some (Deferred.peek d))
  ;;

  (* [Monitor.catch]. *)
  let%test_unit _ =
    let d = Monitor.catch (fun () -> failwith "") in
    run_cycles_until_no_jobs_remain ();
    assert (is_some (Deferred.peek d))
  ;;

end)
