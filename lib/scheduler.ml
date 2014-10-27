open Core.Std
open Import    let _ = _squelch_unused_module_warning_
open Raw_scheduler.T

module Stream = Async_stream

let debug = Debug.scheduler

type t = Raw_scheduler.t with sexp_of

let t = Raw_scheduler.t

include struct
  open Raw_scheduler
  let check_access              = check_access
  let check_access              = check_access
  let create_job                = create_job
  let current_execution_context = current_execution_context
  let enqueue                   = enqueue
  let enqueue_job               = enqueue_job
  let invariant                 = invariant
  let is_dead                   = is_dead
  let num_jobs_run              = num_jobs_run
  let set_check_access          = set_check_access
  let set_check_access          = set_check_access
  let set_execution_context     = set_execution_context
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

let uncaught_exn t = Jobs.uncaught_exn t.jobs

let main_execution_context = (t ()).main_execution_context

let num_pending_jobs t = Jobs.length t.jobs

let next_upcoming_event t = Timing_wheel.next_alarm_fires_at t.events

let cycle_start t = t.cycle_start

let run_every_cycle_start t ~f =
  t.run_every_cycle_start <- f :: t.run_every_cycle_start;
;;

let cycle_times t =
  Stream.create (fun tail ->
    run_every_cycle_start t ~f:(fun () -> Tail.extend tail t.last_cycle_time));
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

let set_thread_safe_external_action_hook t f = t.thread_safe_external_action_hook <- f

let thread_safe_enqueue_external_action t f =
  Thread_safe_queue.enqueue t.external_actions f;
  t.thread_safe_external_action_hook ();
;;

let add_finalizer t heap_block f =
  let execution_context = current_execution_context t in
  let finalizer heap_block =
    (* Here we can be in any thread, and may not be holding the async lock.  So, we
       can only do thread-safe things.

       By putting [heap_block] in [external_actions], we are keeping it alive until the
       next time the async scheduler gets around to dequeueing it.  Calling
       [t.thread_safe_external_action_hook] ensures that will happen in short order.
       Thus, we are not dramatically increasing the lifetime of [heap_block], since the
       OCaml runtime already resurrected [heap_block] so that we could refer to it here.
       The OCaml runtime already removed the finalizer function when it noticed
       [heap_block] could be finalized, so there is no infinite loop in which we are
       causing the finalizer to run again.  Also, OCaml does not impose any requirement on
       finalizer functions that they need to dispose of the block, so it's fine that we
       keep [heap_block] around until later. *)
    if Debug.finalizers then Debug.log_string "enqueueing finalizer";
    thread_safe_enqueue_external_action t (fun () ->
      Raw_scheduler.enqueue t execution_context f heap_block);
  in
  if Debug.finalizers then Debug.log_string "adding finalizer";
  (* We use [Caml.Gc.finalise] instead of [Core.Std.Gc.add_finalizer] because the latter
     has its own wrapper around [Caml.Gc.finalise] to run finalizers synchronously. *)
  Caml.Gc.finalise finalizer heap_block;
;;

let add_finalizer_exn t x f =
  add_finalizer t (Heap_block.create_exn x)
    (fun heap_block -> f (Heap_block.value heap_block))
;;

let force_current_cycle_to_end t = Jobs.force_current_cycle_to_end t.jobs

let run_cycle t =
  if debug then Debug.log "run_cycle starting" t <:sexp_of< t >>;
  let now = Time.now () in
  t.cycle_count <- t.cycle_count + 1;
  t.cycle_start <- now;
  let num_jobs_run_at_start_of_cycle = num_jobs_run t in
  List.iter t.run_every_cycle_start ~f:(fun f -> f ());
  Timing_wheel.advance_clock t.events ~to_:now ~handle_fired:(fun alarm ->
    enqueue_job t (Timing_wheel.Alarm.value t.events alarm) ~free_job:true);
  Jobs.start_cycle t.jobs
    ~max_num_jobs_per_priority:t.max_num_jobs_per_priority_per_cycle;
  let rec run_jobs () =
    match Jobs.run_all t.jobs ~external_actions:t.external_actions with
    | Ok () -> ()
    | Error exn ->
      Monitor.send_exn (Monitor.current ()) exn ~backtrace:`Get;
      (* [run_all] stopped due to an exn.  There may still be jobs that could be run
         this cycle, so [run_jobs] again. *)
      run_jobs ()
  in
  run_jobs ();
  t.last_cycle_time <- Time.diff (Time.now ()) t.cycle_start;
  t.last_cycle_num_jobs <- num_jobs_run t - num_jobs_run_at_start_of_cycle;
  if debug
  then Debug.log "run_cycle finished"
         (uncaught_exn t, is_some (Timing_wheel.next_alarm_fires_at t.events))
         <:sexp_of< Error.t option * bool >>;
;;

let run_cycles_until_no_jobs_remain () =
  if debug then Debug.log_string "run_cycles_until_no_jobs_remain starting";
  let t = t () in
  if is_dead t
  then failwiths "run_cycles_until_no_jobs_remain cannot proceed -- scheduler is dead" t
         <:sexp_of< t >>;
  let rec loop () =
    run_cycle t;
    if not (Jobs.is_empty t.jobs) then loop ();
  in
  loop ();
  (* Reset the current execution context to maintain the invariant that when we're not in
     a job, [current_execution_context = main_execution_context]. *)
  set_execution_context t t.main_execution_context;
  if debug then Debug.log_string "run_cycles_until_no_jobs_remain finished";
  Option.iter (Jobs.uncaught_exn t.jobs) ~f:Error.raise;
;;

let reset_in_forked_process () =
  if debug then Debug.log_string "reset_in_forked_process";
  (* There is no need to empty [main_monitor_hole]. *)
  Raw_scheduler.(t_ref := create ());
;;

let check_invariants t = t.check_invariants

let set_check_invariants t b = t.check_invariants <- b

let set_record_backtraces t b = t.record_backtraces <- b

TEST_MODULE = struct
  (* [Monitor.kill] *)
  TEST_UNIT =
    let m = Monitor.create ~name:"m" () in
    assert (Monitor.is_alive m);
    Monitor.kill m;
    assert (not (Monitor.is_alive m));
    assert Monitor.(is_alive main);
    let r = ref true in
    schedule ~monitor:m (fun () -> r := false);
    run_cycles_until_no_jobs_remain ();
    assert !r;
    assert Monitor.(is_alive main);
  ;;

  (* [Monitor.kill] -- killing parent also kills child. *)
  TEST_UNIT =
    let m = Monitor.create ~name:"parent" () in
    let r = ref true in
    schedule ~monitor:m (fun () ->
      schedule ~monitor:(Monitor.create ~name:"child" ()) (fun () -> r := false);
      Monitor.kill m);
    run_cycles_until_no_jobs_remain ();
    assert !r;
    assert Monitor.(is_alive main);
  ;;

  (* [Monitor.kill] -- killing child does not kill parent. *)
  TEST_UNIT =
    let m = Monitor.create ~name:"parent 2" () in
    let r = ref false in
    let r' = ref true in
    let m' =
      Option.value_exn (within_v ~monitor:m (fun () -> Monitor.create ~name:"child" ()))
    in
    Monitor.kill m';
    schedule ~monitor:m  (fun () -> r  := true );
    schedule ~monitor:m' (fun () -> r' := false);
    run_cycles_until_no_jobs_remain ();
    assert (Monitor.is_alive m);
    assert (not (Monitor.is_alive m'));
    assert Monitor.(is_alive main);
    assert !r ;
    assert !r';
  ;;

  (* [Monitor.catch_stream]. *)
  TEST_UNIT =
    let d = Stream.next (Monitor.catch_stream (fun () -> failwith "")) in
    run_cycles_until_no_jobs_remain ();
    assert (is_some (Deferred.peek d));
  ;;

  (* [Monitor.catch]. *)
  TEST_UNIT =
    let d = Monitor.catch (fun () -> failwith "") in
    run_cycles_until_no_jobs_remain ();
    assert (is_some (Deferred.peek d));
  ;;

end
