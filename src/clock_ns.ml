open Core_kernel.Std
open Import
open Deferred_std

module Stream = Async_stream

let debug = Debug.clock

(* We use [Time_ns.now ()] rather than [cycle_start] for the base time.  There can be
   substantial difference between the two when people do long running computations or
   mix blocking code with async.  And humans expect that [after] is based on the
   current time, not some artifact of async implementation. *)
let span_to_time span = Time_ns.add (Time_ns.now ()) span

let run_at_internal (scheduler : Scheduler0.t) time f a =
  let events = scheduler.events in
  let execution_context = Scheduler0.current_execution_context scheduler in
  if Time_ns.( > ) time (Timing_wheel_ns.now events)
  then Scheduler0.schedule_job scheduler ~at:time execution_context f a
  else begin
    Scheduler0.enqueue scheduler execution_context f a;
    Timing_wheel_ns.Alarm.null ();
  end
;;

let run_at time f a =
  ignore (run_at_internal (Scheduler0.t ()) time f a : _ Timing_wheel_ns.Alarm.t);
;;

let run_after span f a = run_at (span_to_time span) f a

let at =
  let fill result = Ivar.fill result () in
  fun time ->
    let scheduler = Scheduler0.t () in
    if Time_ns.( <= ) time (Timing_wheel_ns.now scheduler.events)
    then Deferred.unit
    else
      let result = Ivar.create () in
      ignore (run_at_internal scheduler time fill result : _ Timing_wheel_ns.Alarm.t);
      Ivar.read result;
;;

let after span = at (span_to_time span)

module Event = struct
  type t =
    { alarm                : Job.t Timing_wheel_ns.Alarm.t
    (* [scheduled_at] is the time at which [t] has most recently been scheduled to fire.
       While [t.alarm] is still in the timing wheel, this is the same as [Alarm.at
       t.alarm]. *)
    ; mutable scheduled_at : Time_ns.t
    (* As long as [Ivar.is_empty fired], we have not yet committed to whether the event
       will happen or be aborted.  When [Ivar.is_empty fired], the alarm may or may not be
       in the timing wheel -- if it isn't, then there's a job in Async's job queue that
       will fire the event, unless it is aborted before that job can run. *)
    ; fired                : [ `Happened | `Aborted ] Ivar.t
    }
  with fields, sexp_of

  let fired t = Ivar.read t.fired

  let invariant t =
    Invariant.invariant _here_ t <:sexp_of< t >> (fun () ->
      let events = Scheduler0.(events (t ())) in
      let check f = Invariant.check_field t f in
      Fields.iter
        ~alarm:(check (fun alarm ->
          if Ivar.is_full t.fired
          then assert (not (Timing_wheel_ns.mem events alarm))))
        ~scheduled_at:(check (fun scheduled_at ->
          if Timing_wheel_ns.mem events t.alarm
          then <:test_result< Time_ns.t >> scheduled_at
                 ~expect:(Timing_wheel_ns.Alarm.at events t.alarm)))
        ~fired:ignore)
  ;;

  let status t =
    match Deferred.peek (Ivar.read t.fired) with
    | None -> `Scheduled_at t.scheduled_at
    | Some x -> (x :> [ `Happened | `Aborted | `Scheduled_at of Time_ns.t ])
  ;;

  let abort t =
    if debug then Debug.log "Clock_ns.Event.abort" t <:sexp_of< t >>;
    match Deferred.peek (fired t) with
    | Some `Aborted  -> `Previously_aborted
    | Some `Happened -> `Previously_happened
    | None ->
      Ivar.fill t.fired `Aborted;
      let scheduler = Scheduler0.t () in
      let events = scheduler.events in
      (* [t.alarm] might have been removed from [events] due to [advance_clock], even
         though the resulting [fire] job hasn't run yet.  So, we have to check before
         removing it. *)
      if Timing_wheel_ns.mem events t.alarm then begin
        Scheduler0.free_job scheduler (Timing_wheel_ns.Alarm.value events t.alarm);
        Timing_wheel_ns.remove events t.alarm;
      end;
      `Ok
  ;;

  let reschedule_at t at =
    if debug
    then Debug.log "Clock_ns.Event.reschedule_at" (t, at) <:sexp_of< t * Time_ns.t >>;
    match Deferred.peek (fired t) with
    | Some `Aborted  -> `Previously_aborted
    | Some `Happened -> `Previously_happened
    | None ->
      let scheduler = Scheduler0.t () in
      let events = Scheduler0.events scheduler in
      let is_in_timing_wheel = Timing_wheel_ns.mem events t.alarm in
      let am_trying_to_reschedule_in_the_future =
        Time_ns.( > ) at (Timing_wheel_ns.now events)
      in
      if am_trying_to_reschedule_in_the_future && not is_in_timing_wheel
      then `Too_late_to_reschedule
      else begin
        t.scheduled_at <- at;
        if is_in_timing_wheel
        then
          if am_trying_to_reschedule_in_the_future
          then Timing_wheel_ns.reschedule events t.alarm ~at
          else begin
            Scheduler0.enqueue_job scheduler ~free_job:true
              (Timing_wheel_ns.Alarm.value events t.alarm);
            Timing_wheel_ns.remove events t.alarm;
          end;
        `Ok
      end;
  ;;

  let reschedule_after t span = reschedule_at t (span_to_time span)

  let run_at scheduled_at f a =
    if debug
    then Debug.log "Clock_ns.Event.run_at" scheduled_at <:sexp_of< Time_ns.t >>;
    let scheduler = Scheduler0.t () in
    let fired = Ivar.create () in
    let fire a =
      (* [fire] runs in an Async job.  The event may have been aborted after the job
         was enqueued, so [fire] must check [fired]. *)
      if Ivar.is_empty fired then begin
        Ivar.fill fired `Happened;
        f a
      end
    in
    let alarm = run_at_internal scheduler scheduled_at fire a in
    { alarm; scheduled_at; fired }
  ;;

  let at        time     = run_at time ignore ()
  let run_after span f a = run_at (span_to_time span) f a
  let after     span     =     at (span_to_time span)
end

open Monitor.Exported_for_scheduler

let at_times ?(stop = Deferred.never ()) next_time =
  let tail = Tail.create () in
  let rec loop () =
    choose [ choice stop                (fun () -> `Stop)
           ; choice (at (next_time ())) (fun () -> `Tick)
           ]
    >>> function
    | `Stop -> Tail.close_exn tail
    | `Tick -> Tail.extend tail (); loop ()
  in
  loop ();
  Tail.collect tail;
;;

let at_varying_intervals ?stop compute_span =
  at_times ?stop (fun () -> Time_ns.add (Time_ns.now ()) (compute_span ()))
;;

let at_intervals ?(start = Time_ns.now ()) ?stop interval =
  at_times ?stop (fun () ->
    Time_ns.next_multiple ~base:start ~after:(Time_ns.now ()) ~interval ())
;;

let run_repeatedly
      ?(start = Deferred.unit)
      ?(stop = Deferred.never ())
      ?(continue_on_error = true)
      ~f ~continue () =
  start
  >>> fun () ->
  (* We use an extra monitor so we can specially handle errors in [f]. *)
  let saw_error = ref false in
  let monitor = Monitor.create ~name:"Clock_ns.run_repeatedly" () in
  Stream.iter (Monitor.detach_and_get_error_stream monitor) ~f:(fun e ->
    Monitor.send_exn (Monitor.current ()) e;
    saw_error := true);
  let rec loop wait =
    upon (choose [ choice stop (fun () -> `Stop)
                 ; choice wait (fun () -> `Continue)
                 ])
      (function
        | `Stop -> ()
        | `Continue ->
          (* The "raise_rest" part of a prior call to [try_with ~rest:`Raise f] could
             have raised an error after having returned ok.  We check [saw_error] and
             don't proceed if it did. *)
          if continue_on_error || not !saw_error then begin
            within' ~monitor (fun () ->
              Monitor.try_with ~rest:`Raise
                (fun () ->
                   (* We check at the last possible moment before running [f] whether
                      [stop] is determined, and if so, abort the loop. *)
                   if Deferred.is_determined stop
                   then return `Stop
                   else f () >>| fun () -> `Ran))
            >>> function
            | Ok z ->
              begin match z with
              | `Stop -> ()
              | `Ran -> loop (continue ())
              end
            | Error error ->
              Monitor.send_exn (Monitor.current ()) error;
              if continue_on_error then loop (continue ())
          end)
  in
  loop Deferred.unit
;;

let every' ?start ?stop ?continue_on_error span f =
  if Time_ns.Span.( <= ) span Time_ns.Span.zero
  then failwiths "Clock_ns.every got nonpositive span" span <:sexp_of< Time_ns.Span.t >>;
  run_repeatedly ?start ?stop ?continue_on_error ~f
    ~continue:(fun () -> after span) ()
;;

let every ?start ?stop ?continue_on_error span f =
  every' ?start ?stop ?continue_on_error span (fun () -> f (); Deferred.unit)
;;

let run_at_intervals' ?start ?stop ?continue_on_error interval f =
  let now = Time_ns.now () in
  let base, start =
    match start with
    | None       -> now, None
    | Some start ->
      start,
      Some (at (Time_ns.next_multiple ()
                  ~base:start
                  ~after:now
                  ~can_equal_after:true
                  ~interval))
  in
  run_repeatedly ?start ?stop ?continue_on_error ~f
    ~continue:(fun () ->
      at (Time_ns.next_multiple ~base ~after:(Time_ns.now ()) ~interval ()))
    ()
;;

let run_at_intervals ?start ?stop ?continue_on_error interval f =
  run_at_intervals' ?start ?stop ?continue_on_error interval
    (fun () -> f (); Deferred.unit)
;;

let with_timeout span d =
  let timeout = Event.after span in
  choose
    (* [choose] is supposed to call at most one choice function one time.  So, the bug
       messages below, should they raise, likely indicate a bug in [choose] rather than
       [with_timeout]. *)
    [ choice d (fun v ->
        begin match Event.abort timeout with
        (* [`Previously_happened] can occur if both [d] and [wait] become determined at
           the same time, e.g. [with_timeout (sec 0.) Deferred.unit]. *)
        | `Ok | `Previously_happened -> ()
        | `Previously_aborted ->
          failwith "Clock_ns.with_timeout bug: should only abort once"
        end;
        `Result v)
    ; choice (Event.fired timeout) (function
        | `Happened -> `Timeout
        | `Aborted -> failwith "Clock_ns.with_timeout bug: both completed and timed out")
    ]
;;

TEST_UNIT "with_timeout doesn't clutter the async timing wheel" =
  let timing_wheel_length () = Timing_wheel_ns.length (Scheduler0.t ()).events in
  let length_before = timing_wheel_length () in
  don't_wait_for (Deferred.ignore (with_timeout Time_ns.Span.day Deferred.unit));
  Scheduler.run_cycles_until_no_jobs_remain ();
  assert (timing_wheel_length () <= length_before);
;;
