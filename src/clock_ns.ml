open Core_kernel.Std
open Import
open Deferred_std

let run_cycles_until_no_jobs_remain = Scheduler.run_cycles_until_no_jobs_remain

module Alarm = Timing_wheel_ns.Alarm
module Scheduler = Scheduler1

let debug = Debug.clock

(* We use [Time_ns.now ()] rather than [cycle_start] for the base time.  There can be
   substantial difference between the two when people do long running computations or
   mix blocking code with async.  And humans expect that [after] is based on the
   current time, not some artifact of async implementation. *)
let span_to_time span = Time_ns.add (Time_ns.now ()) span

let run_at_internal (scheduler : Scheduler.t) time f a =
  let events = scheduler.events in
  let execution_context = Scheduler.current_execution_context scheduler in
  if Time_ns.( > ) time (Timing_wheel_ns.now events)
  then Scheduler.schedule_job scheduler ~at:time execution_context f a
  else begin
    Scheduler.enqueue scheduler execution_context f a;
    Alarm.null ();
  end
;;

let run_at time f a =
  ignore (run_at_internal (Scheduler.t ()) time f a : _ Alarm.t);
;;

let run_after span f a = run_at (span_to_time span) f a

let at =
  let fill result = Ivar.fill result () in
  fun time ->
    let scheduler = Scheduler.t () in
    if Time_ns.( <= ) time (Timing_wheel_ns.now scheduler.events)
    then Deferred.unit
    else
      let result = Ivar.create () in
      ignore (run_at_internal scheduler time fill result : _ Alarm.t);
      Ivar.read result;
;;

let after span = at (span_to_time span)

let remove_alarm_if_scheduled scheduler alarm =
  let events = Scheduler.events scheduler in
  if Timing_wheel_ns.mem events alarm then begin
    Scheduler.free_job scheduler (Alarm.value events alarm);
    Timing_wheel_ns.remove events alarm;
  end
;;

module Event = struct
  type ('a, 'h) t =
    { alarm                : Job.t Alarm.t
    (* [scheduled_at] is the time at which [t] has most recently been scheduled to fire.
       While [t.alarm] is still in the timing wheel, this is the same as [Alarm.at
       t.alarm]. *)
    ; mutable scheduled_at : Time_ns.t
    (* As long as [Ivar.is_empty fired], we have not yet committed to whether the event
       will happen or be aborted.  When [Ivar.is_empty fired], the alarm may or may not be
       in the timing wheel -- if it isn't, then there's a job in Async's job queue that
       will fire the event, unless it is aborted before that job can run. *)
    ; fired                : [ `Aborted of 'a | `Happened of 'h ] Ivar.t
    }
  [@@deriving fields, sexp_of]

  type t_unit = (unit, unit) t [@@deriving sexp_of]

  let fired t = Ivar.read t.fired

  let invariant invariant_a invariant_h t =
    Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
      let events = Scheduler.(events (t ())) in
      let check f = Invariant.check_field t f in
      Fields.iter
        ~alarm:(check (fun alarm ->
          if Ivar.is_full t.fired
          then assert (not (Timing_wheel_ns.mem events alarm))))
        ~scheduled_at:(check (fun scheduled_at ->
          if Timing_wheel_ns.mem events t.alarm
          then [%test_result: Time_ns.t] scheduled_at
                 ~expect:(Alarm.at events t.alarm)))
        ~fired:(check (fun fired ->
          match Deferred.peek (Ivar.read fired) with
          | None -> ()
          | Some (`Aborted a)  -> invariant_a a
          | Some (`Happened h) -> invariant_h h)))
  ;;

  let status t =
    match Deferred.peek (Ivar.read t.fired) with
    | None -> `Scheduled_at t.scheduled_at
    | Some x -> (x :> [ `Happened of _ | `Aborted of _ | `Scheduled_at of Time_ns.t ])
  ;;

  let abort t a =
    if debug then Debug.log "Clock_ns.Event.abort" t [%sexp_of: (_, _) t];
    match Deferred.peek (fired t) with
    | Some (`Aborted  a) -> `Previously_aborted  a
    | Some (`Happened h) -> `Previously_happened h
    | None ->
      Ivar.fill t.fired (`Aborted a);
      remove_alarm_if_scheduled (Scheduler.t ()) t.alarm;
      `Ok
  ;;

  let abort_exn t a =
    match abort t a with
    | `Ok -> ()
    | `Previously_happened _ ->
      failwith "Clock.Event.abort_exn failed to abort event that previously happened"
    | `Previously_aborted _ ->
      failwith "Clock.Event.abort_exn failed to abort event that previously aborted"
  ;;

  let abort_if_possible t a =
    ignore (abort t a : [ `Ok | `Previously_aborted of _ | `Previously_happened of _ ]);
  ;;

  let reschedule_at t at =
    if debug
    then Debug.log "Clock_ns.Event.reschedule_at" (t, at)
           [%sexp_of: (_, _) t * Time_ns.t];
    match Deferred.peek (fired t) with
    | Some (`Aborted  a) -> `Previously_aborted  a
    | Some (`Happened h) -> `Previously_happened h
    | None ->
      let scheduler = Scheduler.t () in
      let events = Scheduler.events scheduler in
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
            Scheduler.enqueue_job scheduler ~free_job:true (Alarm.value events t.alarm);
            Timing_wheel_ns.remove events t.alarm;
          end;
        `Ok
      end;
  ;;

  let reschedule_after t span = reschedule_at t (span_to_time span)

  let run_at scheduled_at f z =
    if debug
    then Debug.log "Clock_ns.Event.run_at" scheduled_at [%sexp_of: Time_ns.t];
    let scheduler = Scheduler.t () in
    let fired = Ivar.create () in
    let fire z =
      (* [fire] runs in an Async job.  The event may have been aborted after the job
         was enqueued, so [fire] must check [fired]. *)
      if Ivar.is_empty fired
      then begin
        let result = f z in
        (* [f z] may have aborted the event, so we must check [fired] again. *)
        if Ivar.is_empty fired then Ivar.fill fired (`Happened result);
      end
    in
    let alarm = run_at_internal scheduler scheduled_at fire z in
    { alarm; scheduled_at; fired }
  ;;

  let at        time     = run_at time ignore ()
  let run_after span f a = run_at (span_to_time span) f a
  let after     span     =     at (span_to_time span)
end

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

module Continue = struct
  type t =
    | Immediately
    | After         of Time_ns.Span.t
    | Next_multiple of Time_ns.t * Time_ns.Span.t

  let at t events =
    match t with
    | Immediately-> Timing_wheel_ns.now events
    | After span -> span_to_time span
    | Next_multiple (base, interval) ->
      Time_ns.next_multiple ~base ~after:(Time_ns.now ()) ~interval ()
  ;;
end

let run_repeatedly
      ?(start = Deferred.unit)
      ?stop
      ?(continue_on_error = true)
      ~f ~continue () =
  start
  >>> fun () ->
  let scheduler = Scheduler.t () in
  let events = Scheduler.events scheduler in
  let alarm = ref (Alarm.null ()) in
  let stop =
    match stop with
    | None -> Deferred.never ()
    | Some stop ->
      upon stop (fun () -> remove_alarm_if_scheduled scheduler !alarm);
      stop
  in
  (* [run_f], [continue_f], and [continue_try_with] are defined so that we allocate their
     closures once, not once per iteration. *)
  let rec run_f () =
    (* Before calling [f], we synchronously check whether [stop] is determined. *)
    if not (Deferred.is_determined stop)
    then
      if continue_on_error
      then Monitor.try_with f ~run:`Now ~rest:`Raise >>> continue_try_with
      else
        let d = f () in
        if Deferred.is_determined d
        then continue_f ()
        else d >>> continue_f
  and continue_f () =
    if not (Deferred.is_determined stop)
    then alarm := run_at_internal scheduler (Continue.at continue events) run_f ()
  and continue_try_with or_error =
    begin match or_error with
    | Ok () -> ()
    | Error error -> Monitor.send_exn (Monitor.current ()) error;
    end;
    continue_f ()
  in
  run_f ()
;;

let every' ?start ?stop ?continue_on_error span f =
  if Time_ns.Span.( <= ) span Time_ns.Span.zero
  then failwiths "Clock_ns.every got nonpositive span" span [%sexp_of: Time_ns.Span.t];
  run_repeatedly ?start ?stop ?continue_on_error ~f ~continue:(After span) ()
;;

let every ?start ?stop ?continue_on_error span f =
  every' ?start ?stop ?continue_on_error span (fun () -> f (); Deferred.unit)
;;

let%bench_module "Clock.every" = (module struct

  let%bench "~continue-on-error:false" =
    let iv = Ivar.create () in
    let n = ref 0 in
    run_repeatedly
      ~stop:(Ivar.read iv)
      ~continue_on_error:false
      ~f:(fun () ->
        if !n >= 1_000
        then Ivar.fill iv ()
        else incr n;
        Deferred.unit)
      ~continue:Immediately
      ();
    run_cycles_until_no_jobs_remain ();
  ;;

  let%bench "~continue_on_error:true" =
    let iv = Ivar.create () in
    let n = ref 0 in
    run_repeatedly
      ~stop:(Ivar.read iv)
      ~continue_on_error:true
      ~f:(fun () ->
        if !n >= 1_000
        then Ivar.fill iv ()
        else incr n;
        Deferred.unit)
      ~continue:Immediately
      ();
    run_cycles_until_no_jobs_remain ();
  ;;
end)

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
    ~continue:(Next_multiple (base, interval)) ()
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
        begin match Event.abort timeout () with
        (* [`Previously_happened] can occur if both [d] and [wait] become determined at
           the same time, e.g. [with_timeout (sec 0.) Deferred.unit]. *)
        | `Ok | `Previously_happened () -> ()
        | `Previously_aborted () ->
          failwith "Clock_ns.with_timeout bug: should only abort once"
        end;
        `Result v)
    ; choice (Event.fired timeout) (function
        | `Happened () -> `Timeout
        | `Aborted  () ->
          failwith "Clock_ns.with_timeout bug: both completed and timed out")
    ]
;;

let%test_unit "with_timeout doesn't clutter the async timing wheel" =
  let timing_wheel_length () = Timing_wheel_ns.length (Scheduler.t ()).events in
  let length_before = timing_wheel_length () in
  don't_wait_for (Deferred.ignore (with_timeout Time_ns.Span.day Deferred.unit));
  run_cycles_until_no_jobs_remain ();
  assert (timing_wheel_length () <= length_before)
;;
