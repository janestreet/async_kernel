open Core_kernel.Std
open Import

let debug = Debug.clock

module Alarm = Timing_wheel_ns.Alarm

module Deferred = Deferred1
module Scheduler = Scheduler1

let upon   = Deferred.upon
let choose = Deferred.choose
let choice = Deferred.choice

let ( >>> ) = upon

module Time_source = Time_source0

include (Time_source : (module type of struct include Time_source end
                         with module Scheduler := Time_source.Scheduler))

open Types.Time_source (* for the fields *)

let read_only (t : [> read] T1.t) = (t :> t)

let create ?(timing_wheel_config = Config.timing_wheel_config) ~now () =
  let scheduler = Scheduler.t () in
  let rec t =
    { events        = Timing_wheel_ns.create ~config:timing_wheel_config ~start:now
    ; handle_fired  = (fun alarm -> Scheduler.handle_fired t alarm)
    ; is_wall_clock = false
    ; scheduler
    }
  in
  t
;;

let wall_clock () = read_only (Scheduler.t ()).time_source

let alarm_precision t = Timing_wheel_ns.alarm_precision t.events

let now t =
  if t.is_wall_clock
  then
    (* For the wall-clock time-source, we use [Time_ns.now ()] rather than
       [Timing_wheel_ns.now t.events].  The latter is only updated at the start of each
       cycle.  There can be substantial difference between the two when people do long
       running computations or mix blocking code with async.  And humans expect that
       wall-clock time is based on [Time.now], not some artifact of async
       implementation. *)
    Time_ns.now ()
  else
    Timing_wheel_ns.now t.events
;;

let advance t ~to_ =
  Timing_wheel_ns.advance_clock t.events ~to_ ~handle_fired:t.handle_fired;
;;

let advance_by t by = advance t ~to_:(Time_ns.add (now t) by)

let fire_past_alarms t =
  Timing_wheel_ns.fire_past_alarms t.events ~handle_fired:t.handle_fired;
;;

let span_to_time t span = Time_ns.add (now t) span

let schedule_job t ~at execution_context f a =
  let alarm =
    Timing_wheel_ns.add t.events ~at
      (Scheduler.create_job t.scheduler execution_context f a)
  in
  begin match t.scheduler.event_added_hook with
  | None -> ()
  | Some f -> f at
  end;
  alarm
;;

let run_at_internal t time f a =
  let execution_context = Scheduler.current_execution_context t.scheduler in
  if Time_ns.( > ) time (Timing_wheel_ns.now t.events)
  then schedule_job t ~at:time execution_context f a
  else begin
    Scheduler.enqueue t.scheduler execution_context f a;
    Alarm.null ();
  end
;;

let run_at t time f a = ignore (run_at_internal t time f a : _ Alarm.t)

let run_after t span f a = run_at t (span_to_time t span) f a

let at =
  let fill result = Ivar.fill result () in
  fun t time ->
    if Time_ns.( <= ) time (Timing_wheel_ns.now t.events)
    then Deferred.unit
    else
      let result = Ivar.create () in
      ignore (run_at_internal t time fill result : _ Alarm.t);
      Ivar.read result;
;;

let after t span = at t (span_to_time t span)

let remove_alarm_if_scheduled t alarm =
  if Timing_wheel_ns.mem t.events alarm then begin
    Scheduler.free_job t.scheduler (Alarm.value t.events alarm);
    Timing_wheel_ns.remove t.events alarm;
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
    ; time_source          : Time_source.t
    }
  [@@deriving fields, sexp_of]

  type t_unit = (unit, unit) t [@@deriving sexp_of]

  let fired t = Ivar.read t.fired

  let invariant invariant_a invariant_h t =
    Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
      let events = t.time_source.events in
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
          | Some (`Happened h) -> invariant_h h))
        ~time_source:ignore)
  ;;

  let status t =
    match Deferred.peek (Ivar.read t.fired) with
    | None -> `Scheduled_at t.scheduled_at
    | Some x -> (x :> [ `Happened of _ | `Aborted of _ | `Scheduled_at of Time_ns.t ])
  ;;

  let abort t a =
    if debug then Debug.log "Time_source.Event.abort" t [%sexp_of: (_, _) t];
    match Deferred.peek (fired t) with
    | Some (`Aborted  a) -> `Previously_aborted  a
    | Some (`Happened h) -> `Previously_happened h
    | None ->
      Ivar.fill t.fired (`Aborted a);
      remove_alarm_if_scheduled t.time_source t.alarm;
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
    then Debug.log "Time_source.Event.reschedule_at" (t, at)
           [%sexp_of: (_, _) t * Time_ns.t];
    match Deferred.peek (fired t) with
    | Some (`Aborted  a) -> `Previously_aborted  a
    | Some (`Happened h) -> `Previously_happened h
    | None ->
      let events = t.time_source.events in
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
            t.time_source.handle_fired t.alarm;
            Timing_wheel_ns.remove events t.alarm;
          end;
        `Ok
      end;
  ;;

  let reschedule_after t span = reschedule_at t (span_to_time t.time_source span)

  let run_at time_source scheduled_at f z =
    if debug
    then Debug.log "Time_source.Event.run_at" scheduled_at [%sexp_of: Time_ns.t];
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
    let alarm = run_at_internal time_source scheduled_at fire z in
    { alarm; scheduled_at; fired; time_source = read_only time_source }
  ;;

  let at time_source time = run_at time_source time ignore ()

  let run_after time_source span f a =
    run_at time_source (span_to_time time_source span) f a
  ;;

  let after time_source span = at time_source (span_to_time time_source span)
end

let at_times ?(stop = Deferred.never ()) t next_time =
  let tail = Tail.create () in
  let rec loop () =
    choose [ choice stop                  (fun () -> `Stop)
           ; choice (at t (next_time ())) (fun () -> `Tick)
           ]
    >>> function
    | `Stop -> Tail.close_exn tail
    | `Tick -> Tail.extend tail (); loop ()
  in
  loop ();
  Tail.collect tail;
;;

let at_varying_intervals ?stop t compute_span =
  at_times t ?stop (fun () -> Time_ns.add (now t) (compute_span ()))
;;

let at_intervals ?start ?stop t interval =
  let start =
    match start with
    | Some x -> x
    | None -> now t
  in
  at_times t ?stop (fun () ->
    Time_ns.next_multiple ~base:start ~after:(now t) ~interval ())
;;

module Continue = struct
  type t =
    | Immediately
    | After         of Time_ns.Span.t
    | Next_multiple of Time_ns.t * Time_ns.Span.t

  let immediately = Immediately

  let at t time_source =
    match t with
    | Immediately -> Timing_wheel_ns.now time_source.events
    | After span -> span_to_time time_source span
    | Next_multiple (base, interval) ->
      Time_ns.next_multiple ~base ~after:(now time_source) ~interval ()
  ;;
end

let run_repeatedly
  ?(start = Deferred.unit)
  ?stop
  ?(continue_on_error = true)
  t ~f ~continue =
  start
  >>> fun () ->
  let alarm = ref (Alarm.null ()) in
  let stop =
    match stop with
    | None -> Deferred.never ()
    | Some stop ->
      upon stop (fun () -> remove_alarm_if_scheduled t !alarm);
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
    then alarm := run_at_internal t (Continue.at continue t) run_f ()
  and continue_try_with or_error =
    begin match or_error with
    | Ok () -> ()
    | Error error -> Monitor.send_exn (Monitor.current ()) error;
    end;
    continue_f ()
  in
  run_f ()
;;

let every' ?start ?stop ?continue_on_error t span f =
  if Time_ns.Span.( <= ) span Time_ns.Span.zero
  then failwiths "Time_source.every got nonpositive span" span [%sexp_of: Time_ns.Span.t];
  run_repeatedly t ?start ?stop ?continue_on_error ~f ~continue:(After span)
;;

let every ?start ?stop ?continue_on_error t span f =
  every' t ?start ?stop ?continue_on_error span (fun () -> f (); Deferred.unit)
;;

let run_at_intervals' ?start ?stop ?continue_on_error t interval f =
  let now = now t in
  let base, start =
    match start with
    | None       -> now, None
    | Some start ->
      start,
      Some (at t (Time_ns.next_multiple ()
                    ~base:start
                    ~after:now
                    ~can_equal_after:true
                    ~interval))
  in
  run_repeatedly t ?start ?stop ?continue_on_error ~f
    ~continue:(Next_multiple (base, interval))
;;

let run_at_intervals ?start ?stop ?continue_on_error t interval f =
  run_at_intervals' ?start ?stop ?continue_on_error t interval
    (fun () -> f (); Deferred.unit)
;;

let with_timeout t span d =
  let timeout = Event.after t span in
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
          failwith "Time_source.with_timeout bug: should only abort once"
        end;
        `Result v)
    ; choice (Event.fired timeout) (function
        | `Happened () -> `Timeout
        | `Aborted  () ->
          failwith "Time_source.with_timeout bug: both completed and timed out")
    ]
;;
