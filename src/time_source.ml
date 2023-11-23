open! Core
open! Import
open! Deferred_std

let debug = Debug.clock

module Alarm = Timing_wheel.Alarm
module Deferred = Deferred1
module Scheduler = Scheduler1

let upon = Deferred.upon
let choose = Deferred.choose
let choice = Deferred.choice
let ( >>> ) = upon

module T1 = struct
  include Synchronous_time_source0.T1

  (* We don't include the [id] in the sexp because the user (rightly) can't control it, so
     it's hard to make it deterministic in tests. *)
  let sexp_of_t
    _
    { id = _
    ; advance_errors = _
    ; am_advancing = _
    ; events
    ; fired_events = _
    ; handle_fired = _
    ; is_wall_clock
    ; most_recently_fired = _
    ; scheduler = _
    }
    =
    if is_wall_clock
    then [%message "<wall_clock>"]
    else
      [%message
        (is_wall_clock : bool)
          (* We don't display the [Job.t]s in [events] because those are
             pool pointers, which are uninformative. *)
          (events : _ Timing_wheel.t)]
  ;;
end

open T1

module Read_write = struct
  type t = read_write T1.t [@@deriving sexp_of]

  let invariant = invariant
  let invariant_with_jobs = invariant_with_jobs
end

type t = read T1.t [@@deriving sexp_of]

let invariant = invariant
let invariant_with_jobs = invariant_with_jobs
let read_only (t : [> read ] T1.t) = (t :> t)
let create = Scheduler.create_time_source
let wall_clock = Scheduler.wall_clock
let alarm_precision t = Timing_wheel.alarm_precision t.events
let is_wall_clock t = t.is_wall_clock
let next_alarm_fires_at t = Timing_wheel.next_alarm_fires_at t.events
let timing_wheel_now t = Timing_wheel.now t.events
let id t = t.id

module Id = Synchronous_time_source0.Id

let now t =
  if t.is_wall_clock
  then
    (* For the wall-clock time-source, we use [Time_ns.now ()] rather than
       [Timing_wheel.now t.events].  The latter is only updated at the start of each
       cycle.  There can be substantial difference between the two when people do long
       running computations or mix blocking code with async.  And humans expect that
       wall-clock time is based on [Time.now], not some artifact of async
       implementation. *)
    Time_ns.now ()
  else timing_wheel_now t
;;

(* We preallocate [send_exn] to avoid allocating it on each call to [advance_clock]. *)
let send_exn = Some Monitor.send_exn
let advance_directly t ~to_ = Synchronous_time_source0.advance_clock t ~to_ ~send_exn

let advance_directly_stop_on_next_alarm t ~to_ =
  Synchronous_time_source0.advance_clock_stop_at_next_alarm t ~to_ ~send_exn
;;

let advance_directly_by t by = advance_directly t ~to_:(Time_ns.after (now t) by)
let advance = advance_directly
let advance_by = advance_directly_by
let fire_past_alarms t = Synchronous_time_source0.fire_past_alarms t ~send_exn
let yield t = Bvar.wait (Scheduler.yield t.scheduler)
let can_run_a_job t = Scheduler.num_pending_jobs t > 0 || Bvar.has_any_waiters t.yield

module Eager_deferred = struct
  let bind_unit t ~f =
    if Deferred.is_determined t
    then f ()
    else (
      let%bind () = t in
      f ())
  ;;

  let map t ~f =
    if Deferred.is_determined t
    then return (f (Deferred.value_exn t))
    else Deferred.map t ~f
  ;;
end

let advance_by_alarms ?wait_for t ~to_ =
  let run_queued_alarms () =
    (* Every time we want to run queued alarms we need to yield control back to the
       [Async.Scheduler] and [wait_for] any logic that is supposed to finish at this time
       before advancing.  If no [wait_for] logic is specified we can simply yield control
       by invoking [yield t], which enqueues another job at the end of the scheduler job
       queue so alarm jobs have the opportunity to run before we advance. *)
    match wait_for with
    | None -> yield t
    | Some f -> f ()
  in
  let one_step () =
    if Synchronous_time_source0.any_fired_events_to_run t
    then now t, `continue
    else to_, `check_time
  in
  let rec walk_alarms () =
    let advance_to, next = one_step () in
    advance_directly_stop_on_next_alarm t ~to_:advance_to;
    fire_past_alarms t;
    Eager_deferred.bind_unit (run_queued_alarms ()) ~f:(fun () ->
      match next with
      | `check_time ->
        if Time_ns.( < ) (timing_wheel_now t) to_ then walk_alarms () else return ()
      | `continue -> walk_alarms ())
  in
  (* This first [run_queued_alarms] call allows [Clock_ns.every] the opportunity to run
     its continuation deferreds so that they can reschedule alarms.  This is particularly
     useful in our "advance hits intermediate alarms" unit test below, but likely useful
     in other cases where [every] is synchronously followed by [advance]. *)
  let%bind () = run_queued_alarms () in
  walk_alarms ()
;;

let advance_by_max_alarms_in_each_timing_wheel_interval ?wait_for t ~to_ =
  let run_queued_alarms () =
    (* Every time we want to run queued alarms we need to yield control back to the
       [Async.Scheduler] and [wait_for] any logic that is supposed to finish at this time
       before advancing.  If no [wait_for] logic is specified we can simply yield control
       by invoking [yield t], which enqueues another job at the end of the scheduler job
       queue so alarm jobs have the opportunity to run before we advance. *)
    match wait_for with
    | None -> yield t
    | Some f -> f ()
  in
  let finish () =
    advance_directly t ~to_;
    fire_past_alarms t;
    (* so that alarms scheduled at or before [to_] fire *)
    run_queued_alarms ()
  in
  let rec walk_alarms () =
    match next_alarm_fires_at t with
    | None -> finish ()
    | Some next_alarm_fires_at ->
      if Time_ns.( >= ) next_alarm_fires_at to_
      then finish ()
      else (
        advance_directly t ~to_:(Timing_wheel.max_alarm_time_in_min_interval_exn t.events);
        fire_past_alarms t;
        let queued_alarms_ran = run_queued_alarms () in
        Eager_deferred.bind_unit queued_alarms_ran ~f:walk_alarms)
  in
  fire_past_alarms t;
  (* This first [run_queued_alarms] call allows [Clock_ns.every] the opportunity to run
     its continuation deferreds so that they can reschedule alarms.  This is particularly
     useful in our "advance hits intermediate alarms" unit test below, but likely useful
     in other cases where [every] is synchronously followed by [advance]. *)
  let%bind () = run_queued_alarms () in
  walk_alarms ()
;;

let advance_by_alarms_by ?wait_for t by =
  advance_by_alarms ?wait_for t ~to_:(Time_ns.after (now t) by)
;;

let span_to_time t span = Time_ns.after (now t) span

let schedule_job t ~at execution_context f a =
  let alarm =
    Timing_wheel.add
      t.events
      ~at
      (Job_or_event.of_job (Scheduler.create_job t.scheduler execution_context f a))
  in
  (match t.scheduler.event_added_hook with
   | None -> ()
   | Some f -> f at);
  alarm
;;

let run_at_internal t time f a =
  let execution_context = Scheduler.current_execution_context t.scheduler in
  if Time_ns.( > ) time (Timing_wheel.now t.events)
  then schedule_job t ~at:time execution_context f a
  else (
    Scheduler.enqueue t.scheduler execution_context f a;
    Alarm.null ())
;;

let run_at t time f a = ignore (run_at_internal t time f a : _ Alarm.t)
let run_after t span f a = run_at t (span_to_time t span) f a

let at =
  let fill result = Ivar.fill_exn result () in
  fun t time ->
    if Time_ns.( <= ) time (Timing_wheel.now t.events)
    then return ()
    else (
      let result = Ivar.create () in
      ignore (run_at_internal t time fill result : _ Alarm.t);
      Ivar.read result)
;;

let after t span = at t (span_to_time t span)

let remove_alarm t alarm : unit =
  let job_or_event = Alarm.value t.events alarm in
  (let open Job_or_event.Match in
   let (K k) = kind job_or_event in
   match k, project k job_or_event with
   | Job, job -> Scheduler.free_job t.scheduler job
   | Event, _ ->
     (* This is unreachable because [alarm] only ever comes from [Event.alarm] which only
        ever gets populated by a call to [schedule_job]. *)
     assert false);
  Timing_wheel.remove t.events alarm
;;

let remove_alarm_if_scheduled t alarm =
  if Timing_wheel.mem t.events alarm then remove_alarm t alarm
;;

module Event = struct
  module Fired = struct
    type ('a, 'h) t =
      | Aborted of 'a
      | Happened of 'h
    [@@deriving sexp_of]
  end

  type ('a, 'h) t =
    { mutable alarm : Job_or_event.t Alarm.t
    ; mutable fire : unit -> unit
    ; (* As long as [Ivar.is_empty fired], we have not yet committed to whether the event
         will happen or be aborted.  When [Ivar.is_empty fired], the alarm may or may not
         be in the timing wheel -- if it isn't, then there's a job in Async's job queue
         that will fire the event, unless it is aborted before that job can run. *)
      fired : ('a, 'h) Fired.t Ivar.t
    ; (* [num_fires_to_skip] is used to reschedule events that have fired and entered the
         Async job queue, but have not yet run.  Those jobs only run if [num_fires_to_skip
         = 0], and otherwise just decrement it.  So, to reschedule an event in such a
         state, we increment [num_fires_to_skip] and add a new alarm to the timing
         wheel. *)
      mutable num_fires_to_skip : int
    ; (* [scheduled_at] is the time at which [t] has most recently been scheduled to fire.
         While [t.alarm] is still in the timing wheel, this is the same as [Alarm.at
         t.alarm]. *)
      mutable scheduled_at : Time_ns.t
    ; time_source : Synchronous_time_source0.t
    }
  [@@deriving fields ~getters ~iterators:iter, sexp_of]

  type t_unit = (unit, unit) t [@@deriving sexp_of]

  let fired t = Ivar.read t.fired

  let invariant invariant_a invariant_h t =
    Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
      let events = t.time_source.events in
      let check f = Invariant.check_field t f in
      Fields.iter
        ~alarm:
          (check (fun alarm ->
             if Ivar.is_full t.fired
             then assert (not (Timing_wheel.mem events alarm))
             else if Timing_wheel.mem events alarm
             then assert (Job_or_event.is_job (Alarm.value events alarm))))
        ~fire:ignore
        ~fired:
          (check (fun (fired : _ Fired.t Ivar.t) ->
             match Deferred.peek (Ivar.read fired) with
             | None -> ()
             | Some (Aborted a) -> invariant_a a
             | Some (Happened h) -> invariant_h h))
        ~num_fires_to_skip:
          (check (fun num_fires_to_skip -> assert (num_fires_to_skip >= 0)))
        ~scheduled_at:
          (check (fun scheduled_at ->
             if Timing_wheel.mem events t.alarm
             then [%test_result: Time_ns.t] scheduled_at ~expect:(Alarm.at events t.alarm)))
        ~time_source:ignore)
  ;;

  module Status = struct
    type ('a, 'h) t =
      | Aborted of 'a
      | Happened of 'h
      | Scheduled_at of Time_ns.t
    [@@deriving sexp_of]
  end

  let status t : _ Status.t =
    match Deferred.peek (Ivar.read t.fired) with
    | None -> Scheduled_at t.scheduled_at
    | Some (Aborted a) -> Aborted a
    | Some (Happened h) -> Happened h
  ;;

  module Abort_result = struct
    type ('a, 'h) t =
      | Ok
      | Previously_aborted of 'a
      | Previously_happened of 'h
    [@@deriving sexp_of]
  end

  let abort t a : _ Abort_result.t =
    if debug then Debug.log "Time_source.Event.abort" t [%sexp_of: (_, _) t];
    match Deferred.peek (fired t) with
    | Some (Aborted a) -> Previously_aborted a
    | Some (Happened h) -> Previously_happened h
    | None ->
      Ivar.fill_exn t.fired (Aborted a);
      remove_alarm_if_scheduled t.time_source t.alarm;
      Ok
  ;;

  let abort_exn t a =
    match abort t a with
    | Ok -> ()
    | Previously_happened _ ->
      raise_s
        [%message "Clock.Event.abort_exn failed to abort event that previously happened"]
    | Previously_aborted _ ->
      raise_s
        [%message "Clock.Event.abort_exn failed to abort event that previously aborted"]
  ;;

  let abort_if_possible t a = ignore (abort t a : _ Abort_result.t)
  let schedule t = t.alarm <- run_at_internal t.time_source t.scheduled_at t.fire ()

  module Reschedule_result = struct
    type ('a, 'h) t =
      | Ok
      | Previously_aborted of 'a
      | Previously_happened of 'h
    [@@deriving sexp_of]
  end

  let reschedule_at t at : _ Reschedule_result.t =
    if debug
    then
      Debug.log "Time_source.Event.reschedule_at" (t, at) [%sexp_of: (_, _) t * Time_ns.t];
    match Deferred.peek (fired t) with
    | Some (Aborted a) -> Previously_aborted a
    | Some (Happened h) -> Previously_happened h
    | None ->
      let events = t.time_source.events in
      let is_in_timing_wheel = Timing_wheel.mem events t.alarm in
      let am_trying_to_reschedule_in_the_future =
        Time_ns.( > ) at (Timing_wheel.now events)
      in
      t.scheduled_at <- at;
      (match am_trying_to_reschedule_in_the_future, is_in_timing_wheel with
       | false, false -> ()
       | false, true ->
         t.time_source.handle_fired t.alarm;
         Timing_wheel.remove events t.alarm
       | true, false ->
         t.num_fires_to_skip <- t.num_fires_to_skip + 1;
         schedule t
       | true, true -> Timing_wheel.reschedule events t.alarm ~at);
      Ok
  ;;

  let reschedule_after t span = reschedule_at t (span_to_time t.time_source span)

  let run_at time_source scheduled_at f z =
    if debug then Debug.log "Time_source.Event.run_at" scheduled_at [%sexp_of: Time_ns.t];
    let t =
      { alarm = Alarm.null ()
      ; fire = ignore (* set below *)
      ; fired = Ivar.create ()
      ; num_fires_to_skip = 0
      ; scheduled_at
      ; time_source = read_only time_source
      }
    in
    let fire () =
      (* [fire] runs in an Async job.  The event may have been aborted after the job
         was enqueued, so [fire] must check [fired]. *)
      if Ivar.is_empty t.fired
      then
        if t.num_fires_to_skip > 0
        then t.num_fires_to_skip <- t.num_fires_to_skip - 1
        else (
          let result = f z in
          (* [f z] may have aborted the event, so we must check [fired] again. *)
          if Ivar.is_empty t.fired then Ivar.fill_exn t.fired (Happened result))
    in
    t.fire <- fire;
    schedule t;
    t
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
    choose
      [ choice stop (fun () -> `Stop); choice (at t (next_time ())) (fun () -> `Tick) ]
    >>> function
    | `Stop -> Tail.close_exn tail
    | `Tick ->
      Tail.extend tail ();
      loop ()
  in
  loop ();
  Tail.collect tail
;;

let at_varying_intervals ?stop t compute_span =
  at_times t ?stop (fun () -> Time_ns.after (now t) (compute_span ()))
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
    | After of Time_ns.Span.t
    | Next_multiple of Time_ns.t * Time_ns.Span.t

  let immediately = Immediately

  let at t time_source =
    match t with
    | Immediately -> Timing_wheel.now time_source.events
    | After span -> span_to_time time_source span
    | Next_multiple (base, interval) ->
      Time_ns.next_multiple ~base ~after:(now time_source) ~interval ()
  ;;
end

let run_repeatedly
  ?(start = return ())
  ?stop
  ?(continue_on_error = true)
  ?(finished = Ivar.create ())
  t
  ~f
  ~continue
  =
  start
  >>> fun () ->
  let alarm = ref (Alarm.null ()) in
  let stop =
    match stop with
    | None -> Deferred.never ()
    | Some stop ->
      upon stop (fun () ->
        if Timing_wheel.mem t.events !alarm
        then (
          remove_alarm t !alarm;
          Ivar.fill_if_empty finished ()));
      stop
  in
  (* [run_f], [continue_f], and [continue_try_with] are defined so that we allocate their
     closures once, not once per iteration. *)
  let rec run_f () =
    (* Before calling [f], we synchronously check whether [stop] is determined. *)
    if Deferred.is_determined stop
    then Ivar.fill_if_empty finished ()
    else if continue_on_error
    then Monitor.try_with f ~run:`Now ~rest:`Raise >>> continue_try_with
    else (
      let d = f () in
      if Deferred.is_determined d then continue_f () else d >>> continue_f)
  and continue_f () =
    if Deferred.is_determined stop
    then Ivar.fill_if_empty finished ()
    else alarm := run_at_internal t (Continue.at continue t) run_f ()
  and continue_try_with or_error =
    (match or_error with
     | Ok () -> ()
     | Error error -> Monitor.send_exn (Monitor.current ()) error);
    continue_f ()
  in
  run_f ()
;;

let every' ?start ?stop ?continue_on_error ?finished t span f =
  if Time_ns.Span.( <= ) span Time_ns.Span.zero
  then raise_s [%message "Time_source.every got nonpositive span" (span : Time_ns.Span.t)];
  run_repeatedly t ?start ?stop ?continue_on_error ?finished ~f ~continue:(After span)
;;

let every ?start ?stop ?continue_on_error t span f =
  every' t ?start ?stop ?continue_on_error ?finished:None span (fun () ->
    f ();
    return ())
;;

let run_at_intervals' ?start ?stop ?continue_on_error t interval f =
  let now = now t in
  let base, start =
    match start with
    | None -> now, None
    | Some start ->
      ( start
      , Some
          (at
             t
             (Time_ns.next_multiple
                ()
                ~base:start
                ~after:now
                ~can_equal_after:true
                ~interval)) )
  in
  run_repeatedly
    t
    ?start
    ?stop
    ?continue_on_error
    ~f
    ~continue:(Next_multiple (base, interval))
;;

let run_at_intervals ?start ?stop ?continue_on_error t interval f =
  run_at_intervals' ?start ?stop ?continue_on_error t interval (fun () ->
    f ();
    return ())
;;

let with_timeout t span d =
  let timeout = Event.after t span in
  choose
    (* The code below does exhaustive case analysis in both [choice]s.  Because [timeout]
       does not escape the scope of this function, certain cases should be impossible, and
       are marked as such with exceptions.  We do not expect those exceptions to occur,
       but if they do, it likely indicates a bug in [choose] rather than
       [with_timeout]. *)
    [ choice d (fun v ->
        (match Event.abort timeout () with
         (* [Previously_happened] can occur if both [d] and [wait] become determined at
            the same time, e.g. [with_timeout (sec 0.) (return ())]. *)
         | Ok | Previously_happened () -> ()
         | Previously_aborted () ->
           raise_s [%message "Time_source.with_timeout bug: should only abort once"]);
        `Result v)
    ; choice (Event.fired timeout) (function
        | Happened () -> `Timeout
        | Aborted () ->
          raise_s [%message "Time_source.with_timeout bug: both completed and timed out"])
    ]
;;

let with_timeout_exn t span d ~error =
  Deferred.map (with_timeout t span d) ~f:(function
    | `Result result -> result
    | `Timeout -> Error.raise error)
;;

let duration_of t f =
  let start = now t in
  (* Eager map to provide more accurate timings when [f] is synchronous. *)
  Eager_deferred.map (f ()) ~f:(fun result ->
    let duration = Time_ns.diff (now t) start in
    result, duration)
;;

let of_synchronous t = t
let to_synchronous t = t

let timing_wheel_has_event_at_or_before wheel time =
  if Timing_wheel.is_empty wheel
  then false
  else (
    let next_alarm = Timing_wheel.next_alarm_fires_at_exn wheel in
    Time_ns_in_this_directory.(next_alarm <= time))
;;

let advance_directly_if_quiescent t ~to_ =
  let is_quescent =
    (* Since this function is intended to be just a fast case of [advance_by_alarms],
       we want to make sure that the call to [Scheduler.yield ()] can be elided,
       hence the [can_run_a_job] check.

       We're not checking epoll, and we're not checking the [external_jobs].  That is an
       observable difference, but since [advance_by_alarms ?wait_for:None] is already
       pretty broken when waiting for external events, it's not going to be meaningful.
    *)
    can_run_a_job t.scheduler
    || Synchronous_time_source0.has_events_to_run t
    || timing_wheel_has_event_at_or_before t.events to_
  in
  if is_quescent
  then false
  else (
    advance_directly t ~to_;
    true)
;;
