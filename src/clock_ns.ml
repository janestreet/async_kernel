open Core_kernel.Std
open Import  let _ = _squelch_unused_module_warning_
open Deferred_std

module Stream = Async_stream

let debug = Debug.clock

(* We use [Time_ns.now ()] rather than [cycle_start] for the base time.  There can be
   substantial difference between the two when people do long running computations or
   mix blocking code with async.  And humans expect that [after] is based on the
   current time, not some artifact of async implementation. *)
let span_to_time span = Time_ns.add (Time_ns.now ()) span

let run_at_internal (scheduler : Raw_scheduler.t) time f a =
  let events = scheduler.events in
  let execution_context = Raw_scheduler.current_execution_context scheduler in
  if Time_ns.( <= ) time (Timing_wheel_ns.now events)
  then Raw_scheduler.enqueue scheduler execution_context f a
  else ignore (Timing_wheel_ns.add events ~at:time
                 (Raw_scheduler.create_job scheduler execution_context f a)
               : _ Timing_wheel_ns.Alarm.t);
;;

let run_at time f a = run_at_internal (Raw_scheduler.t ()) time f a

let run_after span f a = run_at (span_to_time span) f a

let at =
  let fill result = Ivar.fill result () in
  fun time ->
    let scheduler = Raw_scheduler.t () in
    if Time_ns.( <= ) time (Timing_wheel_ns.now scheduler.events)
    then Deferred.unit
    else
      let result = Ivar.create () in
      run_at_internal scheduler time fill result;
      Ivar.read result;
;;

let after span = at (span_to_time span)

module Event = struct

  type t =
    { alarm : Job.t Timing_wheel_ns.Alarm.t
    ; ready : [ `Happened | `Aborted ] Ivar.t
    }
  with fields, sexp_of

  let invariant t =
    Invariant.invariant _here_ t <:sexp_of< t >> (fun () ->
      let events = Raw_scheduler.(events (t ())) in
      let check f = Invariant.check_field t f in
      Fields.iter
        ~alarm:(check (fun alarm ->
          if Ivar.is_full t.ready
          then assert (not (Timing_wheel_ns.mem events alarm))))
        ~ready:ignore)
  ;;

  let status t =
    match Deferred.peek (Ivar.read t.ready) with
    | Some x -> (x :> [ `Happened | `Aborted | `Will_happen_at of Time_ns.t ])
    | None ->
      let scheduler = Raw_scheduler.t () in
      let events = scheduler.events in
      if Timing_wheel_ns.mem events t.alarm
      then `Will_happen_at (Timing_wheel_ns.Alarm.at events t.alarm)
      else begin
        (* [advance_clock] already scheduled [fire] and removed [t.alarm], but the [fire]
           job didn't yet run.  We go ahead and pretend it happened though.  The
           subsequent [fire] job will be a no-op. *)
        Ivar.fill t.ready `Happened;
        `Happened
      end;
  ;;

 let abort t =
    if debug then Debug.log "Clock_ns.Event.abort" t <:sexp_of< t >>;
    match status t with
    | `Aborted -> `Previously_aborted
    | `Happened -> `Previously_happened
    | `Will_happen_at _ ->
      Ivar.fill t.ready `Aborted;
      let scheduler = Raw_scheduler.t () in
      let events = scheduler.events in
      (* [t.alarm] might have been removed from [events] due to [advance_clock], even
         though the resulting [fire] job hasn't run yet.  So, we have to check before
         removing it. *)
      if Timing_wheel_ns.mem events t.alarm then begin
        Raw_scheduler.free_job scheduler (Timing_wheel_ns.Alarm.value events t.alarm);
        Timing_wheel_ns.remove events t.alarm;
      end;
      `Ok
  ;;

  let at time =
    if debug then Debug.log "Clock_ns.Event.at" time <:sexp_of< Time_ns.t >>;
    let scheduler = Raw_scheduler.t () in
    let events = Raw_scheduler.events scheduler in
    let t =
      if Time_ns.( <= ) time (Timing_wheel_ns.now events)
      then
        { alarm = Timing_wheel_ns.Alarm.null ()
        ; ready = Ivar.create_full `Happened
        }
      else
        let ready = Ivar.create () in
        let fire () = Ivar.fill_if_empty ready `Happened in
        let alarm =
          Timing_wheel_ns.add events ~at:time
            (Raw_scheduler.create_job scheduler
               (Raw_scheduler.current_execution_context scheduler)
               fire ())
        in
        { alarm ; ready }
    in
    t, Ivar.read t.ready
  ;;

  let after span = at (span_to_time span)
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
  choose [ choice d            (fun v -> `Result v)
         ; choice (after span) (fun _ -> `Timeout)
         ]
;;
