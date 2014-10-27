open Core.Std
open Import  let _ = _squelch_unused_module_warning_
open Deferred_std

module Stream = Async_stream
module Job = Jobs.Job

let debug = Debug.clock

(* We use [Time.now ()] rather than [cycle_start] for the base time.  There can be
   substantial difference between the two when people do long running computations or
   mix blocking code with async.  And humans expect that [after] is based on the
   current time, not some artifact of async implementation. *)
let span_to_time span = Time.add (Time.now ()) span

let run_at time f a =
  let scheduler = Raw_scheduler.t () in
  let events = scheduler.events in
  let execution_context = Raw_scheduler.current_execution_context scheduler in
  if Time.(<) time (Timing_wheel.now events)
  then Raw_scheduler.enqueue scheduler execution_context f a
  else ignore (Timing_wheel.add events ~at:time
                 (Raw_scheduler.create_job scheduler execution_context f a)
               : _ Timing_wheel.Alarm.t);
;;

let run_after span f a = run_at (span_to_time span) f a

let at =
  let fill result = Ivar.fill result () in
  fun time ->
    let result = Ivar.create () in
    run_at time fill result;
    Ivar.read result;
;;

let after span = at (span_to_time span)

module Event = struct

  type t =
    { mutable alarm : Job.t Timing_wheel.Alarm.t
    ; ready         : [ `Happened | `Aborted ] Ivar.t
    }
  with fields, sexp_of

  let invariant t =
    Invariant.invariant _here_ t <:sexp_of< t >> (fun () ->
      let events = Raw_scheduler.(events (t ())) in
      let check f = Invariant.check_field t f in
      Fields.iter
        ~alarm:(check (fun alarm ->
          if Ivar.is_full t.ready
          then assert (not (Timing_wheel.mem events alarm))))
        ~ready:ignore)
  ;;

  let status t =
    match Deferred.peek (Ivar.read t.ready) with
    | None -> `Waiting
    | Some x -> (x :> [ `Happened | `Aborted | `Waiting ])
  ;;

  let abort t =
    if debug then Debug.log "Clock.Event.abort" t <:sexp_of< t >>;
    match status t with
    | `Aborted -> `Previously_aborted
    | `Happened -> `Previously_happened
    | `Waiting ->
      Ivar.fill t.ready `Aborted;
      let events = Raw_scheduler.(events (t ())) in
      (* [t.alarm] might have been removed from [events] due to [advance_clock], even
         though the resulting [fire] job hasn't run yet.  So, we have to check before
         removing it. *)
      if Timing_wheel.mem events t.alarm then Timing_wheel.remove events t.alarm;
      `Ok
  ;;

  let at time =
    if debug then Debug.log "Clock.Event.at" time <:sexp_of< Time.t >>;
    let t = { alarm = Timing_wheel.Alarm.null (); ready = Ivar.create () } in
    let scheduler = Raw_scheduler.t () in
    let events = Raw_scheduler.events scheduler in
    let fire () =
      match status t with
      | `Happened -> assert false
      | `Aborted -> ()
      | `Waiting -> Ivar.fill t.ready `Happened
    in
    if Time.(<) time (Timing_wheel.now events)
    then fire ()
    else t.alarm <- Timing_wheel.add events ~at:time
                      (Raw_scheduler.create_job scheduler
                         (Raw_scheduler.current_execution_context scheduler)
                         fire ());
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
  at_times ?stop (fun () -> Time.add (Time.now ()) (compute_span ()))
;;

let at_intervals ?(start = Time.now ()) ?stop interval =
  at_times ?stop (fun () ->
    Time.next_multiple ~base:start ~after:(Time.now ()) ~interval ())
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
  let monitor = Monitor.create ~name:"Clock.run_repeatedly" () in
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
  if Time.Span.(<=) span Time.Span.zero
  then failwiths "Clock.every got nonpositive span" span <:sexp_of< Time.Span.t >>;
  run_repeatedly ?start ?stop ?continue_on_error ~f
    ~continue:(fun () -> after span) ()
;;

let every ?start ?stop ?continue_on_error span f =
  every' ?start ?stop ?continue_on_error span (fun () -> f (); Deferred.unit)
;;

let run_at_intervals' ?start ?stop ?continue_on_error interval f =
  let now = Time.now () in
  let base, start =
    match start with
    | None       -> now, None
    | Some start ->
      start,
      Some (at (Time.next_multiple ()
                  ~base:start
                  ~after:now
                  ~can_equal_after:true
                  ~interval))
  in
  run_repeatedly ?start ?stop ?continue_on_error ~f
    ~continue:(fun () -> at (Time.next_multiple ~base ~after:(Time.now ()) ~interval ()))
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
