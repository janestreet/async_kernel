open Core.Std
open Deferred_std

module Stream = Async_stream

let debug = Debug.clock

let events () = Raw_scheduler.((t ()).events)

(* We use [Time.now ()] rather than [cycle_start] for the base time.  There can be
   substantial difference between the two when people do long running computations or
   mix blocking code with async.  And humans expect that [after] is based on the
   current time, not some artifact of async implementation. *)
let span_to_time span = Time.add (Time.now ()) span

let run_at time f a =
  let scheduler = Raw_scheduler.t () in
  let job = Job.create (Raw_scheduler.current_execution_context scheduler) f a in
  match Events.add (events ()) ~at:time job with
  | `Ok _ -> ()
  | `Not_in_the_future -> Raw_scheduler.add_job scheduler job
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
  (* Clock events start in the [Uninitialized] state just for their creation (because of
     cyclic types), and then are immediately marked as [Waiting] or [Happened], depending
     on the time.  A [Waiting] event will then either [Happen] at the appropriate time,
     or be [Aborted] prior to when it would have [Happened].

     Uninitialized
     |           |
     v           v
     Waiting --> Happened
     |
     v
     Aborted *)
  type waiting =
    { event : Execution_context.t Job.t Events.Event.t;
      ready : [ `Happened | `Aborted ] Ivar.t;
    }
  with sexp_of

  type state =
  | Uninitialized
  | Aborted
  | Happened
  | Waiting of waiting
  with sexp_of

  type t = state ref with sexp_of

  let invariant t : unit =
    try
      match !t with
      | Uninitialized | Aborted | Happened -> ()
      | Waiting { event = _; ready } ->
        Ivar.invariant Fn.ignore ready;
        assert (Ivar.is_empty ready);
    with exn ->
      failwiths "Clock.Event.invariant failed" (exn, t) (<:sexp_of< exn * t >>)
  ;;

  let status t =
    match !t with
    | Uninitialized -> assert false
    | Aborted -> `Aborted
    | Happened -> `Happened
    | Waiting _ -> `Waiting
  ;;

  let abort t =
    if debug then Debug.log "Clock.Event.abort" t <:sexp_of< t >>;
    match !t with
    | Uninitialized -> assert false
    | Aborted -> `Previously_aborted
    | Happened -> `Previously_happened
    | Waiting waiting ->
      t := Aborted;
      begin match Events.remove (events ()) waiting.event with
      | `Removed -> ()
      | `Not_present -> failwiths "Clock.Event.abort failed" t <:sexp_of< t >>
      end;
      `Ok
  ;;

  let at time =
    if debug then Debug.log "Clock.Event.at" time <:sexp_of< Time.t >>;
    let ready = Ivar.create () in
    let t = ref Uninitialized in
    let events = events () in
    let scheduler = Scheduler.t () in
    let fire () =
      t := Happened;
      Ivar.fill ready `Happened;
    in
    let job = Job.create (Scheduler.current_execution_context scheduler) fire () in
    begin match Events.add events ~at:time job with
    | `Ok event -> t := Waiting { event; ready };
    | `Not_in_the_future -> fire ()
    end;
    t, Ivar.read ready
  ;;

  let after span = at (span_to_time span)
end

open Monitor.Exported_for_scheduler

let at_times ?(stop = Deferred.never ()) next_time =
  let tail = Tail.create () in
  let rec loop () =
    choose [ choice stop (fun () -> `Stop);
             choice (at (next_time ())) (fun () -> `Tick);
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
  at_times ?stop (fun () -> Time.next_multiple ~base:start ~after:(Time.now ()) ~interval)
;;

let every' ?(start = Deferred.unit) ?(stop = Deferred.never ())
    ?(continue_on_error = true) span f =
  if Time.Span.(<=) span Time.Span.zero then
    failwiths "Clock.every got nonpositive span" span <:sexp_of< Time.Span.t >>;
  start
  >>> fun () ->
    (* We use an extra monitor so we can specially handle errors in [f]. *)
    let saw_error = ref false in
    let monitor = Monitor.create ~name:"Clock.every'" () in
    Stream.iter (Monitor.errors monitor) ~f:(fun e ->
      Monitor.send_exn (Monitor.current ()) e;
      saw_error := true);
    let rec loop wait =
      upon (choose [choice stop (fun () -> `Stop);
                    choice wait (fun () -> `Continue)])
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
                    if Deferred.is_determined stop then
                      return `Stop
                    else
                      f () >>| fun () -> `Ran))
              >>> function
                | Ok z ->
                  begin match z with
                  | `Stop -> ()
                  | `Ran -> loop (after span)
                  end
                | Error error ->
                  Monitor.send_exn (Monitor.current ()) error;
                  if continue_on_error then loop (after span)
            end)
    in
    loop Deferred.unit
;;

let every ?start ?stop ?continue_on_error span f =
  every' ?start ?stop ?continue_on_error span (fun () -> f (); Deferred.unit)
;;

let with_timeout span d =
  choose [choice d (fun v -> `Result v);
          choice (after span) (fun _ -> `Timeout);
         ]
;;
