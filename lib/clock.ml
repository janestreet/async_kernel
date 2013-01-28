open Core.Std
open Import
open Deferred_std

module Stream = Async_stream
module Event = Clock_event

open Monitor.Exported_for_scheduler

let can_not_abort (d, _event) = d >>| function `Aborted -> assert false | `Happened -> ()

let at_event time = let (d, e) = Event.at time in (e, d)

let at time = can_not_abort (at_event time)

let after_event span =
  (* We use [Time.now()] rather than [cycle_start] for the base time.  There can be
     substantial difference between the two when people do long running computations or
     mix blocking code with async.  And humans expect that [after] is based on the current
     time, not some artifact of async implementation. *)
  at_event (Time.add (Time.now ()) span)
;;

let after span = can_not_abort (after_event span)

let at_varying_intervals ?(stop = Deferred.never ()) compute_span =
  let m = Tail.create () in
  let rec loop () =
    upon
      (choose
         [choice stop (fun () -> `Stop);
          choice (at (Time.add (Time.now ()) (compute_span ())))
            (fun () -> `Tick)])
      (function
        | `Stop -> ()
        | `Tick ->
          Tail.extend m ();
          loop ())
  in
  loop ();
  Tail.collect m;
;;

let at_intervals ?stop span = at_varying_intervals ?stop (fun () -> span)

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
            (* The "raise_rest" part of a prior call to [try_with_raise_rest f] could have
               raised an error after having returned ok.  We check [saw_error] and don't
               proceed if it did. *)
            if continue_on_error || not !saw_error then begin
              within' ~monitor (fun () ->
                Monitor.try_with_raise_rest
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
