open Core.Std

module Scheduler = Raw_scheduler

let debug = Debug.clock

open Raw_clock_event.T

type t = Execution_context.t Raw_clock_event.t with sexp_of

let events () = Scheduler.((t ()).events)

let status t =
  match t.state with
  | Uninitialized -> assert false
  | Aborted -> `Aborted
  | Happened -> `Happened
  | Waiting _ -> `Waiting
;;

let fire t =
  if debug then Debug.log "Clock_event.fire" t <:sexp_of< t >>;
  match t.state with
  | Uninitialized | Aborted | Happened -> failwiths "Event.fire" t <:sexp_of< t >>
  | Waiting waiting ->
    Ivar.fill (Ivar.of_raw waiting.ready) `Happened;
    t.state <- Happened;
;;

let abort t =
  if debug then Debug.log "Clock_event.abort" t <:sexp_of< t >>;
  match t.state with
  | Uninitialized -> assert false
  | Aborted -> `Previously_aborted
  | Happened -> `Previously_happened
  | Waiting waiting ->
    t.state <- Aborted;
    begin match Events.remove (events ()) waiting.event with
    | `Not_present -> assert false
    | `Removed -> ()
    end;
    `Ok
;;

let at time =
  let ready = Ivar.create () in
  let t = { state = Uninitialized } in
  begin match Events.add (events ()) ~at:time t with
  | `Not_in_the_future ->
    t.state <- Happened;
    Ivar.fill ready `Happened;
  | `Ok events_event ->
    t.state <- Waiting { event = events_event; ready = Ivar.to_raw ready };
  end;
  if debug then Debug.log "Clock_event.at" (time, t) <:sexp_of< Time.t * t >>;
  (t, Ivar.read ready)
;;
