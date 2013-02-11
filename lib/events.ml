open Core.Std

let debug = Debug.clock

module Event = struct
  type 'a t =
    { at : Time.t;
      value : 'a;
      mutable heap_element : 'a t sexp_opaque Heap.heap_el option;
    }
  with fields, sexp_of

  let compare t1 t2 = Time.compare t1.at t2.at
end

type 'a t =
  { events : 'a Event.t Heap.t;
    mutable now : Time.t;
  }
with fields, sexp_of

let is_empty t = Heap.is_empty t.events

let is_ready t event = Time.(event.Event.at <= t.now)

let invariant t =
  try
    Heap.iter t.events ~f:(fun event ->
      begin match event.Event.heap_element with
      | None -> assert false
      | Some heap_el -> assert (Heap.heap_el_mem t.events heap_el)
      end;
      assert (not (is_ready t event)));
  with exn ->
    failwiths "invariant failed" (exn, t) <:sexp_of< exn * _ t >>
;;

let create ~now =
  { events = Heap.create Event.compare;
    now;
  }
;;

let iter t ~f = Heap.iter t.events ~f

let next_upcoming t = Heap.top t.events

let advance_clock t ~to_ =
  if debug then Debug.log "Events.advance_clock" (to_, t) <:sexp_of< Time.t * _ t >>;
  if Time.(to_ <= t.now) then
    `Not_in_the_future
  else begin
    t.now <- to_;
    let rec loop ac =
      match Heap.cond_pop t.events (fun event -> is_ready t event) with
      | None -> ac
      | Some event ->
        event.Event.heap_element <- None;
        loop (event.Event.value :: ac)
    in
    `Ok (loop [])
  end
;;

let add t ~at value =
  if debug then Debug.log "Events.add" (at, t) <:sexp_of< Time.t * _ t >>;
  if Time.(at <= t.now) then
    `Not_in_the_future
  else begin
    let event =
      { Event.
        at;
        value;
        heap_element = None;
      }
    in
    event.Event.heap_element <- Some (Heap.push t.events event);
    `Ok event
  end
;;

let remove t event =
  if debug then Debug.log "Events.remove" (event, t) <:sexp_of< _ Event.t * _ t >>;
  match event.Event.heap_element with
  | None -> `Not_present
  | Some heap_element ->
    if not (Heap.heap_el_mem t.events heap_element) then
      `Not_present
    else begin
      event.Event.heap_element <- None;
      Heap.remove heap_element;
      `Removed
    end
;;
