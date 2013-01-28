open Core.Std
open Import
open Deferred_std
module Stream = Async_stream
module Q = Queue

let show_debug_messages = ref false
let check_invariant = ref false

module Blocked_read = struct
  (* A [Blocked_read.t] represents a blocked read attempt.  If someone reads from an empty
     pipe, they enqueue a [Blocked_read.t] in the queue of [blocked_reads].  Later, when
     values are written to a pipe, that will cause some number of blocked reads to be
     filled, first come first serve.  The blocked-read constructor specifies how many
     values a read should consume from the pipe when it gets its turn.

     If a pipe is closed, then all blocked reads will be filled with [`Eof]. *)
  type 'a t =
  | Zero    of       [ `Eof | `Ok           ] Ivar.t
  | One     of       [ `Eof | `Ok of 'a     ] Ivar.t
  | All     of       [ `Eof | `Ok of 'a Q.t ] Ivar.t
  | At_most of int * [ `Eof | `Ok of 'a Q.t ] Ivar.t
  with sexp_of

  let invariant t =
    try
      match t with
      | Zero _ | All _ | One _ -> ()
      | At_most (i, _) -> assert (i > 0);
    with
    | exn ->
      failwiths "Pipe.Blocked_read.invariant failed" (exn, t) <:sexp_of< exn * a t >>
  ;;

  let is_empty = function
    | Zero        i  -> Ivar.is_empty i
    | One         i  -> Ivar.is_empty i
    | All         i  -> Ivar.is_empty i
    | At_most (_, i) -> Ivar.is_empty i
  ;;

  let fill_with_eof = function
    | Zero        i  -> Ivar.fill i `Eof
    | One         i  -> Ivar.fill i `Eof
    | All         i  -> Ivar.fill i `Eof
    | At_most (_, i) -> Ivar.fill i `Eof
end

module Blocked_flush = struct
  (* A [Blocked_flush.t] represents a blocked flush operation, which can be enabled by a
     future read.  If someone does [flushed p] on a pipe, that blocks until everything
     that's currently in the pipe at that point has drained out of the pipe.  When we call
     [flushed], it records the total amount of data that has been written so far in
     [fill_when_num_values_read].  We fill the [Flush.t] with [`Ok] when this amount of
     data has been read from the pipe.

     A [Blocked_flush.t] can also be filled with [`Reader_closed], which happens when the
     reader end of the pipe is closed, and we are thus sure that the unread elements
     preceding the flush will never be read. *)
  type t =
    { fill_when_num_values_read : int;
      ready : [ `Ok | `Reader_closed ] Ivar.t;
    }
  with fields, sexp_of

  let fill t v = Ivar.fill t.ready v
end

type ('a, 'phantom) t =
  { (* [id] is an integer used to distinguish pipes when debugging. *)
    id : int;
    (* [buffer] holds values written to the pipe that have not yet been read. *)
    buffer : 'a Q.t;
    (* [size_budget] governs pushback on writers to the pipe.

       There is *no* invariant that [Q.length buffer <= size_budget].  There is no hard
       upper bound on the number of elements that can be stuffed into the [buffer].  This
       is due to the way we handle writes.  When we do a write, all of the values written
       are immediately enqueued into [buffer].  After the write, if [Q.length buffer <=
       t.size_budget], then the writer will be notified to continue writing.  After the
       write, if [length t > t.size_budget], then the write will block until the pipe is
       under budget. *)
    mutable size_budget : int;
    (* [pushback] is used to give feedback to writers about whether they should write to
       the pipe.  [pushback] is full iff [length t <= t.size_budget || is_closed t]. *)
    mutable pushback : unit Ivar.t;
    (* [num_values_read] keeps track of the total number of values that have been read
       from the pipe.  We do not have to worry about overflow in [num_values_read].  You'd
       need to write 2^62 elements to the pipe, which would take about 146 years, at a
       flow rate of 1 size-unit/nanosecond. *)
    mutable num_values_read : int;
    (* [blocked_flushes] holds flushes whose preceding elements have not been completely
       read.  For each blocked flush, the number of elements that need to be read from the
       pipe in order to fill the flush is:

       fill_when_num_values_read - num_values_read

       Keeping the data in this form allows us to change a single field (num_values_read)
       when we consume values instead of having to iterate over the whole queue of
       flushes. *)
    blocked_flushes : Blocked_flush.t Q.t;
    (* [blocked_reads] holds reads that are waiting on data to be written to the pipe. *)
    blocked_reads : 'a Blocked_read.t Q.t;
    (* [closed] is filled when we close the write end of the pipe. *)
    closed : unit Ivar.t;
  }
with fields, sexp_of

type ('a, 'phantom) pipe = ('a, 'phantom) t with sexp_of

let hash t = Hashtbl.hash t.id

let equal (t1 : (_, _) t) t2 = phys_equal t1 t2

let is_empty t = Q.is_empty t.buffer

let is_closed t = Ivar.is_full t.closed

let closed t = Ivar.read t.closed

let pushback t = Ivar.read t.pushback

let length t = Q.length t.buffer

let invariant t =
  try
    assert (t.size_budget >= 0);
    assert (Ivar.is_full t.pushback = (length t <= t.size_budget || is_closed t));
    Q.iter t.blocked_flushes ~f:(fun f ->
      assert (f.Blocked_flush.fill_when_num_values_read > t.num_values_read));
    assert (List.is_sorted ~compare
              (List.map (Q.to_list t.blocked_flushes)
                 ~f:Blocked_flush.fill_when_num_values_read));
    if is_empty t then assert (Q.is_empty t.blocked_flushes);
    (* If data is available, no one is waiting for it.  This would need to change if we
       ever implement [read_exactly] as an atomic operation. *)
    if not (is_empty t) then assert (Q.is_empty t.blocked_reads);
    Q.iter t.blocked_reads ~f:(fun read ->
      Blocked_read.invariant read;
      assert (Blocked_read.is_empty read));
    (* You never block trying to read a closed pipe. *)
    if is_closed t then assert (Q.is_empty t.blocked_reads);
  with
  | exn ->
    failwiths "Pipe.invariant failed" (exn, t) <:sexp_of< exn * (a, b) t >>
;;

module Reader = struct
  type phantom with sexp_of
  type 'a t = ('a, phantom) pipe with sexp_of
  let invariant = invariant
end

module Writer = struct
  type phantom with sexp_of
  type 'a t = ('a, phantom) pipe with sexp_of
  let invariant = invariant
end

let id_ref = ref 0

let create () =
  incr id_ref;
  let t =
    { id              = !id_ref       ;
      closed          = Ivar.create ();
      size_budget     = 0             ;
      pushback        = Ivar.create ();
      buffer          = Q.create ()   ;
      num_values_read = 0             ;
      blocked_flushes = Q.create ()   ;
      blocked_reads   = Q.create ()   ;
    }
  in
  Ivar.fill t.pushback (); (* initially, the pipe does not pushback *)
  if !check_invariant then invariant t;
  (t, t)
;;

let update_pushback t =
  if length t <= t.size_budget || is_closed t then
    Ivar.fill_if_empty t.pushback ()
  else if Ivar.is_full t.pushback then
    t.pushback <- Ivar.create ();
;;

let close t =
  if !show_debug_messages then Debug.log "close" t <:sexp_of< (a, b) t >>;
  if !check_invariant then invariant t;
  if not (is_closed t) then begin
    Ivar.fill t.closed ();
    if is_empty t then begin
      Q.iter  t.blocked_reads ~f:Blocked_read.fill_with_eof;
      Q.clear t.blocked_reads;
    end;
    update_pushback t;
  end;
;;

let close_read t =
  if !show_debug_messages then Debug.log "close_read" t <:sexp_of< (a, b) t >>;
  if !check_invariant then invariant t;
  Q.iter  t.blocked_flushes ~f:(fun flush -> Blocked_flush.fill flush `Reader_closed);
  Q.clear t.blocked_flushes;
  Q.clear t.buffer;
  update_pushback t; (* we just cleared the buffer, so may need to fill [t.pushback] *)
  close t;
;;

let rec fill_flushes t =
  match Q.peek t.blocked_flushes with
  | None -> ()
  | Some flush ->
    if t.num_values_read >= flush.Blocked_flush.fill_when_num_values_read then begin
      Blocked_flush.fill flush `Ok;
      ignore (Q.dequeue_exn t.blocked_flushes : Blocked_flush.t);
      fill_flushes t;
    end
;;

(* [consume_all t] reads all the elements in [t], in constant time. *)
let consume_all t =
  let result = Q.create () in
  t.num_values_read <- t.num_values_read + length t;
  Q.transfer ~src:t.buffer ~dst:result;
  fill_flushes t;
  update_pushback t;
  result
;;

let consume_one t =
  assert (length t >= 1);
  let result = Q.dequeue_exn t.buffer in
  t.num_values_read <- t.num_values_read + 1;
  fill_flushes t;
  update_pushback t;
  result;
;;

(* [consume_at_most t num_values] reads [min num_values (length t)] items.  It is an
   error if [is_empty t] or [num_values < 0]. *)
let consume_at_most t num_values =
  assert (num_values >= 0);
  if num_values >= length t then
    consume_all t (* fast because it can use [Q.transfer] *)
  else begin
    t.num_values_read <- t.num_values_read + num_values;
    fill_flushes t;
    let result = Q.create () in
    for i = 1 to num_values do
      Q.enqueue result (Q.dequeue_exn t.buffer);
    done;
    update_pushback t;
    result
  end
;;

let set_size_budget t size_budget =
  if size_budget < 0 then
    failwiths "negative size_budget" size_budget <:sexp_of< int >>;
  t.size_budget <- size_budget;
  update_pushback t;
;;

let fill_blocked_reads t =
  while not (Q.is_empty t.blocked_reads) && not (is_empty t) do
    let module R = Blocked_read in
    match Q.dequeue_exn t.blocked_reads with
    | R.Zero        ivar  -> Ivar.fill ivar  `Ok
    | R.One         ivar  -> Ivar.fill ivar (`Ok (consume_one t))
    | R.All         ivar  -> Ivar.fill ivar (`Ok (consume_all t))
    | R.At_most (n, ivar) -> Ivar.fill ivar (`Ok (consume_at_most t n))
  done;
;;

(* checks all invariants, calls a passed in f to handle a write, then updates reads and
   pushback *)
let start_write t =
  if !show_debug_messages then Debug.log "write" t <:sexp_of< (a, b) t >>;
  if !check_invariant then invariant t;
  if is_closed t then failwiths "write to closed pipe" t <:sexp_of< (a, b) t >>;
;;

let finish_write t =
  fill_blocked_reads t;
  update_pushback t;
;;

let write' t values =
  start_write t;
  Q.transfer ~src:values ~dst:t.buffer;
  finish_write t;
  pushback t;
;;

let write_no_pushback t value =
  start_write t;
  Q.enqueue t.buffer value;
  finish_write t;
;;

let write t value =
  write_no_pushback t value;
  pushback t
;;

let with_write t ~f =
  pushback t
  >>| fun () ->
  if is_closed t then
    `Closed
  else
    `Ok (f (fun x -> write_no_pushback t x))
;;

let start_read t label =
  if !show_debug_messages then Debug.log label t <:sexp_of< (a, b) t >>;
  if !check_invariant then invariant t;
;;

let read_now t =
  start_read t "read_now";
  if is_empty t then begin
    if is_closed t then
      `Eof
    else
      `Nothing_available
  end else begin
    assert (Q.is_empty t.blocked_reads);
    `Ok (consume_all t)
  end
;;

let clear t =
  match read_now t with
  | `Eof | `Nothing_available | `Ok _ -> ()
;;

let read' t =
  start_read t "read'";
  match read_now t with
  | (`Ok _ | `Eof) as r -> return r
  | `Nothing_available  ->
    Deferred.create (fun ivar ->
      Q.enqueue t.blocked_reads (Blocked_read.All ivar))
;;

let read t =
  start_read t "read";
  if is_empty t then begin
    if is_closed t then
      return `Eof
    else
      Deferred.create (fun ivar ->
        Q.enqueue t.blocked_reads (Blocked_read.One ivar))
  end else begin
    assert (Q.is_empty t.blocked_reads);
    return (`Ok (consume_one t))
  end
;;

let read_at_most t ~num_values =
  start_read t "read_at_most";
  if num_values <= 0
  then failwiths "Pipe.read_at_most num_values < 0" num_values <:sexp_of< int >>;
  if is_empty t
  then begin
    if is_closed t
    then return `Eof
    else
      Deferred.create (fun ivar ->
        Q.enqueue t.blocked_reads (Blocked_read.At_most (num_values, ivar)))
  end
  else begin
    assert (Q.is_empty t.blocked_reads);
    return (`Ok (consume_at_most t num_values))
  end
;;

let values_available t =
  start_read t "values_available";
  if not (is_empty t)
  then return `Ok
  else if is_closed t
  then return `Eof
  else
    Deferred.create (fun ivar ->
      Q.enqueue t.blocked_reads (Blocked_read.Zero ivar))
;;

(* [read_exactly t ~num_values] loops, getting you all [num_values] items, up to EOF. *)
let read_exactly t ~num_values =
  start_read t "read_exactly";
  if num_values <= 0 then
    failwiths "Pipe.read_exactly got num_values <= 0" num_values <:sexp_of< int >>;
  Deferred.create (fun finish ->
    let result = Q.create () in
    let rec loop () =
      let already_read = Q.length result in
      assert (already_read <= num_values);
      if already_read = num_values then
        Ivar.fill finish (`Exactly result)
      else begin
        read_at_most t ~num_values:(num_values - already_read)
        >>> function
        | `Eof -> Ivar.fill finish (if already_read = 0 then `Eof else `Fewer result)
        | `Ok q ->
          Q.transfer ~src:q ~dst:result;
          loop ();
      end
    in
    loop ())
;;

let flushed t =
  if is_empty t then
    return `Ok
  else
    (* [t] might be closed.  But the read end can't be closed, because if it were, then
       [t] would be empty.  If the write end is closed but not the read end, then we want
       to enqueue a blocked flush because the enqueued values may get read. *)
    Deferred.create (fun ready ->
      Q.enqueue t.blocked_flushes
        { Blocked_flush.
          fill_when_num_values_read = t.num_values_read + length t;
          ready;
        })
;;

let fold_gen t ~init ~f =
  if !check_invariant then invariant t;
  Deferred.create (fun finished ->
    let rec loop b =
      read' t >>> function
      | `Eof  -> Ivar.fill finished b
      | `Ok q -> f b q loop
    in
    loop init)
;;

let fold' t ~init ~f = fold_gen t ~init ~f:(fun b q loop -> f b q >>> loop)

let fold t ~init ~f = fold_gen t ~init ~f:(fun init q loop -> loop (Q.fold q ~init ~f))

let iter_gen t ~f = fold_gen t ~init:() ~f:(fun () q loop -> f q loop)

let drain t = iter_gen t ~f:(fun _ loop -> loop ())

let drain_and_count t = fold_gen t ~init:0 ~f:(fun sum q loop -> loop (sum + Q.length q))

let iter' t ~f = fold' t ~init:() ~f:(fun () q -> f q)

let iter t ~f = iter' t ~f:(fun q -> Deferred.Queue.iter q ~f)

let iter_without_pushback t ~f = iter_gen t ~f:(fun q loop -> Q.iter q ~f; loop ())

let read_all input =
  let result = Q.create () in
  iter_gen input ~f:(fun q loop -> Q.transfer ~src:q ~dst:result; loop ());
  >>| fun () ->
  result
;;

let to_list r = read_all r >>| Q.to_list

(* CR201206 dpowers: remove this *)
let to_stream t =
  Stream.create (fun tail ->
    iter_without_pushback t ~f:(fun x -> Tail.extend tail x)
    >>> fun () ->
    Tail.close_exn tail)
;;

(* The implementation of [of_stream] does as much batching as possible.  It grabs as many
   items as are available into an internal queue.  Once it has grabbed everything, it
   writes it to the pipe and then blocks waiting for the next element from the stream.

   There's no possibility that we'll starve the pipe reading an endless stream, just
   accumulating the elements into our private queue forever without ever writing them
   downstream to the pipe.  Why? because while we're running, the stream-producer *isn't*
   running -- there are no Async block points in the queue-accumulator loop.  So the
   queue-accumulator loop will eventually catch up to the current stream tail, at which
   point we'll do the pipe-write and then block on the stream... thus giving the
   stream-producer a chance to make more elements.

   One can't implement [of_stream] using [Stream.iter] or [Stream.iter'] because you
   need to be able to stop early when the consumer closes the pipe.  Also, using either
   of those would entail significantly more deferred overhead, whereas the below
   implementation uses a deferred only when it needs to wait for data from the stream. *)
(* CR201206 dpowers: remove this *)
let of_stream s =
  let r, w = create () in
  let q = Q.create () in
  let transfer () =
    if not (Q.is_empty q) then
      (* Can not pushback on the stream, so ignore the pushback on the pipe. *)
      whenever (write' w q);
  in
  let rec loop s =
    assert (not (is_closed w));
    let next_deferred = Stream.next s in
    match Deferred.peek next_deferred with
    | Some next -> loop_next next
    | None -> transfer (); upon next_deferred check_closed_loop_next
  and check_closed_loop_next next = if not (is_closed w) then loop_next next
  and loop_next = function
    | Stream.Nil -> transfer (); close w
    | Stream.Cons (x, s) -> Q.enqueue q x; loop s
  in
  loop s;
  r
;;

let transfer_gen input output ~f =
  if !check_invariant then begin
    invariant input;
    invariant output;
  end;
  Deferred.create (fun result ->
    let output_closed () =
      close_read input;
      Ivar.fill result ()
    in
    let rec loop () =
      choose [ choice (values_available input) (function `Eof | `Ok as x -> x);
               choice (closed output) (fun () -> `Output_closed);
             ]
      >>> function
      | `Output_closed -> output_closed ()
      | `Eof -> Ivar.fill result ()
      | `Ok ->
        match read_now input with
        | `Eof -> Ivar.fill result ()
        | `Nothing_available -> loop ()
        | `Ok inq -> f inq continue
    and continue outq =
      if is_closed output
      then output_closed ()
      else write' output outq >>> loop
    in
    loop ())
;;

let transfer'   input output ~f = transfer_gen input output ~f:(fun q k -> f q >>> k)
let transfer    input output ~f = transfer_gen input output ~f:(fun q k -> k (Q.map q ~f))
let transfer_id input output    = transfer_gen input output ~f:(fun q k -> k q)

let map_gen input ~f =
  let (result, output) = create () in
  upon (transfer_gen input output ~f) (fun () -> close output);
  result
;;

let map' input ~f = map_gen input ~f:(fun q k -> f q >>> k)
let map  input ~f = map_gen input ~f:(fun q k -> k (Q.map q ~f))

let filter_map' input ~f = map'    input ~f:(fun q -> Deferred.Queue.filter_map q ~f)
let filter_map  input ~f = map_gen input ~f:(fun q k -> k (Q.filter_map q ~f))

let filter input ~f = filter_map input ~f:(fun x -> if f x then Some x else None)

let of_list l =
  let reader, writer = create () in
  whenever (write' writer (Q.of_list l));
  close writer;
  reader
;;

let interleave inputs =
  if !check_invariant then List.iter inputs ~f:invariant;
  let (output, writer) = create () in
  upon
    (Deferred.List.iter inputs ~how:`Parallel ~f:(fun input -> transfer_id input writer))
    (fun () -> close writer);
  output
;;

let concat inputs =
  let r, w = create () in
  upon (Deferred.List.iter inputs ~f:(fun input -> transfer_id input w))
    (fun () -> close w);
  r
;;

TEST_MODULE = struct
  let () =
    check_invariant := true;
    show_debug_messages := false;
  ;;

  let stabilize = Scheduler.run_cycles_until_no_jobs_remain

  let read_result d = Q.to_list (Option.value_exn (Deferred.peek d))

  TEST_UNIT =
    List.iter (List.init 10 ~f:(fun i -> List.init i ~f:Fn.id))
      ~f:(fun l ->
        let reader = of_list l in
        upon (read_all reader) (fun q -> assert (Q.to_list q = l)));
  stabilize ();
  ;;

  (* ==================== close, close_read ==================== *)

  TEST_UNIT =
    let (reader, writer) = create () in
    assert (not (is_closed writer));
    close writer;
    assert (Deferred.is_determined (closed reader));
    assert (is_closed reader);
    assert (is_closed writer);
  ;;

  TEST_UNIT =
    let (reader, writer) = create () in
    assert (not (is_closed writer));
    close_read reader;
    assert (Deferred.is_determined (closed reader));
    assert (is_closed reader);
    assert (is_closed writer);
  ;;

  TEST_UNIT =
    let check_read read =
      let (reader, writer) = create () in
      let d = read reader in
      assert (Deferred.peek d = None);
      close writer;
      stabilize ();
      assert (Deferred.peek d = Some `Eof);
      let d = read reader in
      stabilize ();
      assert (Deferred.peek d = Some `Eof);
    in
    check_read read';
    check_read read;
    check_read (fun reader -> read_at_most reader ~num_values:1);
    check_read (fun reader -> read_exactly reader ~num_values:1);
    check_read values_available;
  ;;

  TEST_UNIT =
    let check_read read get_values =
      let (reader, writer) = create () in
      whenever (write writer 13);
      close writer;
      let d = read reader in
      stabilize ();
      match Deferred.peek d with
      | Some z -> assert ([13] = get_values z)
      | None -> assert false
    in
    check_read read' (function `Ok q -> Q.to_list q | _ -> assert false);
    check_read read (function `Ok a -> [a] | _ -> assert false);
    check_read (fun r -> read_at_most r ~num_values:1)
      (function `Ok q -> Q.to_list q | _ -> assert false);
    check_read (fun r -> read_exactly r ~num_values:1)
      (function `Exactly q -> Q.to_list q | _ -> assert false);
    check_read (fun r -> return (read_now r))
      (function `Ok q -> Q.to_list q | _ -> assert false);
    check_read read_all Q.to_list;
  ;;

  TEST_UNIT =
    let (reader, writer) = create () in
    let f1 = flushed writer in
    whenever (write writer 13);
    let f2 = flushed writer in
    close_read reader;
    stabilize ();
    assert (Deferred.peek f1 = Some `Ok);
    assert (Deferred.peek f2 = Some `Reader_closed);
  ;;

  (* ==================== pushback ==================== *)

  TEST_UNIT =
    let (_, writer) = create () in
    let p = write writer () in
    close writer;
    stabilize ();
    assert (Deferred.peek p = Some ());
  ;;

  TEST_UNIT =
    let (reader, writer) = create () in
    let p = write writer () in
    close_read reader;
    stabilize ();
    assert (Deferred.peek p = Some ());
  ;;

  TEST_UNIT =
    let (reader, writer) = create () in
    let p = write writer () in
    stabilize ();
    assert (Deferred.peek p = None);
    ignore (read_now reader);
    stabilize ();
    assert (Deferred.peek p = Some ());
  ;;

  TEST_UNIT =
    let (reader, writer) = create () in
    let p = write writer () in
    let _ = write writer () in
    assert (length writer = 2);
    stabilize ();
    assert (Deferred.peek p = None);
    ignore (read reader);
    stabilize ();
    assert (length writer = 1);
    assert (Deferred.peek p = None);
    ignore (read reader);
    stabilize ();
    assert (length writer = 0);
    assert (Deferred.peek p = Some ());
  ;;

  (* ==================== read_all ==================== *)


  TEST_UNIT =
    let (reader, writer) = create () in
    close writer;
    let d = read_all reader in
    stabilize ();
    assert (read_result d = []);
  ;;

  TEST_UNIT =
    let (reader, writer) = create () in
    whenever (write writer 13);
    close writer;
    let d = read_all reader in
    stabilize ();
    assert (read_result d = [13]);
  ;;

  (* ==================== read_at_most ==================== *)

  TEST_UNIT =
    let (reader, writer) = create () in
    whenever (write' writer (Q.of_list [12; 13; 14]));
    close writer;
    let d =
      read_at_most reader ~num_values:2
      >>| function
      | `Eof -> assert false
      | `Ok q -> q
    in
    stabilize ();
    assert (read_result d = [12; 13]);
  ;;

  TEST_UNIT =
    let (reader, writer) = create () in
    whenever (write' writer (Q.of_list [12; 13; 14]));
    close writer;
    let d =
      read_at_most reader ~num_values:4
      >>| function
      | `Eof -> assert false
      | `Ok q -> q
    in
    stabilize ();
    assert (read_result d = [12; 13; 14]);
  ;;

  (* ==================== clear ==================== *)

  TEST_UNIT =
    let l = [ 12; 13 ] in
    let (reader, writer) = create () in
    let p = write' writer (Q.of_list l) in
    clear reader;
    stabilize ();
    assert (Deferred.peek p = Some ());
    assert (length reader = 0);
    whenever (write' writer (Q.of_list l));
    close writer;
    let d = read_all reader in
    stabilize ();
    assert (read_result d = l);
  ;;

  (* ==================== map ==================== *)

  TEST_UNIT =
    let (reader, writer) = create () in
    let reader = map reader ~f:(fun x -> x + 13) in
    whenever (write' writer (Q.of_list [ 1; 2; 3 ]));
    let d =
      read_at_most reader ~num_values:2
      >>| function
      | `Eof -> assert false
      | `Ok q -> close_read reader; q
    in
    stabilize ();
    assert (is_closed writer);
    assert (read_result d = [ 14; 15 ]);
  ;;

  (* ==================== of_stream ==================== *)

  TEST_UNIT =
    let tail = Tail.create () in
    let pipe = of_stream (Tail.collect tail) in
    stabilize ();
    assert (length pipe = 0);
    Tail.extend tail 13;
    stabilize ();
    assert (length pipe = 1);
    Tail.extend tail 14;
    Tail.extend tail 15;
    stabilize ();
    assert (length pipe = 3);
    let d = read_all pipe in
    Tail.close_exn tail;
    stabilize ();
    assert (read_result d = [ 13; 14; 15 ]);
  ;;

  (* ==================== interleave ==================== *)

  TEST_UNIT =
    let t = interleave [] in
    let d = read_all t in
    stabilize ();
    assert (read_result d = []);
  ;;

  TEST_UNIT =
    let l = [ 1 ; 2; 3 ] in
    let t = interleave [ of_list l ] in
    let d = read_all t in
    stabilize ();
    assert (read_result d = l);
  ;;

  TEST_UNIT =
    let l = [ 1 ; 2; 3 ] in
    let t = interleave [ of_list l; of_list l ] in
    let d = read_all t in
    stabilize ();
    assert (List.length (read_result d) = 2 * List.length l);
  ;;

end
;;

