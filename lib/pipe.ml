open Core.Std
open Import
open Deferred_std
module Stream = Async_stream
module Q = Queue

let show_debug_messages = ref false
let check_invariant = ref false

module Flushed_result = struct
  type t = [ `Ok | `Reader_closed ] with sexp_of

  let combine (l : t Deferred.t list) =
    Deferred.all l
    >>| fun l ->
    match List.mem l `Reader_closed with
    | true  -> `Reader_closed
    | false -> `Ok
  ;;
end

(* A [Consumer.t] acts as the monitor of some process that reads values from a pipe and
   processes them, allowing that process:
   - to communicate that it has taken responsibility for the values
   - to signal when it has finished with the values to interested parties (via
   [downstream_flushed])

   It is used in two steps:

   1. calling [Consumer.start] at the point where the consumer takes values out of the
   Pipe via [read] or [read'].
   2. calling [Consumer.values_sent_downstream].

   If no [Consumer.t] is supplied when a value is read then the value is defined to be
   flushed at that time. *)
module Consumer : sig
  type t with sexp_of

  include Invariant.S with type t := t

  val create
    :  pipe_id:int
    -> downstream_flushed:(unit -> Flushed_result.t Deferred.t)
    -> t
  val pipe_id : t -> int
  val start : t -> unit
  val values_sent_downstream : t -> unit
  val values_sent_downstream_and_flushed : t -> Flushed_result.t Deferred.t
end = struct
  type t =
    { pipe_id             : int
    (* [values_read] reflects whether values the consumer has read from the pipe have been
       sent downstream or if not, holds an ivar that is to be filled when they are. *)
    ; mutable values_read : [ `Have_been_sent_downstream
                            | `Have_not_been_sent_downstream of unit Ivar.t
                            ]
    (* [downstream_flushed ()] returns when all prior values that the consumer has
       passed downstream have been flushed all the way down the chain of pipes. *)
    ; downstream_flushed  : unit -> Flushed_result.t Deferred.t
    }
  with fields, sexp_of

  let invariant t : unit =
    try
      let check f field = f (Field.get field t) in
      Fields.iter
        ~pipe_id:ignore
        ~values_read:(check (function
          | `Have_been_sent_downstream -> ()
          | `Have_not_been_sent_downstream ivar -> assert (Ivar.is_empty ivar)))
        ~downstream_flushed:ignore;
    with exn ->
      failwiths "Pipe.Consumer.invariant failed" (exn, t) <:sexp_of< exn * t >>
  ;;

  let create ~pipe_id ~downstream_flushed =
    { pipe_id
    ; values_read        = `Have_been_sent_downstream
    ; downstream_flushed
    }
  ;;

  let start t =
    match t.values_read with
    | `Have_not_been_sent_downstream _ -> ()
    | `Have_been_sent_downstream ->
      t.values_read <- `Have_not_been_sent_downstream (Ivar.create ())
  ;;

  let values_sent_downstream t =
    match t.values_read with
    | `Have_been_sent_downstream -> ()
    | `Have_not_been_sent_downstream ivar ->
      Ivar.fill ivar ();
      t.values_read <- `Have_been_sent_downstream;
  ;;

  let values_sent_downstream_and_flushed t =
    match t.values_read with
    | `Have_been_sent_downstream -> t.downstream_flushed ()
    | `Have_not_been_sent_downstream when_sent_downstream ->
      Ivar.read when_sent_downstream
      >>= fun () ->
      t.downstream_flushed ()
  ;;
end

module Blocked_read = struct
  (* A [Blocked_read.t] represents a blocked read attempt.  If someone reads from an empty
     pipe, they enqueue a [Blocked_read.t] in the queue of [blocked_reads].  Later, when
     values are written to a pipe, that will cause some number of blocked reads to be
     filled, first come first serve.  The blocked-read constructor specifies how many
     values a read should consume from the pipe when it gets its turn.

     If a pipe is closed, then all blocked reads will be filled with [`Eof]. *)
  type 'a wants =
    | Zero    of       [ `Eof | `Ok           ] Ivar.t
    | One     of       [ `Eof | `Ok of 'a     ] Ivar.t
    | All     of       [ `Eof | `Ok of 'a Q.t ] Ivar.t
    | At_most of int * [ `Eof | `Ok of 'a Q.t ] Ivar.t
  with sexp_of

  type 'a t =
    { wants : 'a wants
    ; consumer : Consumer.t option
    }
  with fields, sexp_of

  let invariant t : unit =
    try
      let check f field = f (Field.get field t) in
      Fields.iter
        ~wants:(check (function
          | Zero _ | All _ | One _ -> ()
          | At_most (i, _) -> assert (i > 0)))
        ~consumer:(check (function
          | None -> ()
          | Some consumer -> Consumer.invariant consumer));
    with exn ->
      failwiths "Pipe.Blocked_read.invariant failed" (exn, t) <:sexp_of< exn * _ t >>
  ;;

  let create wants consumer = { wants; consumer }

  let is_empty t =
    match t.wants with
    | Zero        i  -> Ivar.is_empty i
    | One         i  -> Ivar.is_empty i
    | All         i  -> Ivar.is_empty i
    | At_most (_, i) -> Ivar.is_empty i
  ;;

  let fill_with_eof t =
    match t.wants with
    | Zero        i  -> Ivar.fill i `Eof
    | One         i  -> Ivar.fill i `Eof
    | All         i  -> Ivar.fill i `Eof
    | At_most (_, i) -> Ivar.fill i `Eof
  ;;
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
    { fill_when_num_values_read : int
    ; ready : [ `Ok | `Reader_closed ] Ivar.t
    }
  with fields, sexp_of

  let fill t v = Ivar.fill t.ready v
end

type ('a, 'phantom) t =
  { (* [id] is an integer used to distinguish pipes when debugging. *)
    id                        : int
  (* [buffer] holds values written to the pipe that have not yet been read. *)
  ; mutable buffer            : 'a Q.t
  (* [size_budget] governs pushback on writers to the pipe.

     There is *no* invariant that [Q.length buffer <= size_budget].  There is no hard
     upper bound on the number of elements that can be stuffed into the [buffer].  This
     is due to the way we handle writes.  When we do a write, all of the values written
     are immediately enqueued into [buffer].  After the write, if [Q.length buffer <=
     t.size_budget], then the writer will be notified to continue writing.  After the
     write, if [length t > t.size_budget], then the write will block until the pipe is
     under budget. *)
  ; mutable size_budget       : int
  (* [pushback] is used to give feedback to writers about whether they should write to
     the pipe.  [pushback] is full iff [length t <= t.size_budget || is_closed t]. *)
  ; mutable pushback          : unit Ivar.t
  (* [num_values_read] keeps track of the total number of values that have been read
     from the pipe.  We do not have to worry about overflow in [num_values_read].  You'd
     need to write 2^62 elements to the pipe, which would take about 146 years, at a
     flow rate of 1 size-unit/nanosecond. *)
  ; mutable num_values_read   : int
  (* [blocked_flushes] holds flushes whose preceding elements have not been completely
     read.  For each blocked flush, the number of elements that need to be read from the
     pipe in order to fill the flush is                        :

     fill_when_num_values_read - num_values_read

     Keeping the data in this form allows us to change a single field(num_values_read)
     when we consume values instead of having to iterate over the whole queue of
     flushes. *)
  ; blocked_flushes           : Blocked_flush.t Q.t
  (* [blocked_reads] holds reads that are waiting on data to be written to the pipe. *)
  ; blocked_reads             : 'a Blocked_read.t Q.t
  (* [closed] is filled when we close the write end of the pipe. *)
  ; closed                    : unit Ivar.t
  (* [read_closed] is filled when we close the read end of the pipe. *)
  ; read_closed               : unit Ivar.t

  (* [consumers] is a list of all consumers that may be handling values read from the
     pipe. *)
  ; mutable consumers         : Consumer.t list
  (* [upstream_flusheds] has a function for each pipe immediately upstream of this one.
     That function walks to the head(s) of the upstream pipe, and calls
     [downstream_flushed] on the head(s).  See the definition of [upstream_flushed]
     below. *)
  ; mutable upstream_flusheds : (unit -> Flushed_result.t Deferred.t) list
  }
with fields, sexp_of

type ('a, 'phantom) pipe = ('a, 'phantom) t with sexp_of

let hash t = Hashtbl.hash t.id

let equal (t1 : (_, _) t) t2 = phys_equal t1 t2

let is_closed t = Ivar.is_full t.closed

let is_read_closed t = Ivar.is_full t.read_closed

let closed t = Ivar.read t.closed

let pushback t = Ivar.read t.pushback

let length t = Q.length t.buffer

let is_empty t = length t = 0

let invariant t : unit =
  try
    let check f = fun field -> f (Field.get field t) in
    Fields.iter
      ~id:ignore
      ~buffer:ignore
      ~size_budget:(check (fun size_budget -> assert (size_budget >= 0)))
      ~pushback:(check (fun pushback ->
        assert (Ivar.is_full pushback = (length t <= t.size_budget || is_closed t))))
      ~num_values_read:ignore
      ~blocked_flushes:(check (fun blocked_flushes ->
        Q.iter blocked_flushes ~f:(fun (f : Blocked_flush.t) ->
          assert (f.fill_when_num_values_read > t.num_values_read));
        assert (List.is_sorted ~compare
                  (List.map (Q.to_list blocked_flushes)
                     ~f:Blocked_flush.fill_when_num_values_read));
        if is_empty t then assert (Q.is_empty blocked_flushes)))
      ~blocked_reads:(check (fun blocked_reads ->
        (* If data is available, no one is waiting for it.  This would need to change if
           we ever implement [read_exactly] as an atomic operation. *)
        if not (is_empty t) then assert (Q.is_empty blocked_reads);
        Q.iter blocked_reads ~f:(fun read ->
          Blocked_read.invariant read;
          assert (Blocked_read.is_empty read));
        (* You never block trying to read a closed pipe. *)
        if is_closed t then assert (Q.is_empty blocked_reads)))
      ~closed:ignore
      ~read_closed:ignore
      ~consumers:(check (fun l ->
        List.iter l ~f:(fun consumer ->
          Consumer.invariant consumer;
          assert (Consumer.pipe_id consumer = t.id))))
      ~upstream_flusheds:ignore
  with exn ->
    failwiths "Pipe.invariant failed" (exn, t) <:sexp_of< exn * (_, _) t >>
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
    { id                = !id_ref
    ; closed            = Ivar.create ()
    ; read_closed       = Ivar.create ()
    ; size_budget       = 0
    ; pushback          = Ivar.create ()
    ; buffer            = Q.create    ()
    ; num_values_read   = 0
    ; blocked_flushes   = Q.create    ()
    ; blocked_reads     = Q.create    ()
    ; consumers         = []
    ; upstream_flusheds = []
    }
  in
  Ivar.fill t.pushback (); (* initially, the pipe does not pushback *)
  if !check_invariant then invariant t;
  (t, t)
;;

let update_pushback t =
  if length t <= t.size_budget || is_closed t
  then Ivar.fill_if_empty t.pushback ()
  else if Ivar.is_full t.pushback
  then t.pushback <- Ivar.create ();
;;

let close t =
  if !show_debug_messages then eprints "close" t <:sexp_of< (_, _) t >>;
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

let init f =
  let r, w = create () in
  don't_wait_for (Monitor.protect (fun () -> f w)
                    ~finally:(fun () -> close w; Deferred.unit));
  r
;;

let close_read t =
  if !show_debug_messages then eprints "close_read" t <:sexp_of< (_, _) t >>;
  if !check_invariant then invariant t;
  if not (is_read_closed t) then begin
    Ivar.fill t.read_closed ();
    Q.iter  t.blocked_flushes ~f:(fun flush -> Blocked_flush.fill flush `Reader_closed);
    Q.clear t.blocked_flushes;
    Q.clear t.buffer;
    update_pushback t; (* we just cleared the buffer, so may need to fill [t.pushback] *)
    close t;
  end;
;;

let values_were_read t consumer =
  Option.iter consumer ~f:Consumer.start;
  let rec loop () =
    match Q.peek t.blocked_flushes with
    | None -> ()
    | Some flush ->
      if t.num_values_read >= flush.fill_when_num_values_read then begin
        ignore (Q.dequeue_exn t.blocked_flushes : Blocked_flush.t);
        begin match consumer with
        | None -> Blocked_flush.fill flush `Ok;
        | Some consumer ->
          upon (Consumer.values_sent_downstream_and_flushed consumer) (fun flush_result ->
            Blocked_flush.fill flush flush_result);
        end;
        loop ();
      end
  in
  loop ();
;;

(* [consume_all t] reads all the elements in [t]. *)
let consume_all t consumer =
  let result = t.buffer in
  t.buffer <- Q.create ();
  t.num_values_read <- t.num_values_read + Q.length result;
  values_were_read t consumer;
  update_pushback t;
  result
;;

let consume_one t consumer =
  assert (length t >= 1);
  let result = Q.dequeue_exn t.buffer in
  t.num_values_read <- t.num_values_read + 1;
  values_were_read t consumer;
  update_pushback t;
  result
;;

(* [consume_at_most t num_values] reads [min num_values (length t)] items.  It is an
   error if [is_empty t] or [num_values < 0]. *)
let consume_at_most t num_values consumer =
  assert (num_values >= 0);
  if num_values >= length t
  then consume_all t consumer
  else begin
    t.num_values_read <- t.num_values_read + num_values;
    values_were_read t consumer;
    let result = Q.create ~capacity:num_values () in
    Q.blit_transfer ~src:t.buffer ~dst:result ~len:num_values ();
    update_pushback t;
    result
  end
;;

let set_size_budget t size_budget =
  if size_budget < 0
  then failwiths "negative size_budget" size_budget <:sexp_of< int >>;
  t.size_budget <- size_budget;
  update_pushback t;
;;

let fill_blocked_reads t =
  while not (Q.is_empty t.blocked_reads) && not (is_empty t) do
    let blocked_read = Q.dequeue_exn t.blocked_reads in
    let consumer = blocked_read.consumer in
    match blocked_read.wants with
    | Zero        ivar  -> Ivar.fill ivar  `Ok
    | One         ivar  -> Ivar.fill ivar (`Ok (consume_one     t   consumer))
    | All         ivar  -> Ivar.fill ivar (`Ok (consume_all     t   consumer))
    | At_most (n, ivar) -> Ivar.fill ivar (`Ok (consume_at_most t n consumer))
  done;
;;

(* checks all invariants, calls a passed in f to handle a write, then updates reads and
   pushback *)
let start_write t =
  if !show_debug_messages then eprints "write" t <:sexp_of< (_, _) t >>;
  if !check_invariant then invariant t;
  if is_closed t then failwiths "write to closed pipe" t <:sexp_of< (_, _) t >>;
;;

let finish_write t =
  fill_blocked_reads t;
  update_pushback t;
;;

let write_without_pushback' t values =
  start_write t;
  Q.blit_transfer ~src:values ~dst:t.buffer ();
  finish_write t;
;;

let write' t values =
  write_without_pushback' t values;
  pushback t;
;;

let write_without_pushback t value =
  start_write t;
  Q.enqueue t.buffer value;
  finish_write t;
;;

let write t value =
  write_without_pushback t value;
  pushback t;
;;

let write_when_ready t ~f =
  pushback t
  >>| fun () ->
  if is_closed t
  then `Closed
  else `Ok (f (fun x -> write_without_pushback t x))
;;

let ensure_consumer_matches ?consumer t =
  match consumer with
  | None -> ()
  | Some consumer ->
    if t.id <> Consumer.pipe_id consumer
    then failwiths "Attempt to use consumer with wrong pipe" (consumer, t)
           <:sexp_of< Consumer.t * _ Reader.t >>
;;

let start_read ?consumer t label =
  if !show_debug_messages then eprints label t <:sexp_of< (_, _) t >>;
  if !check_invariant then invariant t;
  ensure_consumer_matches t ?consumer;
;;

let gen_read_now ?consumer t consume =
  start_read t "read_now" ?consumer;
  if is_empty t then begin
    if is_closed t
    then `Eof
    else `Nothing_available
  end else begin
    assert (Q.is_empty t.blocked_reads); (* from [invariant] and [not (is_empty t)] *)
    `Ok (consume t consumer)
  end
;;

let read_now' ?consumer t = gen_read_now t ?consumer consume_all
let read_now  ?consumer t = gen_read_now t ?consumer consume_one

let peek t = Queue.peek t.buffer

let clear t =
  match read_now' t with
  | `Eof | `Nothing_available | `Ok _ -> ()
;;

let read' ?consumer t =
  start_read t "read'" ?consumer;
  match read_now' t ?consumer with
  | (`Ok _ | `Eof) as r -> return r
  | `Nothing_available  ->
    Deferred.create (fun ivar ->
      Q.enqueue t.blocked_reads (Blocked_read.(create (All ivar)) consumer))
;;

let read ?consumer t =
  start_read t "read" ?consumer;
  if is_empty t then begin
    if is_closed t then
      return `Eof
    else
      Deferred.create (fun ivar ->
        Q.enqueue t.blocked_reads (Blocked_read.(create (One ivar)) consumer))
  end else begin
    assert (Q.is_empty t.blocked_reads);
    return (`Ok (consume_one t consumer))
  end
;;

let read_at_most ?consumer t ~num_values =
  start_read t "read_at_most" ?consumer;
  if num_values <= 0
  then failwiths "Pipe.read_at_most num_values < 0" num_values <:sexp_of< int >>;
  if is_empty t then begin
    if is_closed t
    then return `Eof
    else
      Deferred.create (fun ivar ->
        Q.enqueue t.blocked_reads
          (Blocked_read.(create (At_most (num_values, ivar))) consumer))
  end else begin
    assert (Q.is_empty t.blocked_reads);
    return (`Ok (consume_at_most t num_values consumer))
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
      Q.enqueue t.blocked_reads (Blocked_read.(create (Zero ivar)) None))
;;

(* [read_exactly t ~num_values] loops, getting you all [num_values] items, up to EOF. *)
let read_exactly ?consumer t ~num_values =
  start_read t "read_exactly" ?consumer;
  if num_values <= 0
  then failwiths "Pipe.read_exactly got num_values <= 0" num_values <:sexp_of< int >>;
  Deferred.create (fun finish ->
    let result = Q.create () in
    let rec loop () =
      let already_read = Q.length result in
      assert (already_read <= num_values);
      if already_read = num_values
      then Ivar.fill finish (`Exactly result)
      else begin
        read_at_most ?consumer t ~num_values:(num_values - already_read)
        >>> function
        | `Eof -> Ivar.fill finish (if already_read = 0 then `Eof else `Fewer result)
        | `Ok q ->
          Q.blit_transfer ~src:q ~dst:result ();
          loop ();
      end
    in
    loop ())
;;

let downstream_flushed t =
  if is_empty t
  then
    if List.is_empty t.consumers
    then return `Ok
    else Flushed_result.combine (List.map t.consumers
                                   ~f:Consumer.values_sent_downstream_and_flushed)
  else
    (* [t] might be closed.  But the read end can't be closed, because if it were, then
       [t] would be empty.  If the write end is closed but not the read end, then we want
       to enqueue a blocked flush because the enqueued values may get read. *)
    Deferred.create (fun ready ->
      Q.enqueue t.blocked_flushes
        { fill_when_num_values_read = t.num_values_read + length t
        ; ready
        })
;;

(* In practice, along with [link] and [add_upstream_flushed], [upstream_flushed] traverses
   the graph of linked pipes up to the heads and then calls [downstream_flushed] on
   them. *)
let upstream_flushed t =
  if List.is_empty t.upstream_flusheds
  then downstream_flushed t
  else Flushed_result.combine (List.map t.upstream_flusheds ~f:(fun f -> f ()))
;;

let add_upstream_flushed t upstream_flushed =
  t.upstream_flusheds <- upstream_flushed :: t.upstream_flusheds;
;;

let add_consumer t ~downstream_flushed =
  let consumer = Consumer.create ~pipe_id:t.id ~downstream_flushed in
  t.consumers <- consumer :: t.consumers;
  consumer;
;;

(* [link ~upstream ~downstream] links flushing of two pipes together. *)
let link ~upstream ~downstream =
  add_upstream_flushed downstream (fun () -> upstream_flushed upstream);
  add_consumer upstream ~downstream_flushed:(fun () -> downstream_flushed downstream);
;;

type ('a, 'b, 'c, 'accum) fold =
  ?consumer:Consumer.t
  -> 'a Reader.t
  -> init:'accum
  -> f:('accum -> 'b -> 'c)
  -> 'accum Deferred.t

let fold_gen (read_now : ?consumer:Consumer.t -> _ Reader.t -> _) ?consumer t ~init ~f =
  if !check_invariant then invariant t;
  ensure_consumer_matches t ?consumer;
  Deferred.create (fun finished ->
    (* We do [Deferred.unit >>>] to ensure that [f] is only called asynchronously. *)
    Deferred.unit
    >>> fun () ->
    let rec loop b =
      match read_now t ?consumer with
      | `Eof -> Ivar.fill finished b
      | `Ok v -> f b v loop
      | `Nothing_available -> values_available t >>> fun _ -> loop b
    in
    loop init)
;;

let fold' ?consumer t ~init ~f =
  fold_gen read_now' ?consumer t ~init ~f:(fun b q loop -> f b q >>> loop)
;;

let fold ?consumer t ~init ~f =
  fold_gen read_now  ?consumer t ~init ~f:(fun b a loop -> f b a >>> loop)
;;

let fold_without_pushback ?consumer t ~init ~f =
  fold_gen read_now  ?consumer t ~init ~f:(fun b a loop -> loop (f b a))
;;

type ('a, 'b, 'c) iter =
  ?consumer:Consumer.t
  -> ?continue_on_error:bool (** default is [false] *)
  -> 'a Reader.t
  -> f:('b -> 'c)
  -> unit Deferred.t

let with_error_to_current_monitor ?(continue_on_error = false) f a =
  if not continue_on_error
  then f a
  else begin
    Monitor.try_with (fun () -> f a)
    >>| function
    | Ok () -> ()
    | Error exn -> Monitor.send_exn (Monitor.current ()) (Monitor.extract_exn exn)
  end;
;;

let iter' ?consumer ?continue_on_error t ~f =
  fold' ?consumer t ~init:() ~f:(fun () q ->
    with_error_to_current_monitor ?continue_on_error f q)
;;

let iter ?consumer ?continue_on_error t ~f =
  fold_gen read_now ?consumer t ~init:() ~f:(fun () a loop ->
    with_error_to_current_monitor ?continue_on_error f a
    >>> fun () ->
    loop ())
;;

(* [iter_without_pushback] is a common case, so we implement it in an optimized manner,
   rather than via [iter].  The implementation reads only one element at a time, so that
   if [f] closes [t] or raises, no more elements will be read. *)
let iter_without_pushback ?consumer ?(continue_on_error = false) t ~f =
  ensure_consumer_matches t ?consumer;
  let f =
    if not continue_on_error
    then f
    else (fun a -> try f a with exn -> Monitor.send_exn (Monitor.current ()) exn)
  in
  Deferred.create (fun finished ->
    (* We do [Deferred.unit >>>] to ensure that [f] is only called asynchronously. *)
    Deferred.unit
    >>> fun () ->
    let rec loop () =
      match read_now t ?consumer with
      | `Eof -> Ivar.fill finished ()
      | `Ok a -> f a; loop ()
      | `Nothing_available -> values_available t >>> fun _ -> loop ()
    in
    loop ());
;;

let drain t = iter' t ~f:(fun _ -> Deferred.unit)

let drain_and_count t = fold' t ~init:0 ~f:(fun sum q -> return (sum + Q.length q))

let read_all input =
  let result = Q.create () in
  iter' input ~f:(fun q -> Q.blit_transfer ~src:q ~dst:result (); Deferred.unit)
  >>| fun () ->
  result
;;

let to_list r = read_all r >>| Q.to_list

let to_stream_deprecated t =
  Stream.create (fun tail ->
    iter_without_pushback t ~f:(fun x -> Tail.extend tail x)
    >>> fun () ->
    Tail.close_exn tail)
;;

(* The implementation of [of_stream_deprecated] does as much batching as possible.  It
   grabs as many items as are available into an internal queue.  Once it has grabbed
   everything, it writes it to the pipe and then blocks waiting for the next element from
   the stream.

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
let of_stream_deprecated s =
  let r, w = create () in
  let q = Q.create () in
  let transfer () =
    if not (Q.is_empty q) then
      (* Can not pushback on the stream, so ignore the pushback on the pipe. *)
      don't_wait_for (write' w q);
  in
  let rec loop s =
    assert (not (is_closed w));
    let next_deferred = Stream.next s in
    match Deferred.peek next_deferred with
    | Some next -> loop_next next
    | None -> transfer (); upon next_deferred check_closed_loop_next
  and check_closed_loop_next next = if not (is_closed w) then loop_next next
  and loop_next = function
    | Nil -> transfer (); close w
    | Cons (x, s) -> Q.enqueue q x; loop s
  in
  loop s;
  r
;;

let transfer_gen
      (read_now : ?consumer:Consumer.t -> _ Reader.t -> _) write input output ~f =
  if !check_invariant then begin
    invariant input;
    invariant output;
  end;
  let consumer = link ~upstream:input ~downstream:output in
  Deferred.create (fun result ->
    (* We do [Deferred.unit >>>] to ensure that [f] is only called asynchronously. *)
    Deferred.unit
    >>> fun () ->
    let output_closed () =
      close_read input;
      Ivar.fill result ()
    in
    let rec loop () =
      if is_closed output
      then output_closed ()
      else
        match read_now input ~consumer with
        | `Eof -> Ivar.fill result ()
        | `Ok x -> f x continue
        | `Nothing_available ->
          choose [ choice (values_available input) ignore
                 ; choice (closed output)          ignore
                 ]
          >>> fun () ->
          loop ()
    and continue y =
      if is_closed output
      then output_closed ()
      else begin
        let pushback = write output y in
        Consumer.values_sent_downstream consumer;
        pushback
        >>> fun () ->
        loop ()
      end
    in
    loop ())
;;

let transfer' input output ~f =
  transfer_gen read_now' write' input output ~f:(fun q k -> f q >>> k)
;;

let transfer input output ~f =
  transfer_gen read_now write input output ~f:(fun a k -> k (f a))
;;

let transfer_id input output =
  transfer_gen read_now' write' input output ~f:(fun q k -> k q)
;;

let map_gen read write input ~f =
  let result, output = create () in
  upon (transfer_gen read write input output ~f) (fun () -> close output);
  result
;;

let map' input ~f = map_gen read_now' write' input ~f:(fun q k -> f q >>> k)
let map  input ~f = map_gen read_now  write  input ~f:(fun a k -> k (f a))

let filter_map' input ~f = map' input ~f:(fun q -> Deferred.Queue.filter_map q ~f)

let filter_map input ~f =
  map_gen read_now' write' input ~f:(fun q k ->
    k (Queue.filter_map q ~f:(fun x -> if is_read_closed input then None else f x)))
;;

let filter input ~f = filter_map input ~f:(fun x -> if f x then Some x else None)

let of_list l =
  let reader, writer = create () in
  don't_wait_for (write' writer (Q.of_list l));
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

let merge inputs ~cmp =
  let r, w = create () in
  let heap = Heap.create ~cmp:(fun (a1, _) (a2, _) -> cmp a1 a2) () in
  let handle_read input eof_or_ok =
    match eof_or_ok with
    | `Eof -> ()
    | `Ok v -> Heap.add heap (v, input);
  in
  let rec pop_heap_and_loop () =
    (* At this point, all inputs not at Eof occur in [heap] exactly once, so we know what
       the next output element is. *)
    match Heap.pop heap with
    | None -> close w
    | Some (v, input) ->
      write_without_pushback w v;
      if Heap.length heap = 0
      then upon (transfer_id input w) (fun () -> close w)
      else
        match read_now input with
        | `Eof | `Ok _ as x ->
          handle_read input x;
          pop_heap_and_loop ();
        | `Nothing_available ->
          pushback w
          >>> fun () ->
          read input
          >>> fun x ->
          handle_read input x;
          pop_heap_and_loop ();
  in
  let initial_push =
    Deferred.List.iter inputs ~f:(fun input ->
      read input
      >>| fun x ->
      handle_read input x)
  in
  upon initial_push pop_heap_and_loop;
  r
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
      don't_wait_for (write writer 13);
      close writer;
      let d = read reader in
      stabilize ();
      match Deferred.peek d with
      | Some z -> assert ([ 13 ] = get_values z)
      | None -> assert false
    in
    check_read read' (function `Ok q -> Q.to_list q | _ -> assert false);
    check_read read (function `Ok a -> [ a ] | _ -> assert false);
    check_read (fun r -> read_at_most r ~num_values:1)
      (function `Ok q -> Q.to_list q | _ -> assert false);
    check_read (fun r -> read_exactly r ~num_values:1)
      (function `Exactly q -> Q.to_list q | _ -> assert false);
    check_read (fun r -> return (read_now' r))
      (function `Ok q -> Q.to_list q | _ -> assert false);
    check_read read_all Q.to_list;
  ;;

  TEST_UNIT =
    let (reader, writer) = create () in
    let f1 = downstream_flushed writer in
    don't_wait_for (write writer 13);
    let f2 = downstream_flushed writer in
    close_read reader;
    stabilize ();
    assert (Deferred.peek f1 = Some `Ok);
    assert (Deferred.peek f2 = Some `Reader_closed);
  ;;

  (* ==================== init ==================== *)
  TEST_UNIT =
    let reader = init (fun _ -> Deferred.never ()) in
    stabilize ();
    assert (not (is_closed reader));
    assert (Option.is_none (peek reader));
  ;;

  TEST_UNIT =
    let reader = init (fun writer ->
      write_without_pushback writer ();
      Deferred.unit) in
    stabilize ();
    assert (is_closed reader);
    assert (Option.is_some (peek reader));
  ;;

  TEST_UNIT =
    let finish = Ivar.create () in
    let reader = init (fun writer ->
      write_without_pushback writer ();
      Ivar.read finish)
    in
    stabilize ();
    assert (not (is_closed reader));
    assert (Option.is_some (peek reader));
    Ivar.fill finish ();
    let d = to_list reader in
    stabilize ();
    assert (is_closed reader);
    assert (Deferred.peek d = Some [ () ]);
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
    ignore (read_now' reader);
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
    don't_wait_for (write writer 13);
    close writer;
    let d = read_all reader in
    stabilize ();
    assert (read_result d = [ 13 ]);
  ;;

  (* ==================== read_at_most ==================== *)

  TEST_UNIT =
    let (reader, writer) = create () in
    don't_wait_for (write' writer (Q.of_list [ 12; 13; 14 ]));
    close writer;
    let d =
      read_at_most reader ~num_values:2
      >>| function
      | `Eof -> assert false
      | `Ok q -> q
    in
    stabilize ();
    assert (read_result d = [ 12; 13 ]);
  ;;

  TEST_UNIT =
    let (reader, writer) = create () in
    don't_wait_for (write' writer (Q.of_list [ 12; 13; 14 ]));
    close writer;
    let d =
      read_at_most reader ~num_values:4
      >>| function
      | `Eof -> assert false
      | `Ok q -> q
    in
    stabilize ();
    assert (read_result d = [ 12; 13; 14 ]);
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
    don't_wait_for (write' writer (Q.of_list l));
    close writer;
    let d = read_all reader in
    stabilize ();
    assert (read_result d = l);
  ;;

  (* ==================== map ==================== *)

  TEST_UNIT =
    let (reader, writer) = create () in
    let reader = map reader ~f:(fun x -> x + 13) in
    don't_wait_for (write' writer (Q.of_list [ 1; 2; 3 ]));
    let d =
      read_exactly reader ~num_values:2
      >>| function
      | `Eof | `Fewer _ -> assert false
      | `Exactly q -> close_read reader; q
    in
    stabilize ();
    assert (is_closed writer);
    assert (read_result d = [ 14; 15 ]);
  ;;

  (* ==================== of_stream_deprecated ==================== *)

  TEST_UNIT =
    let tail = Tail.create () in
    let pipe = of_stream_deprecated (Tail.collect tail) in
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
    let l = [ 1; 2; 3 ] in
    let t = interleave [ of_list l ] in
    let d = read_all t in
    stabilize ();
    assert (read_result d = l);
  ;;

  TEST_UNIT =
    let l = [ 1; 2; 3 ] in
    let t = interleave [ of_list l; of_list l ] in
    let d = read_all t in
    stabilize ();
    assert (List.length (read_result d) = 2 * List.length l);
  ;;

  (* ==================== merge ==================== *)
  TEST_UNIT =
    let cases =
      [ []
      ; [ [] ]
      ; [ [ 1; 3; 7 ] ]
      ; [ []; []; []; ]
      ; [ [ 1; 7; 10 ] ]
      ; [ [ 1; 5; 12 ]; [ 3; 3; 4; 22 ]; [ 1 ]; [ 40 ] ]
      ; [ [ 1; 5; 12 ]; [ 3; 3; 4; 22 ]; []; [] ]
      ; [ [ 27 ]; [ 1; 3; 3; 4; 22 ]; [ 2; 27; 49 ] ]
      ; [ [ 27 ]; [ 1; 3; 3; 4; 22; 27; 31; 59; 72 ]; [ 2; 27; 49 ] ]
      ; [ [ 2; 9; 12; 27; 101 ]
        ; [ 1; 3; 3; 4; 22; 27; 31; 59; 72 ]
        ; [ 2; 27; 49; 127; 311 ]
        ]
      ]
    in
    let transfer_by = [ 1; 2; 3; 5; 10 ] in
    let cmp = Int.compare in
    let finished =
      Deferred.List.iter cases ~f:(fun lists ->
        (* The merge function assumes that the pipes return ordered values. *)
        let lists = List.map lists ~f:(fun list -> List.sort list ~cmp) in
        let expected_result = List.sort ~cmp (List.concat lists) in
        Deferred.List.iter transfer_by ~f:(fun transfer_by ->
          let pipes = List.map lists ~f:(fun _ -> create ()) in
          let merged_pipe = merge (List.map pipes ~f:fst) ~cmp in
          List.iter2_exn lists pipes ~f:(fun list (_, writer) ->
            let rec loop index = function
              | [] -> close writer
              | a :: tail ->
                write_without_pushback writer a;
                if index mod transfer_by > 0
                then loop (index + 1) tail
                else begin
                  pushback writer
                  >>> fun () ->
                  pushback merged_pipe
                  >>> fun () ->
                  loop (index + 1) tail
                end
            in
            loop 1 list);
          to_list merged_pipe
          >>| fun actual_result ->
          if not (actual_result = expected_result)
          then failwiths "mismatch" (actual_result, expected_result)
                 <:sexp_of< int list * int list >>))
    in
    stabilize ();
    assert (Deferred.is_determined finished);
  ;;

  (* ==================== iter' ==================== *)

  TEST_UNIT =
    let r = ref 0 in
    let l = [ 1; 2; 3 ] in
    let t = of_list l in
    let iter_finished = ref false in
    upon (iter' t ~f:(fun q -> Queue.iter q ~f:(fun i -> r := !r + i); Deferred.unit))
      (fun () -> iter_finished := true);
    stabilize ();
    assert (!r = 6);
    assert !iter_finished;
  ;;

  TEST_UNIT =
    let count = ref 0 in
    let r, w = create () in
    write_without_pushback w 13;
    let iter_finished = ref false in
    ignore
      (Monitor.try_with
         (fun () ->
            let finished =
              iter' r ~f:(fun q ->
                Queue.iter q ~f:(fun i ->
                  if i = 17 then failwith "" else count := !count + i);
                Deferred.unit)
            in
            upon finished (fun () -> iter_finished := true);
            finished));
    stabilize ();
    write_without_pushback w 17;
    stabilize ();
    assert (!count = 13);
    assert (not !iter_finished);
  ;;

  TEST_UNIT =
    let count = ref 0 in
    let r, w = create () in
    write_without_pushback w 13;
    let iter_finished = ref false in
    ignore
      (Monitor.try_with
         (fun () ->
            let finished =
              iter' r ~continue_on_error:true ~f:(fun q ->
                Queue.iter q ~f:(fun i ->
                  if i = 17 then failwith "" else count := !count + i);
                Deferred.unit)
            in
            upon finished (fun () -> iter_finished := true);
            finished));
    stabilize ();
    assert (not !iter_finished);
    write_without_pushback w 17;
    stabilize ();
    assert (not !iter_finished);
    write_without_pushback w 19;
    stabilize ();
    assert (!count = 32);
    assert (not !iter_finished);
    close w;
    stabilize ();
    assert (!iter_finished);
  ;;

  (* ==================== iter ==================== *)

  TEST_UNIT =
    let r = ref 0 in
    let l = [ 1; 2; 3 ] in
    let t = of_list l in
    let iter_finished = ref false in
    upon (iter t ~f:(fun i -> r := !r + i; Deferred.unit))
      (fun () -> iter_finished := true);
    stabilize ();
    assert (!r = 6);
    assert !iter_finished;
  ;;

  TEST_UNIT =
    let r = ref 0 in
    let l = [ 13; 17 ] in
    let t = of_list l in
    let iter_finished = ref false in
    ignore
      (Monitor.try_with
         (fun () ->
            let finished =
              iter t ~f:(fun i ->
                if i = 17 then failwith "" else r := !r + i;
                Deferred.unit)
            in
            upon finished (fun () -> iter_finished := true);
            finished));
    stabilize ();
    assert (!r = 13);
    assert (not !iter_finished);
  ;;

  TEST_UNIT =
    let r = ref 0 in
    let l = [ 1; 2; 3 ] in
    let t = of_list l in
    let iter_finished = ref false in
    ignore
      (Monitor.try_with
         (fun () ->
            let finished =
              iter t ~continue_on_error:true ~f:(fun i ->
                if i = 2 then failwith "" else r := !r + i;
                Deferred.unit)
            in
            upon finished (fun () -> iter_finished := true);
            finished));
    stabilize ();
    assert (!r = 4);
    assert !iter_finished;
  ;;

  (* ==================== iter_without_pushback ==================== *)

  TEST_UNIT =
    let r = ref 0 in
    let l = [ 1; 2; 3 ] in
    let t = of_list l in
    let iter_finished = ref false in
    upon (iter_without_pushback t ~f:(fun i -> r := !r + i))
      (fun () -> iter_finished := true);
    stabilize ();
    assert (!r = 6);
    assert !iter_finished;
  ;;

  TEST_UNIT =
    let r = ref 0 in
    let l = [ 13; 17 ] in
    let t = of_list l in
    let iter_finished = ref false in
    ignore
      (Monitor.try_with
         (fun () ->
            let finished =
              iter_without_pushback t ~f:(fun i ->
                if i = 17 then failwith "" else r := !r + i)
            in
            upon finished (fun () -> iter_finished := true);
            finished));
    stabilize ();
    assert (!r = 13);
    assert (not !iter_finished);
  ;;

  TEST_UNIT =
    let r = ref 0 in
    let l = [ 1; 2; 3 ] in
    let t = of_list l in
    let iter_finished = ref false in
    ignore
      (Monitor.try_with
         (fun () ->
            let finished =
              iter_without_pushback t ~continue_on_error:true ~f:(fun i ->
                if i = 2 then failwith "" else r := !r + i)
            in
            upon finished (fun () -> iter_finished := true);
            finished));
    stabilize ();
    assert (!r = 4);
    assert !iter_finished;
  ;;

  (* ==================== flush chaining ==================== *)
  TEST_UNIT =
    let flushed f = match Deferred.peek f with Some `Ok -> true | _ -> false in
    let r, w = create () in
    assert (Deferred.peek (downstream_flushed w) = Some `Ok);
    let flushed_downstream = ref (return `Ok) in
    let consumer = add_consumer r ~downstream_flushed:(fun () -> !flushed_downstream) in
    let f1 = downstream_flushed w in
    stabilize ();
    assert (Deferred.peek f1 = Some `Ok);
    write_without_pushback w ();
    let f2 = downstream_flushed w in
    assert (Deferred.peek (read r ~consumer) = Some (`Ok ()));
    let f3 = downstream_flushed w in
    assert (not (flushed f2));
    assert (not (flushed f3));
    Consumer.values_sent_downstream consumer;
    let flushed_downstream_ivar = Ivar.create () in
    flushed_downstream := Ivar.read flushed_downstream_ivar;
    let f4 = downstream_flushed w in
    stabilize ();
    let f5 = downstream_flushed w in
    assert (not (flushed f2));
    assert (not (flushed f3));
    assert (not (flushed f4));
    assert (not (flushed f5));
    Ivar.fill flushed_downstream_ivar `Ok;
    let f6 = downstream_flushed w in
    write_without_pushback w ();
    let f7 = downstream_flushed w in
    stabilize ();
    assert (flushed f2);
    assert (flushed f3);
    assert (flushed f4);
    assert (flushed f5);
    assert (flushed f6);
    assert (not (flushed f7));
  ;;

  TEST_UNIT =
    let flushed f = match Deferred.peek f with Some `Ok -> true | _ -> false in
    let r_, w = create () in
    let r = map r_ ~f:Fn.id in
    let f1 = downstream_flushed r in
    stabilize ();
    assert (Deferred.peek f1 = Some `Ok);
    write_without_pushback w ();
    let f2 = downstream_flushed w in
    let f3 = upstream_flushed r in
    stabilize ();
    assert (is_empty w);
    assert (not (is_empty r));
    assert (not (flushed f2));
    assert (not (flushed f3));
    let f4 = downstream_flushed w in
    let f5 = upstream_flushed r in
    assert (Deferred.peek (read r) = Some (`Ok ()));
    let f6 = downstream_flushed w in
    let f7 = upstream_flushed r in
    write_without_pushback w ();
    let f8 = downstream_flushed w in
    let f9 = upstream_flushed r in
    stabilize ();
    assert (flushed f2);
    assert (flushed f3);
    assert (flushed f4);
    assert (flushed f5);
    assert (flushed f6);
    assert (flushed f7);
    assert (not (flushed f8));
    assert (not (flushed f9));
  ;;

  (* ==================== consumer mismatch ==================== *)
  TEST_UNIT =
    let r1, _w1 = create () in
    let r2, _w2 = create () in
    let consumer = add_consumer r1 ~downstream_flushed:(const (return `Ok)) in
    assert (Result.is_error (Result.try_with (fun () -> read_now r2 ~consumer)));
  ;;

  (* ==================== close_read and single-item processors ==================== *)
  TEST_UNIT =
    let i = ref 0 in
    let r = of_list [ (); () ] in
    don't_wait_for (fold r ~init:() ~f:(fun () () -> incr i; close_read r; Deferred.unit));
    stabilize ();
    assert (!i = 1);
  ;;

  TEST_UNIT =
    let i = ref 0 in
    let r = of_list [ (); () ] in
    don't_wait_for (fold_without_pushback r ~init:() ~f:(fun () () -> incr i; close_read r));
    stabilize ();
    assert (!i = 1);
  ;;

  TEST_UNIT =
    let i = ref 0 in
    let r = of_list [ (); () ] in
    don't_wait_for (iter r ~f:(fun () -> incr i; close_read r; Deferred.unit));
    stabilize ();
    assert (!i = 1);
  ;;

  TEST_UNIT =
    let i = ref 0 in
    let r = of_list [ (); () ] in
    don't_wait_for (iter_without_pushback r ~f:(fun () -> incr i; close_read r));
    stabilize ();
    assert (!i = 1);
  ;;

  TEST_UNIT =
    let r = of_list [ (); () ] in
    let r2, w2 = create () in
    upon (transfer r w2 ~f:(fun () -> close_read r)) (fun () -> close w2);
    let res = to_list r2 in
    stabilize ();
    assert (Deferred.peek res = Some [ () ]);
  ;;

  TEST_UNIT =
    let r = of_list [ (); () ] in
    let res = to_list (map r ~f:(fun () -> close_read r)) in
    stabilize ();
    assert (Deferred.peek res = Some [ () ]);
  ;;

  TEST_UNIT =
    let r = of_list [ (); () ] in
    let res = to_list (filter_map r ~f:(fun () -> close_read r; Some ())) in
    stabilize ();
    assert (Deferred.peek res = Some [ () ]);
  ;;

  TEST_UNIT =
    let r = of_list [ (); () ] in
    let res = to_list (filter r ~f:(fun () -> close_read r; true)) in
    stabilize ();
    assert (Deferred.peek res = Some [ () ]);
  ;;
end
