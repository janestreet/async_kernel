(** A pipe is a buffered FIFO communication channel.

    A pipe has a "writer" end and a "reader" end.  The intent is that a writer feeds
    values into the pipe and then waits until it is notified that it should put more data
    in (referred to as "pushback").

    Each pipe contains a buffer that is a queue of values that have been written to the
    pipe but not yet read from the pipe.  The length of the queue is not bounded; whenever
    the pipe is written to, values are immediately enqueued.  However, writers are
    supposed to respect pushback from readers, either via the [unit Deferred.t] returned
    by [write] calls or by explicitly calling [pushback].

    If a pipe is empty, then readers queue up, waiting for values to be written.  As soon
    as values are written, if a reader is available to consume them, the values will be
    handed to the reader.

    One can use [flushed] to get notified by a pipe when all prior writes have been
    consumed by a reader.

    There are distinct [Reader] and [Writer] modules and types, but all of the operations
    on readers and writers are available directly from the [Pipe] module. *)
open Core.Std

type ('a, 'phantom) t with sexp_of
type ('a, 'phantom) pipe = ('a, 'phantom) t with sexp_of

(** {1 Reader and Writer modules} *)
(******************************************************************************)
(** These provide reader- and writer-specific types for the base pipe type. *)

module Writer : sig
  type phantom
  type 'a t = ('a, phantom) pipe with sexp_of

  val invariant : _ t -> unit
end

module Reader : sig
  type phantom
  type 'a t = ('a, phantom) pipe with sexp_of

  val invariant : _ t -> unit
end

(** {1 Creation} *)

(** [create ()] creates a new pipe. *)
val create : unit  -> 'a Reader.t * 'a Writer.t

(** [of_list l] returns a closed pipe reader filled with the contents of [l]. *)
val of_list : 'a list -> 'a Reader.t

(** {1 Closing} *)
(******************************************************************************)
(** [close t] closes the write end of the pipe:

    - Future write attempts will fail, raising an exception.

    - If, at the time of the close, there are reads blocked waiting for data, these reads
    will unblock, producing [`Eof].

    - Future read attempts will drain the data that was in the pipe at the time of the
    close, until the pipe's buffer has been exhausted; subsequent reads will immediately
    get [`Eof].

    Thus, after a pipe has been closed, reads never block.

    Close is idempotent.

    [close_read t] closes both the read and write ends of the pipe.  It does everything
    [close] does, and in addition:

    - all pending flushes become determined with [`Reader_closed].
    - the pipe buffer is cleared.
    - all subsequent reads will get [`Eof] *)
val close       : _ Writer.t -> unit
val close_read  : _ Reader.t -> unit

(** [is_closed t] returns [true] iff [close t] or [close_read t] has been called. *)
val is_closed : (_, _) t -> bool

(** [closed t] returns a deferred that becomes determined when [close t] or [close_read t]
    is called. *)
val closed : (_, _) t -> unit Deferred.t

(** {1 Generic pipe operations} *)
(******************************************************************************)
(** These operations apply to all values of type [(_, _) t], that is, both readers
    and writers. *)

(** [flushed t] becomes determined when all values written prior to the call to [flushed]
    have been removed from the pipe and handed to a reader, or if the reader end of the
    pipe is closed. *)
val flushed : (_, _) t -> [ `Ok | `Reader_closed ] Deferred.t

(** [length t] returns the number of elements currently queued in [t] *)
val length : (_, _) t -> int

(** {1 Writing} *)
(******************************************************************************)
(** The write operations return a deferred value that is determined when either (1) it is
    OK to write again to the pipe or (2) the pipe has been closed.  This deferred is the
    data-producer's interface to the pipe pushback mechanism: it tells the producer when
    it should proceed after doing a write -- either to produce and write more data to the
    pipe, or to abandon production entirely.  The pushback mechansim is just advisory: a
    producer task can, but typically should not, dump arbitrary amounts of data into a
    pipe even if there is no consumer draining the pipe.

    Producers that write a sequence of values to a pipe should be aware that the consumers
    who read from the pipe can close the pipe early -- that is, before the producer has
    finished doing all of its writes.  If this happens, further writes will raise an
    exception.  To avoid these errors, all writes must be atomically guarded by
    [is_closed] tests.  Thus, a typical writer loop should look like this:

    {[
    fun countup hi w = (* Send the ints in range \[0,hi) to writer W. *)
      let rec loop i =
        if i < hi and not (is_closed w) then (* Guard write w/closed test. *)
          write i w >>>            (* Do the write then block until datum     *)
          fun () -> loop (i+1)     (*   fits or the pipe is closed.           *)
        else close w (* No harm done if reader has already closed the pipe.*)
      in
      loop 0
    ]}

    If the pipe's consumer stops reading early and closes the pipe, [countup] won't error
    out trying to write further values down the pipe: it will immediately wake up and
    exit. *)

(** [pushback writer] becomes determined when either [writer] has been closed or
    the pipe is empty. *)
val pushback : 'a Writer.t -> unit Deferred.t

(** [write' writer q] transfers the elements from [q] into the pipe, leaving [q] empty.
    [write'] returns a pushback deferred, as described above.  Writing to a closed pipe
    raises.

    [write writer v] is equivalent to [write' writer (Queue.singleton v)]. *)
val write' : 'a Writer.t -> 'a Queue.t -> unit Deferred.t
val write  : 'a Writer.t -> 'a         -> unit Deferred.t

(** [with_write writer ~f] waits until there is space available in the pipe, and then
    calls [f write], where [write] can be used by [f] to write a single value into the
    pipe at a time.  [with_write] guarantees that the pipe is open when it calls [f], and
    hence that the writes will succeed, unless [f] itself closes the pipe. *)
val with_write :
  'a Writer.t
  -> f:(('a -> unit) -> 'b)
  -> [ `Closed | `Ok of 'b ] Deferred.t

(** {1 Reading} *)
(******************************************************************************)
(** With two special exceptions, all read procedures have a best-effort/forward-progress
    semantics:

    - Best effort: When you do a read, you get what's available {e right now}, which might
    be less than you requested.

    - Forward progress: However, if {e nothing is available}, you block until some data
    comes in (unless you're at EOF, in which case there's obviously no point in waiting).
    So the only time you ever get an empty, 0-item read is when you're at EOF.

    The best-effort semantics allows you to program in a style that processes data in big
    slabs, yet also moves data through your processing in as timely a way as possible.

    The forward-progress semantics means that every call produces {e some} data, so you
    can process an n-element input with at most n reads; you cannot burn an unbounded
    number of cycles "spinning" doing an unbounded number of empty-result "polling" calls
    (which, in a non-preemptive system like Async could lock up the process).

    The two exceptions to best-effort/forward-progress semantics are [read_now], which
    polls for data, thus abandoning the forward-progress guarantee, and [read_exactly],
    which loops until it has read the entire amount requested (or encountered EOF), thus
    abandoning the best-effort guarantee of timeliness. *)

(** [read' pipe] reads all of the values available in the pipe, as soon as any value
    becomes available.  The resulting queue will satisfy [Q.length q > 0]. *)
val read' : 'a Reader.t -> [ `Eof | `Ok of 'a Queue.t ] Deferred.t

(** [read pipe] reads a single value from the pipe. *)
val read : 'a Reader.t -> [ `Eof | `Ok of 'a ] Deferred.t

(** [read_at_most r ~num_values] reads up to [num_values] values from the pipe's currently
    available data, blocking if the pipe is empty.  The resulting queue will satisfy [0 <
    Queue.length q <= num_values].  [read_at_most] raises if [num_values <= 0]. *)
val read_at_most : 'a Reader.t -> num_values:int -> [ `Eof | `Ok of 'a Queue.t ] Deferred.t

(** [read_exactly r ~num_values] reads exactly [num_values] items, unless EOF is
    encountered.  [read_exactly] performs a sequence of [read_at_most] operations, so
    there is no guarantee that the queue of values it returns comprise a contiguous
    segment of the written stream of values -- other readers might pick off elements
    in-between [read_exactly]'s atomic reads.  [read_exactly] raises if [num_values <= 0].
*)
val read_exactly
  :  'a Reader.t
  -> num_values:int
  -> [ `Eof
     | `Fewer of 'a Queue.t   (* 0 < Q.length q < num_values *)
     | `Exactly of 'a Queue.t (* Q.length q = num_values *)
     ] Deferred.t

(** [read_now reader] reads all of the values from [reader] that are immediately
    available.  The resulting queue will satisfy [Q.length q > 0].  If [reader] is closed,
    [read_now] returns [`Eof].  If [reader] is empty, [read_now] returns
    [`Nothing_available].  [read_now] has the danger of permitting the computation to
    "spin" doing empty reads; it is only useful in exotic circumstances. *)
val read_now : 'a Reader.t -> [ `Eof | `Nothing_available | `Ok of 'a Queue.t ]

(** [clear reader] consumes all of the values currently in [reader], and all blocked
    flushes become determined with [`Ok]. *)
val clear : 'a Reader.t -> unit

(** [read_all reader] reads all the values from the pipe until it is closed.  An
    alternative name might be [Reader.to_queue]. *)
val read_all : 'a Reader.t -> 'a Queue.t Deferred.t

(** [values_available reader] returns a deferred that becomes determined when there are
    values in the pipe.  If there are multiple readers (a rare situation), there is no
    guarantee, that some other reader hasn't become active because of ordinary async
    scheduling and removed some or all of the values between the time the result of
    [values_available] becomes determined and the time something waiting [upon] that
    result runs.

    [values_available] is useful when one wants to [choose] on values being available in a
    pipe, so that one can be sure and not remove values and drop them on the floor.

    [values_available] is roughly equivalent to [read_at_most ~num_values:0]. *)
val values_available : _ Reader.t -> [ `Eof | `Ok ] Deferred.t

(** {1 Sequence functions} *)
(******************************************************************************)
(** Issues: {ul
    {- Scalar & batch sequence processing

    Each of the sequence functions (fold, iter, transfer, map) comes in two versions:
    "scalar" and "batch" processing.  The scalar version has the ordinary type for [f],
    which handles an element at a time in a non-deferred way.  In the batch version, [f]
    deals with a queue of elements from the pipe at a time, and can block, which will
    cause pushback on writers due to elements not being consumed. }

    {- Early-close and functions that copy between pipes

    Some functions ([transfer], [map], [filter_map], [filter], [interleave], [concat], and
    their primed, batch-processing variants) spawn a background task that copies data from
    some upstream pipe to some downstream pipe, perhaps with some processing inserted
    in-between.  These copying tasks finish under two circumstances.  The standard,
    "normal" case is when the copying task gets EOF from the upstream pipe -- there is no
    more data to copy.  In this case, the copying task closes the downstream pipe and
    exits.

    Somewhat less common is when the downstream consumer decides to stop reading early,
    while the upstream producer is still sending data to the copy task.  (E.g., perhaps
    the consumer was searching its incoming stream for some value, and it found that
    value, so there's no need to search further.)  In this case, the consumer closes its
    pipe to indicate it's done reading values.  When the copy task discovers that its
    downstream pipe is closed, it propagate the close to the upstream producer by closing
    its pipe and stops processing. } } *)

(** [fold' reader ~init ~f] reads a batch of elements from [reader], supplies them to [f],
    waits for [f] to finish, and then repeats.  [fold'] finishes when the call to [f] on
    the final batch of elements from [reader] finishes.

    [fold reader ~init ~f] folds over the elements of [reader], consuming them as they
    come in.  [fold] finishes when the final call to [f] returns. *)
val fold' : 'a Reader.t -> init:'b -> f:('b -> 'a Queue.t -> 'b Deferred.t) -> 'b Deferred.t
val fold :  'a Reader.t -> init:'b -> f:('b -> 'a         -> 'b           ) -> 'b Deferred.t

(** [iter' reader ~f] repeatedly applies [f] to batches of elements of [reader], waiting
    for each call to [f] to finish before continuing.  The deferred returned by [iter']
    becomes determined when the call to [f] on the final batch of elements finishes.

    [iter] is a specialization of [iter'] that uses [Deferred.Queue.iter ~f]

    [iter_without_pushback] is a specialized version that applies [f] to each element
    that arrives on the pipe, without giving [f] a chance to pushback on the iteration
    continuing. *)
val iter' : 'a Reader.t -> f:('a Queue.t -> unit Deferred.t) -> unit Deferred.t
val iter  : 'a Reader.t -> f:('a         -> unit Deferred.t) -> unit Deferred.t
val iter_without_pushback
          : 'a Reader.t -> f:('a         -> unit           ) -> unit Deferred.t


(** [transfer' input output ~f ?stop] repeatedly reads a batch of elements from [input],
    applies [f] to the batch, writes the result as a batch to [output], and then waits on
    [pushback] in [output] before continuing.  [transfer'] finishes if [input] is closed,
    or [output] is closed, or [stop] is determined.  If [output] is closed, then
    [transfer'] closes [input].

    [transfer] is a specialization of [transfer'] that uses [Queue.map ~f].

    [transfer_id] is a specialization of [transfer'] wifh [f = Fn.id]. *)
val transfer'
  : 'a Reader.t -> 'b Writer.t -> f:('a Queue.t -> 'b Queue.t Deferred.t) -> unit Deferred.t
val transfer
  : 'a Reader.t -> 'b Writer.t -> f:('a         -> 'b                   ) -> unit Deferred.t
val transfer_id
  : 'a Reader.t -> 'a Writer.t                                            -> unit Deferred.t

(** [map' input ~f] returns a reader, [output], and repeatedly applies [f] to batches of
    elements from [input], with the results appearing in [output].  If values are not
    being consumed from [output], [map'] will pushback and stop consuming values from
    [input].

    If [output] is closed, then [map'] will close [input].

    [map] is a specialization of [map'] that uses [Queue.map ~f]. *)
val map' : 'a Reader.t -> f:('a Queue.t -> 'b Queue.t Deferred.t) -> 'b Reader.t
val map  : 'a Reader.t -> f:('a         -> 'b                   ) -> 'b Reader.t

(** [filter_map' input ~f] returns a reader, [output], and repeatedly applies [f] to
    elements from [input], with the results that aren't [None] appearing in [output].  If
    values are not being consumed from [output], [filter_map'] will pushback and stop
    consuming values from [input].

    If [output] is closed, then [filter_map'] will close [input].

    [filter_map] is a specialized version of [filter_map']. *)
val filter_map' : 'a Reader.t -> f:('a -> 'b option Deferred.t) -> 'b Reader.t
val filter_map  : 'a Reader.t -> f:('a -> 'b option           ) -> 'b Reader.t

(** [filter input ~f] returns a reader, [output], and copies to [output] each element from
    [input] that satisfies the predicate [f].  If [output] is closed, then [filter] closes
    [input]. *)
val filter : 'a Reader.t -> f:('a -> bool) -> 'a Reader.t

(** [interleave inputs] returns a reader [output], and, for each input, transfers batches
    of values from that input to [output], using [transfer_id].  Each input is transferred
    to [output] independently.  So, batches of values from different inputs can be in
    flight to [output] simultaneously, but at most one batch at a time from any particular
    input.  The operation is complete when either all the [inputs] produce EOF, or when
    [output] is closed by the downstream consumer (in which case [interleave] closes all
    the [inputs]). *)
val interleave : 'a Reader.t list -> 'a Reader.t

(** [concat inputs] return a reader, [output], with the values from each pipe in [inputs]
    in sequence.  [concat] closes [output] once it reaches EOF on the final input.
    If [output] is closed, then [concat] closes all its inputs. *)
val concat : 'a Reader.t list -> 'a Reader.t

(** [to_stream reader] returns a stream that reads everything from the pipe. *)
val to_stream : 'a Reader.t -> 'a Async_stream.t

(** [of_stream reader] return a pipe that has one element for every element on the
    stream *)
val of_stream : 'a Async_stream.t -> 'a Reader.t

(** [drain reader] repeatedly reads values from [reader] and throws them away.

    [drain_and_count] is like [drain], except it also counts the number of values it
    has read. *)
val drain           : 'a Reader.t -> unit Deferred.t
val drain_and_count : 'a Reader.t ->  int Deferred.t

(** [to_list input] reads everything from [input]; on EOF, it produces the accumulated
    list of these values. *)
val to_list : 'a Reader.t -> 'a list Deferred.t

(** {1 Miscellaneous} *)
(******************************************************************************)

(** [hash] a hash function suitable for pipes *)
val hash : (_, _) t -> int

(** [equal] on pipes is physical equality. *)
val equal : ('a, 'b) t -> ('a, 'b) t -> bool

(** {1 Size budget} *)
(******************************************************************************)
(** Every pipe has a "size budget", which governs the pushback that is used to discourage
    writers from enqueueing arbitrarily large amounts of data.  As long as the length of
    the pipe exceeds the size budget, writers will not be notified to do further writing.
    Whenver the length is less than or equal to the size budget, writers will be notified
    to continue.

    Every pipe's initial size budget is zero. *)
val size_budget: (_, _) t -> int

(** [set_size_budget t i] changes the size budget of [t] to [i].  Any nonnegative value is
    allowed. *)
val set_size_budget: (_, _) t -> int -> unit

(** {1 Debugging} *)
(******************************************************************************)

(** [show_debug_messages], if true will cause a message to be printed at the start of each
    operation, showing the pipe and other arguments. *)
val show_debug_messages : bool ref

(** [check_invariant], if true, will cause pipes' invariants to be checked at the start of
    each operation. *)
val check_invariant : bool ref
