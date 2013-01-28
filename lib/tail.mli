(** A tail is a pointer to the end of a stream that can be used to extend the stream. *)

open Import

type 'a t with sexp_of

(** [create ()] returns a new tail. *)
val create : unit -> _ t

(** [extend t v] extends the stream, and will raise an exception if [t] has been
    closed. *)
val extend : 'a t -> 'a -> unit

(** [close_exn t] closes [t].  Subsequent calls to [close_exn] or [extend]
    will raise an exception. *)
val close_exn : _ t -> unit

(** [close_if_open t] closes [t], if it's not already closed.  If [t] is already
    closed, then this is a no-op. *)
val close_if_open : _ t -> unit

(** [is_closed t] returns true iff the stream [t] is closed. *)
val is_closed : _ t -> bool

(** The [Raw] interface and [Stream] module exposed here are for async's internal use
    only.  They must be exported here because we want the [Tail.t] and [Stream.t] types to
    be fully abstract, so that they show up nicely in type errors, yet other async code
    defined later needs to deal with the raw types. *)

include Raw
  with type execution_context := Execution_context.t
  with type ('a, 'b) raw := ('a, 'b) Raw_tail.t
  with type 'a t := 'a t

module Stream : sig
  type 'a t with sexp_of

  type ('a, 'execution_context) next_
  = ('a, 'execution_context) Raw_stream.next
  = Nil | Cons of 'a * ('a, 'execution_context) Raw_stream.t

  type 'a next = ('a, Execution_context.t) next_

  val next : 'a t -> 'a next Deferred.t

  include Raw
    with type execution_context := Execution_context.t
    with type ('a, 'b) raw := ('a, 'b) Raw_stream.t
    with type 'a t := 'a t
end

(** [collect t] returns the stream starting at the current position of the tail, i.e. the
    stream consisting of all subsequent [extend]s. *)
val collect : 'a t -> 'a Stream.t
