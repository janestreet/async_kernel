(** An ivar is a write-once cell that can be empty or full (i.e. hold a single value) that
    one can [read] and to obtain a deferred that becomes determined when the ivar is
    filled.  An ivar is similar to an ['a option ref], except it is an error to fill an
    already full ivar. *)

open Core.Std
open Import

type 'a t with bin_io, sexp_of

include Invariant.S1 with type 'a t := 'a t

(** [equal t t'] is physical equality of [t] and [t']. *)
val equal : 'a t -> 'a t -> bool

(** [create ()] returns an empty ivar. *)
val create : unit -> 'a t

(** [fill t v] fills [t] with value [v] if [t] was empty.  If [t] was full, fill raises
    an exception. *)
val fill : 'a t -> 'a -> unit

(** [fill_if_empty t v] fills [t] with [v] if [t] is currently empty.  If [t] is full,
    then [fill_if_empty] does nothing.  *)
val fill_if_empty : 'a t -> 'a -> unit

(** [is_empty t] returns true if [t] is empty *)
val is_empty : 'a t -> bool

(** [is_full t] returns true if [t] is full *)
val is_full : 'a t -> bool

(** The [Raw] interface and [Deferred] module exposed here are for async's internal use
    only.  They must be exported here because we want the [Deferred.t] and [Ivar.t] types
    to be fully abstract, so that they show up nicely in type errors, yet other async
    code defined later needs to deal with the raw types. *)

include Raw
  with type execution_context := Execution_context.t
  with type ('a, 'b) raw := ('a, 'b) Raw_ivar.t
  with type 'a t := 'a t

module Deferred : sig
  type +'a t with sexp_of

  include Raw
    with type execution_context := Execution_context.t
    with type ('a, 'b) raw := ('a, 'b) Raw_deferred.t
    with type 'a t := 'a t
end

(** [read t] returns a deferred that becomes enabled with value [v] after the ivar is
    filled with [v]. *)
val read : 'a t -> 'a Deferred.t


