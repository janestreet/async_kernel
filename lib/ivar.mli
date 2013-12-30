(** A write-once cell that can be empty or full (i.e. hold a single value).

    One can [read] an ivar to obtain a deferred that becomes determined when the ivar is
    filled.  An ivar is similar to an ['a option ref], except it is an error to fill an
    already full ivar. *)

open Core.Std
open Import

type 'a t with bin_io, sexp_of
type 'a ivar = 'a t

include Invariant.S1 with type 'a t := 'a t

(** [equal t t'] is physical equality of [t] and [t']. *)
val equal : 'a t -> 'a t -> bool

(** [create ()] returns an empty ivar. *)
val create : unit -> 'a t

(** [fill t v] fills [t] with value [v] if [t] was empty.  If [t] was full, fill raises
    an exception.

    It is guaranteed that immediately after calling [fill t],
    [is_some (Deferred.peek (read t))]. *)
val fill : 'a t -> 'a -> unit

(** [fill_if_empty t v] fills [t] with [v] if [t] is currently empty.  If [t] is full,
    then [fill_if_empty] does nothing.  *)
val fill_if_empty : 'a t -> 'a -> unit

(** [is_empty t] returns true if [t] is empty *)
val is_empty : 'a t -> bool

(** [is_full t] returns true if [t] is full *)
val is_full : 'a t -> bool

(** The [Deferred] module exposed here is for Async's internal use only. *)
module Deferred : sig

  type +'a t with sexp_of

  module Handler : sig type 'a t with sexp_of end

  val create : ('a ivar -> unit) -> 'a t
  val peek : 'a t -> 'a option
  val is_determined : _ t -> bool
  val return : 'a -> 'a t
  val upon  : 'a t -> ('a -> unit) -> unit
  val bind  : 'a t -> ('a -> 'b t) -> 'b t
  val add_handler : 'a t -> ('a -> unit) -> Execution_context.t -> 'a Handler.t
  val remove_handler : 'a t -> 'a Handler.t -> unit

end

(** [read t] returns a deferred that becomes enabled with value [v] after the ivar is
    filled with [v]. *)
val read : 'a t -> 'a Deferred.t
