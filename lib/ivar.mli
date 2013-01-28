(** An ivar is a write-once cell that can be empty or full (i.e. hold a single value) that
    one can [read] and to obtain a deferred that becomes determined when the ivar is
    filled.  An ivar is similar to an [a option ref], except it is an error to fill an
    already full ivar. *)

open Core.Std

type 'a t = ('a, Execution_context.t) Raw_ivar.t with bin_io, sexp_of
type 'a detailed = 'a t with sexp_of
type 'a deferred = ('a, Execution_context.t) Raw_deferred.t

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

(** [read t] returns a deferred that becomes enabled with value [v] after the ivar is
    filled with [v]. *)
val read : 'a t -> 'a deferred
