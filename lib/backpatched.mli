(** A [Backpatched] value is either a simple value or a "hole", which may or may not have
    been filled.  One can first define a hole and pass it around, and then later fill it
    in (i.e. backpatch it).  This is useful when there is a cycle in the definitions of
    some entities, but the cycle can be resolved (by backpatching) before the value in
    the hole needs to be accessed dynamically. *)
open Core.Std
open Import

module Hole : sig
  type 'a t

  val create : name:string -> 'a t

  (** It is an error to [fill] an already-filled hole. *)
  val fill : 'a t -> 'a -> unit Or_error.t

  (** It is not an error to [empty] an already-empty hole. *)
  val empty : _ t -> unit
end

type 'a t with sexp_of

val create : 'a -> 'a t

val of_hole : 'a Hole.t -> 'a t

(** It is an error to [get] from a hole that has not been filled. *)
val get_exn : 'a t -> 'a


