(** Internal to Async -- see {!Deferred} for the public API. *)

[@@@implicit_kind: ('a : value_or_null) * ('b : value_or_null)]

open! Core
open! Import

type +'a t : value mod non_float = 'a Types.Deferred.t [@@deriving sexp_of]
(** @canonical Async_kernel.Deferred.t *)

include Invariant.S1 with type 'a t := 'a t

val of_ivar : 'a Ivar0.t -> 'a t
val create : ('a Ivar0.t -> unit) @ once -> 'a t
val peek : 'a t -> 'a option
val value_exn : 'a t -> 'a
val is_determined : 'a t -> bool
val return : 'a -> 'a t
val upon : 'a t -> ('a -> unit) -> unit
val bind : 'a t -> f:('a -> 'b t) -> 'b t

module Handler : sig
  type 'a t [@@deriving sexp_of]
end

val add_handler : 'a t -> ('a -> unit) -> Execution_context.t -> 'a Handler.t
val remove_handler : 'a t -> 'a Handler.t -> unit
