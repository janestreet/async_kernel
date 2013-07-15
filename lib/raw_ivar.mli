open Core.Std
open Import

type 'a t with sexp_of
type 'a ivar = 'a t

include Invariant.S1 with type 'a t := 'a t

val create : unit -> _ t
val create_full : 'a -> 'a t

val peek     : 'a t -> 'a option
val is_empty : _ t -> bool
val is_full  : _ t -> bool

val equal : 'a t -> 'a t -> bool

val connect : bind_result:'a t -> bind_rhs:'a t -> unit
val fill : 'a t -> 'a  -> unit
val install_removable_handler : 'a t -> 'a Raw_handler.t -> Unregister.t
val upon  : 'a t -> ('a -> unit) -> unit
val upon' : 'a t -> ('a -> unit) -> Unregister.t

val debug_space_leaks : int option ref

val indir : 'a t -> 'a t
