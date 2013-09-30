(** Internal to async -- see {!Deferred} for the public API. *)

open Import

type +'a t with sexp_of
type 'a deferred = 'a t

val of_ivar : 'a Raw_ivar.t -> 'a t

val peek : 'a t -> 'a option

val create : ('a Raw_ivar.t -> unit) -> 'a t
val peek : 'a t -> 'a option
val is_determined : _ t -> bool
val return : 'a -> 'a t
val upon  : 'a t -> ('a -> unit) -> unit
val upon' : 'a t -> ('a -> unit) -> Unregister.t
val bind  : 'a t -> ('a -> 'b t) -> 'b t
val install_removable_handler : 'a t -> 'a Raw_handler.t -> Unregister.t
