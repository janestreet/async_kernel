(** Internal to Async -- see {!Deferred} for the public API. *)

open Import

type +'a t with sexp_of
type 'a deferred = 'a t

val of_ivar : 'a Raw_ivar.t -> 'a t

val create : ('a Raw_ivar.t -> unit) -> 'a t
val peek : 'a t -> 'a option
val is_determined : _ t -> bool
val return : 'a -> 'a t
val upon  : 'a t -> ('a -> unit) -> unit
val bind  : 'a t -> ('a -> 'b t) -> 'b t

module Handler : sig type 'a t with sexp_of end
val add_handler : 'a t -> ('a -> unit) -> Execution_context.t -> 'a Handler.t
val remove_handler : 'a t -> 'a Handler.t -> unit

