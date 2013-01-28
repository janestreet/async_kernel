open Import

module Handler : sig
  type ('a, 'execution_context) t
end

type ('a, 'execution_context) t with sexp_of
type ('a, 'execution_context) ivar = ('a, 'execution_context) t

val create      : unit -> ('a, _) t

val peek     : ('a, _) t -> 'a option
val is_empty : (_, _) t -> bool
val is_full  : (_, _) t -> bool

module Scheduler_dependent
  (Scheduler : Basic_scheduler)
  (Ivar : Raw
    with type execution_context := Scheduler.Execution_context.t
    with type ('a, 'execution_context) raw := ('a, 'execution_context) ivar)
  : sig

  type 'a t = 'a Ivar.t with sexp_of

  val equal : 'a t -> 'a t -> bool
  val create      : unit -> 'a t
  val create_full :   'a -> 'a t
  val peek     : 'a t -> 'a option
  val is_empty : _ t -> bool
  val is_full  : _ t -> bool

  val connect : bind_result:'a t -> bind_rhs:'a t -> unit
  val fill : 'a t -> 'a  -> unit
  val install_removable_handler
    : 'a t -> ('a, Scheduler.Execution_context.t) Raw_handler.t -> Unregister.t
  val upon  : 'a t -> ('a -> unit) -> unit
  val upon' : 'a t -> ('a -> unit) -> Unregister.t

  include Raw
    with type execution_context := Scheduler.Execution_context.t
    with type ('a, 'execution_context) raw := ('a, 'execution_context) ivar
    with type 'a t := 'a t

end

val debug_space_leaks : int option ref
