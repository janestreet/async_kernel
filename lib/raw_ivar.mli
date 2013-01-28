open Core.Std
open Import

module Handler : sig
  type ('a, 'execution_context) t
end

type ('a, 'execution_context) t with sexp_of
type ('a, 'execution_context) ivar = ('a, 'execution_context) t

val equal : ('a, 'b) t -> ('a, 'b) t -> bool

val create      : unit -> ('a, _) t
val create_full : 'a   -> ('a, _) t

val peek     : ('a, _) t -> 'a option
val is_empty : ('a, _) t -> bool
val is_full  : ('a, _) t -> bool

module Scheduler_dependent (Scheduler : Basic_scheduler) : sig
  type 'a t = ('a, Scheduler.Execution_context.t) ivar with sexp_of
  type 'a detailed = 'a t with sexp_of

  val connect : bind_result:'a t -> bind_rhs:'a t -> unit
  val fill : 'a t -> 'a  -> unit
  val install_removable_handler
    : 'a t -> ('a, Scheduler.Execution_context.t) Raw_handler.t -> Unregister.t
  val upon  : 'a t -> ('a -> unit) -> unit
  val upon' : 'a t -> ('a -> unit) -> Unregister.t
end

val debug_space_leaks : int option ref
