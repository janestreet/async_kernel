open Core.Std
open Import

type (+'a, 'execution_context) t
type ('a, 'execution_context) deferred = ('a, 'execution_context) t

val of_ivar : ('a, 'execution_context) Raw_ivar.t -> ('a, 'execution_context) t

val create : (('a, 'execution_context) Raw_ivar.t -> unit) -> ('a, 'execution_context) t

val peek : ('a, _) t -> 'a option

val is_determined : ('a, _) t -> bool

val return : 'a -> ('a, _) t

module Scheduler_dependent (Scheduler : Basic_scheduler) : sig
  type 'a t = ('a, Scheduler.Execution_context.t) deferred with sexp_of
  type 'a detailed = 'a t with sexp_of

  val upon  : 'a t -> ('a -> unit) -> unit
  val upon' : 'a t -> ('a -> unit) -> Unregister.t
  val bind  : 'a t -> ('a -> 'b t) -> 'b t
  val install_removable_handler
    : 'a t -> ('a, Scheduler.Execution_context.t) Raw_handler.t -> Unregister.t
end
