open Import

type (+'a, 'execution_context) t with sexp_of
type ('a, 'execution_context) deferred = ('a, 'execution_context) t

val of_ivar : ('a, 'execution_context) Raw_ivar.t -> ('a, 'execution_context) t

val peek : ('a, _) t -> 'a option

module Scheduler_dependent
  (Scheduler : Basic_scheduler)
  (Deferred : Raw
     with type execution_context := Scheduler.Execution_context.t
     with type ('a, 'execution_context) raw := ('a, 'execution_context) t)
  (Ivar : Raw
     with type execution_context := Scheduler.Execution_context.t
     with type ('a, 'execution_context) raw := ('a, 'execution_context) Raw_ivar.t)
  : sig

  type 'a t = 'a Deferred.t with sexp_of

  val create : ('a Ivar.t -> unit) -> 'a t
  val peek : 'a t -> 'a option
  val is_determined : _ t -> bool
  val return : 'a -> 'a t
  val upon  : 'a t -> ('a -> unit) -> unit
  val upon' : 'a t -> ('a -> unit) -> Unregister.t
  val bind  : 'a t -> ('a -> 'b t) -> 'b t
  val install_removable_handler
    : 'a t -> ('a, Scheduler.Execution_context.t) Raw_handler.t -> Unregister.t

  include Raw
    with type execution_context := Scheduler.Execution_context.t
    with type ('a, 'execution_context) raw := ('a, 'execution_context) deferred
    with type 'a t := 'a t

end
