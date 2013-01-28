(** A handler is a continuation that captures the current async execution context.  It can
    be scheduled for future invocation by installing it in a deferred. *)
type 'a t

(** [create k] creates a handler by coupling the continuation [k] together with the
    evaluation context that is current at the time [create] is called.  Whenever this
    handler is later invoked, it will happen in this saved evaluation context.  *)
val create : ('a -> unit) -> 'a t

(** [prepend h ~f] pre-composes the handler with the function [f] *)
val prepend : 'a t -> f:('b -> 'a) -> 'b t

(** [filter h ~f] makes the eventual execution of the handler [h] on a value [v] dependent
    on whether predicate [f] holds of [v] *)
val filter : 'a t -> f:('a -> bool) -> 'a t

(** [install h d] behaves like [upon] except that it also returns a uninstall function
    that, when called, uninstalls the handler *)
val install : 'a t -> 'a Deferred.t -> (unit -> unit)

(** [schedule h v] schedules the handler [h] to run at some point in the future by being
    called on value [v] *)
val schedule : 'a t -> 'a -> unit
