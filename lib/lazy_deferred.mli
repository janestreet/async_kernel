(** A delayed computation that can produce a deferred.

    Nothing happens with a lazy deferred unless one [force]s it.  Forcing a lazy deferred
    starts the computation, which will eventually cause the deferred to become determined.
    As usual with laziness, multiply forcing a lazy deferred is no different than forcing
    it a single time.

    Exceptions (both synchronous and asynchronous) raised by a delayed computation are
    returned by [force] ([wait], [peek], etc.), or will be raised to the monitor in effect
    when [force_exn] ([wait_exn], [peek_exn], etc.) was called. *)

open Core.Std

type 'a t

(** [create f] creates a new lazy deferred that will call [f] when it is forced. *)
val create : (unit -> 'a Deferred.t) -> 'a t

(** [force t] forces evaluation of [t] and returns a deferred that becomes determined
    when the deferred computation becomes determined or raises. *)
val force     : 'a t -> ('a, exn) Result.t Deferred.t
val force_exn : 'a t -> 'a Deferred.t

(** [wait t] waits for [t] to be forced.  If no one ever calls [force t], [wait] will wait
    forever. *)
val wait     : 'a t -> ('a, exn) Result.t Deferred.t
val wait_exn : 'a t -> 'a Deferred.t

(** [bind t f] in the lazy-deferred monad creates a computation that, when forced, will
    force [t], apply [f] to the result, and then force the result of that. *)
include Monad with type 'a t := 'a t

(** [bind'] differs from [bind] in that the supplied function produces an ['a Deferred.t]
    rather than an ['a t]. *)
val bind' : 'a t -> ('a -> 'b Deferred.t) -> 'b t

(** [follow t f] returns a new lazy deferred almost like [bind'] with the notable
    difference that its computation will start as soon as the deferred it is following
    becomes [determined].  Since the resulting deferred depends on the ['a] value computed
    by [t] forcing the resulting of [follow] will force the compuation of [t]. *)
val follow : 'a t -> ('a -> 'b Deferred.t) -> 'b t

(** Read-only operations. *)

(** [peek t = Deferred.peek (wait t)] *)
val peek     : 'a t -> ('a, exn) Result.t option
val peek_exn : 'a t -> 'a option

val is_determined : _ t -> bool

val is_forced : _ t -> bool
