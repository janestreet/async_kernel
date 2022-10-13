(** An applicative type for concurrent computations with limited concurrency.

    The computation takes place lazily and only when the [run] function is called.

    Compared to how [Throttle] is typically used, this type lets you avoid an up-front
    time&memory cost of adding all concurrent jobs to the throttle. In particular you can
    implement a throttled traversal of a very large data structure without doing a
    synchronous walk over the entire structure at any given time.
*)

open! Core
module Deferred := Deferred1

type 'a t

include Applicative with type 'a t := 'a t

(** [job f] takes a function of type [unit -> 'a Defferred.t] and converts it to a
    [Throttled] type than can be executed with throttling using [val run].

    The function will be holding one throttle token while running. *)
val job : (unit -> 'a Deferred.t) -> 'a t

(** [run t] takes a [Throttled] type and runs it. *)
val run : 'a t -> max_concurrent_jobs:int -> 'a Deferred.t

(** [of_thunk thunk] converts a function of type [unit -> 'a t] to type ['a t].
    Useful for delaying computation. *)
val of_thunk : (unit -> 'a t) -> 'a t

(** [both_unit t1 t2] combines two [unit t] into one in a way that's more efficent
    by saving the mapping over the final deferred.

    This optimization is important if [t1] finishes early but [t2] finishes late, since
    the memory usage between the two events is reduced to 0. *)
val both_unit : unit t -> unit t -> unit t
