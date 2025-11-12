(** Like {!Ivar}, but can be safely transported to and filled from any domain. *)

open! Core
open! Import

type 'a t : value mod contended portable

(** [create ()] returns an empty ivar. It can only be called from the initial domain, from
    inside the async scheduler. *)
val create : unit -> 'a t

(** [fill_if_empty t v] fills [t] with [v] if [t] is empty. If [t] is full, then
    [fill_if_empty] does nothing.

    Filling happens asynchronously in an async cycle, so you cannot assume that the ivar
    is full immediately after calling this function.

    [fill_if_empty] is safe to be run from any domain. *)
val fill_if_empty : 'a t -> 'a @ portable -> unit @@ portable

(** [read t] returns a deferred that becomes enabled with value [v] after the ivar is
    filled with [v]. It can only be called from the initial domain, from inside the async
    scheduler.

    [read t] can only be called if ['a] mode crosses contention, since we don't know if
    the returned value has come from another capsule. *)
val read : ('a : value mod contended). 'a t -> 'a Deferred.t
