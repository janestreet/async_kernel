(** Like {!Bvar}, but can be safely transported to and filled from any domain. *)

open! Core
open! Import

type ('a, -'permissions) t : value mod contended portable

(** [create ()] returns an empty bvar. It can only be called from the initial domain, from
    inside the async scheduler. *)
val create : unit -> ('a, read_write) t

(** [wait t] becomes determined by the next call to [broadcast t a]. It can only be called
    from the initial domain, from inside the async scheduler.

    [wait t] can only be called if ['a] mode crosses contention, since we don't know if
    the returned value has come from another capsule. *)
val wait : ('a : value mod contended, [> read ]) t -> 'a Deferred.t

(** [broadcast t a] causes {i all} of the non-determined deferreds returned from [wait t]
    to become determined with [a]. If no such deferreds exist, this operation is a no-op.

    Since [t] is [mod contended portable], [broadcast t a] may be called from any domain,
    meaning the [a] passed in must be [portable]. *)
val broadcast : ('a, [> write ]) t -> 'a @ portable -> unit @@ portable
