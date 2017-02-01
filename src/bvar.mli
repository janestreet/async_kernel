(** A [Bvar] is a synchronization point that allows one to [broadcast] a value to clients
    [wait]ing on the broadcast.  With a [Bvar], one can efficiently notify multiple
    clients of edge-triggered conditions, repeating as each edge trigger occurs.

    [Bvar] is like an ivar/deferred, except that it is always "empty" and can be
    repeatedly "filled" (via [broadcast]).

    Another way to view [Bvar] is as a restriction of [Condition] that supports only
    broadcast, not [signal]ing a single waiter.  Dropping [signal] simplifies the
    implementation significantly. *)

open! Core_kernel
open! Import

type 'a t = 'a Types.Bvar.t [@@deriving sexp_of]

include Invariant.S1 with type 'a t := 'a t

val create : unit -> 'a t

(** [wait t] becomes determined by the next call to [broadcast t a]. *)
val wait : 'a t -> 'a Deferred0.t

val broadcast : 'a t -> 'a -> unit

(** [has_any_waiters t] returns [true] iff there has been a call to [wait t] since the
    most recent call to [broadcast t]. *)
val has_any_waiters : 'a t -> bool
