(** Events is for keeping a set of events that need to happen in the future.  One
    can add and remove events, and update the time to find out the events that need to
    happen. *)
open Core.Std

module Event : sig
  type 'a t with sexp_of

  val at : _ t -> Time.t
  val value : 'a t -> 'a
end

type 'a t with sexp_of

val invariant : _ t -> unit

val create : now:Time.t -> _ t

val iter : 'a t -> f:('a Event.t -> unit) -> unit

val is_empty : _ t -> bool

val now : _ t -> Time.t

(** [advance_clock t ~to_] advances the clock to [to_], and returns [`Ok values], with
    values for all events in [t] with [at <= to_].

    [advance_clock] returns [`Not_in_the_future] if [Time.(<=) to_ (now t)] *)
val advance_clock : 'a t -> to_:Time.t -> [ `Not_in_the_future | `Ok of 'a list ]

(** [add t ~at value] adds a new event [e] to [t] with the specified [value], and returns
    [`Ok e].  [add] returns [`Not_in_the_future] if [Time.(<=) at (now t)]. *)
val add : 'a t -> at:Time.t -> 'a -> [ `Not_in_the_future | `Ok of 'a Event.t ]

(** [remove t event] removes [event] from [t] and returns [`Removed], if [event] is
    present in [t], else it returns [`Not_present]. *)
val remove : 'a t -> 'a Event.t -> [ `Not_present | `Removed ]

(** [next_upcoming t] returns the next upcoming event in [t], if any. *)
val next_upcoming : 'a t -> 'a Event.t option
