open! Core
module Deferred := Deferred1

type ('a, 'cmp) t := ('a, 'cmp) Set.t

val for_all : ('a, _) t -> f:('a -> bool Deferred.t) -> bool Deferred.t

(** [how] is exposed here so if [f] has a lot of work that can be done asynchronously, it
    can first be concurrently applied to elements of [t] and only then counted. *)
val count
  :  how:Monad_sequence.how
  -> ('a, _) t
  -> f:('a -> bool Deferred.t)
  -> int Deferred.t

(** [how] is exposed here so if [f] has a lot of work that can be done asynchronously, it
    can first be concurrently applied to elements of [t] and only then summed. *)
val sum
  :  (module Base.Container.Summable with type t = 'sum)
  -> how:Monad_sequence.how
  -> ('a, _) t
  -> f:('a -> 'sum Deferred.t)
  -> 'sum Deferred.t

val find : ('a, _) t -> f:('a -> bool Deferred.t) -> 'a option Deferred.t
val find_map : ('a, _) t -> f:('a -> 'b option Deferred.t) -> 'b option Deferred.t

val map
  :  ('b, 'cmp) Base.Comparator.Module.t
  -> how:Monad_sequence.how
  -> ('a, _) t
  -> f:('a -> 'b Deferred.t)
  -> ('b, 'cmp) t Deferred.t

val filter_map
  :  ('b, 'cmp) Base.Comparator.Module.t
  -> how:Monad_sequence.how
  -> ('a, _) t
  -> f:('a -> 'b option Deferred.t)
  -> ('b, 'cmp) t Deferred.t

val filter
  :  how:Monad_sequence.how
  -> ('a, 'cmp) t
  -> f:('a -> bool Deferred.t)
  -> ('a, 'cmp) t Deferred.t

val fold
  :  ('a, _) t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum Deferred.t)
  -> 'accum Deferred.t

val fold_right
  :  ('a, _) t
  -> init:'accum
  -> f:('a -> 'accum -> 'accum Deferred.t)
  -> 'accum Deferred.t

val iter
  :  how:Monad_sequence.how
  -> ('a, _) t
  -> f:('a -> unit Deferred.t)
  -> unit Deferred.t
