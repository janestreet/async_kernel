open! Core

include Deferred1.Monad_sequence with type 'a t := 'a Sequence.t (** @inline *)

val fold_mapi
  :  ?how:Monad_sequence.how
  -> 'a Sequence.t
  -> init:'acc
  -> mapi_f:(int -> 'a -> 'b Deferred1.t)
  -> fold_f:('acc -> 'b -> 'acc)
  -> 'acc Deferred1.t

val count
  :  ?how:Monad_sequence.how
  -> 'a Sequence.t
  -> f:('a -> bool Deferred1.t)
  -> int Deferred1.t

val sum
  :  (module Base.Container.Summable with type t = 'sum)
  -> ?how:Monad_sequence.how
  -> 'a Sequence.t
  -> f:('a -> 'sum Deferred1.t)
  -> 'sum Deferred1.t
