open! Core
include Deferred1.Monad_sequence with type 'a t := 'a list

val fold_until
  :  'a list
  -> init:'acc
  -> f:('acc -> 'a -> ('acc, 'final) Continue_or_stop.t Deferred1.t)
  -> finish:('acc -> 'final Deferred1.t)
  -> 'final Deferred1.t
