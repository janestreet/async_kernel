open! Core

(** Implements ['a option Deferred.t] as a monad, for e.g. [let%bind.Deferred.Option]. *)
include Monad.S with type 'a t = 'a option Deferred0.t
(** @inline *)

(** Implements iterators on ['a option], where [~f] can return a [Deferred.t]. *)
module Container :
  Monad_sequence.S_sequential_unindexed
  with type 'a monad := 'a Deferred0.t
  with type 'a t := 'a option
