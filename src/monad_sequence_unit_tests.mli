open! Core_kernel.Std
open! Import

module Deferred = Deferred1

module Make
    (M : sig type 'a t [@@deriving compare, sexp_of] end)
    (S : Deferred.Monad_sequence with type 'a t := 'a M.t)
  : sig end
