open! Core_kernel
open! Async_kernel
open! Import

(** @open *)
include module type of Eager_deferred1

(** Intended usage is to [open Eager_deferred.Use] to shadow operations from the non-eager
    world and rebind them to their eager counterparts. *)
module Use : sig
  module Deferred : sig
    type 'a t = 'a Deferred.t

    include module type of Eager_deferred1
  end

  include Monad.Infix with type 'a t := 'a Deferred.t
  include module type of Deferred.Let_syntax

  val upon : 'a Deferred.t -> ('a -> unit) -> unit
  val ( >>> ) : 'a Deferred.t -> ('a -> unit) -> unit

  val ( >>=? )
    :  ('a, 'e) Result.t Deferred.t
    -> ('a -> ('b, 'e) Result.t Deferred.t)
    -> ('b, 'e) Result.t Deferred.t

  val ( >>|? )
    :  ('a, 'e) Result.t Deferred.t
    -> ('a -> 'b)
    -> ('b, 'e) Result.t Deferred.t
end
