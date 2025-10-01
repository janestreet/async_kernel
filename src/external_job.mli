open! Core
open! Import

type 'a inner = 'a Types.External_job.inner =
  { execution_context : Execution_context.t @@ aliased many
  ; f : #(Capsule.Initial.k Capsule.Access.t * 'a) @ unique -> unit
  ; a : 'a @@ many
  }

type t' = Types.External_job.t' = T : 'a inner -> t' [@@unboxed]
type t : value mod contended portable = t' Capsule.Initial.Data.t [@@deriving sexp_of]

module Encapsulated : sig
  val create
    :  execution_context:Execution_context.t Capsule.Initial.Data.t
    -> f:
         (#(Capsule.Initial.k Capsule.Access.t * 'a) @ unique -> unit)
           Capsule.Initial.Data.t
       @ once
    -> a:'a Capsule.Initial.Data.t @ unique
    -> t @ once unique
    @@ portable
end
