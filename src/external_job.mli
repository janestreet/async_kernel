open! Core
open! Import

type 'a inner = 'a Types.External_job.inner =
  { execution_context : Execution_context.t
  ; f : 'a -> unit
  ; a : 'a
  }

type t' = Types.External_job.t' = T : 'a inner -> t' [@@unboxed]
type t : value mod contended portable = t' Capsule.Initial.Data.t [@@deriving sexp_of]

module Encapsulated : sig
  val create
    :  execution_context:Execution_context.t Capsule.Initial.Data.t
    -> f:('a -> unit) Capsule.Initial.Data.t
    -> a:'a Capsule.Initial.Data.t
    -> t
    @@ portable
end
