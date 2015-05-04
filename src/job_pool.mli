open Core_kernel.Std
open Import

type slots = (Execution_context.t, Obj.t -> unit, Obj.t) Pool.Slots.t3 with sexp_of

type t = slots Pool.t with sexp_of

include Invariant.S with type t := t

val create : unit -> t
