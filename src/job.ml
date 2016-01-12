open! Core_kernel.Std
open! Import

type t = Job_pool.slots Pool.Pointer.t [@@deriving sexp_of]

let invariant _ = ()
