open! Core_kernel.Std
open! Import

type t = Job_pool.slots Pool.Pointer.t with sexp_of

let invariant _ = ()
