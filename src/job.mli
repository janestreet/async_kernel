open! Core_kernel.Std
open! Import

type t = Types.Job.t [@@deriving sexp_of]

include Invariant.S with type t := t
