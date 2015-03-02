open! Core_kernel.Std
open! Import

type t = Normal | Low with sexp_of

let normal = Normal
let low    = Low
