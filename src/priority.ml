open! Core
open! Import

type t =
  | Normal
  | Low
[@@deriving sexp_of, enumerate]

let normal = Normal
let low = Low
