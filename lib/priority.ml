open Core.Std  let _ = _squelch_unused_module_warning_
open Import

type t = Normal | Low with sexp_of

let normal = Normal
let low    = Low
