module Debug_in_this_directory = Debug

open Core.Std  let _ = _squelch_unused_module_warning_

module Debug = Debug_in_this_directory

let concat = String.concat

let eprints = Core.Std.Debug.eprints
let eprint  = Core.Std.Debug.eprint

module Poly = Polymorphic_compare

let _squelch_unused_module_warning_ = ()
