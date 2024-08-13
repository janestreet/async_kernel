open! Core
open! Import
module Clock_ns = Clock_ns
module Time_ns = Time_ns
module Time = Time_float
module Async_kernel_scheduler = Async_kernel_scheduler
module Date = Date

let after = Clock_ns.after
let at = Clock_ns.at
let every = Clock_ns.every
let with_timeout = Clock_ns.with_timeout
let with_timeout_exn = Clock_ns.with_timeout_exn
