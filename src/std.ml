open! Core_kernel.Std
open! Import

module Clock_ns          = Clock_ns
module Condition         = Async_condition
module Deferred          = Deferred
module Execution_context = Execution_context
module Gc                = Async_gc
module Handler           = Handler
module Ivar              = Ivar
module Quickcheck_intf   = Async_quickcheck_intf
module Quickcheck        = Async_quickcheck
module Lazy_deferred     = Lazy_deferred
module Monad_sequence    = Monad_sequence
module Monitor           = Monitor
module Pipe              = Pipe
module Priority          = Priority
module Sequencer         = Throttle.Sequencer
module Stream            = Async_stream
module Tail              = Tail
module Throttle          = Throttle


let after          = Clock_ns.after
let at             = Clock_ns.at
let catch          = Monitor.catch
let choice         = Deferred.choice
let choose         = Deferred.choose
let don't_wait_for = Deferred.don't_wait_for
let every          = Clock_ns.every
let never          = Deferred.never
let return         = Deferred.return
let schedule       = Scheduler.schedule
let schedule'      = Scheduler.schedule'
let try_with       = Monitor.try_with
let upon           = Deferred.upon
let with_timeout   = Clock_ns.with_timeout
let within         = Scheduler.within
let within'        = Scheduler.within'

let ( >>>  ) = Deferred.Infix. ( >>> )
let ( >>=? ) = Deferred.Result.( >>= )
let ( >>|? ) = Deferred.Result.( >>| )

include (Deferred : sig include Monad.Infix with type 'a t := 'a Deferred.t end)

include Deferred.Let_syntax
