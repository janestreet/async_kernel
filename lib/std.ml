module Async_config      = Config
module Clock             = Clock
module Condition         = Async_condition
module Deferred          = struct
  (* circular dependency, Async_or_error depends on Monitor which depends on Deferred *)
  include Deferred
  module Or_error = Async_or_error
end
module Execution_context = Execution_context
module Gc                = Async_gc
module Handler           = Handler
module Ivar              = Ivar
module Lazy_deferred     = Lazy_deferred
module Monitor           = Monitor
module Pipe              = Pipe
module Priority          = Priority
module Sequencer         = Throttle.Sequencer
module Stream            = Async_stream
module Tail              = Tail
module Throttle          = Throttle

include Deferred_std

let after        = Clock    .after
let at           = Clock    .at
let catch        = Monitor  .catch
let every        = Clock    .every
let schedule     = Scheduler.schedule
let schedule'    = Scheduler.schedule'
let try_with     = Monitor  .try_with
let with_timeout = Clock    .with_timeout
let within       = Scheduler.within
let within'      = Scheduler.within'

let (>>=?) = Deferred.Result.(>>=)
let (>>|?) = Deferred.Result.(>>|)
