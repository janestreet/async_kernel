module Clock             = Clock
module Deferred          = Deferred
module Execution_context = Execution_context
module Handler           = Handler
module Ivar              = Ivar
module Monitor           = Monitor
module Pipe              = Pipe
module Priority          = Priority
module Stream            = Async_stream
module Tail              = Tail
module Throttle          = Throttle
module Sequencer         = Throttle.Sequencer

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
