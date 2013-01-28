open Core.Std

val invariant : unit -> unit
val current_execution_context  : unit -> Execution_context.t
val initialize_execution_context : Block_group.t -> unit
val with_execution_context     : Execution_context.t -> f:(unit -> 'a) -> 'a
val add_job                    : Execution_context.t -> ('a -> unit) -> 'a -> unit
val main_execution_context     : unit -> Execution_context.t
val cycle_start : unit -> Time.t
val run_cycle : unit -> [ `Jobs_remain | `No_jobs_remain ]
val run_cycles_until_no_jobs_remain : unit -> unit
val next_upcoming_event : unit -> Time.t option
val uncaught_exception : unit -> Error.t option
val num_pending_jobs : unit -> int
val num_jobs_run : unit -> int
val cycle_times : unit -> Time.Span.t Async_stream.t
val cycle_num_jobs : unit -> int Async_stream.t
val cycle_count : unit -> int
val set_max_num_jobs_per_priority_per_cycle : int -> unit


type 'a with_options =
  ?block_group:Block_group.t
  -> ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> 'a
val within'   : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
val within    : ((unit -> unit         ) -> unit         ) with_options
val within_v  : ((unit -> 'a           ) -> 'a option    ) with_options
val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
val schedule  : ((unit -> unit         ) -> unit         ) with_options

val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t
