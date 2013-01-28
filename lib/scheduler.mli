open Core.Std

type t with sexp_of

val t : unit -> t

val invariant : t -> unit
val current_execution_context  : t -> Execution_context.t
val with_execution_context     : t -> Execution_context.t -> f:(unit -> 'a) -> 'a

(** [preserve_execution_context f] saves the current execution context and returns a
    function [g] such that [g a] adds a job that runs [f a] in the saved execution
    context. *)
val preserve_execution_context : t -> ('a -> unit) -> ('a -> unit) Staged.t

val add_job                    : Execution_context.t -> ('a -> unit) -> 'a -> unit
val main_execution_context     : Execution_context.t
val cycle_start : t -> Time.t
val run_cycle : t -> unit
val run_cycles_until_no_jobs_remain : unit -> unit
val next_upcoming_event : t -> Time.t option
val uncaught_exn : t -> Error.t option
val num_pending_jobs : t -> int
val num_jobs_run : t -> int
val cycle_times : t -> Time.Span.t Async_stream.t
val cycle_num_jobs : t -> int Async_stream.t
val cycle_count : t -> int
val set_max_num_jobs_per_priority_per_cycle : t -> int -> unit
val set_check_access : t -> (unit -> unit) -> unit
val check_access : t -> unit

val add_finalizer     : t -> 'a Heap_block.t -> ('a Heap_block.t -> unit) -> unit
val add_finalizer_exn : t -> 'a              -> ('a              -> unit) -> unit

val set_thread_safe_finalizer_hook : t -> (unit -> unit) -> unit

type 'a with_options =
  ?work_group:Work_group.t
  -> ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> 'a
val within'   : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
val within    : ((unit -> unit         ) -> unit         ) with_options
val within_v  : ((unit -> 'a           ) -> 'a option    ) with_options
val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
val schedule  : ((unit -> unit         ) -> unit         ) with_options

val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t

val reset_in_forked_process : unit -> unit
