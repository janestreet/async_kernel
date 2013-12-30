(** Internal to Async -- see {!Async_unix.Scheduler} for the public API. *)

open Core.Std
open Import

type t with sexp_of

val t : unit -> t

include Invariant.S with type t := t

val current_execution_context  : t -> Execution_context.t
val with_execution_context     : t -> Execution_context.t -> f:(unit -> 'a) -> 'a
val set_execution_context      : t -> Execution_context.t -> unit

val enqueue     : t -> Execution_context.t -> ('a -> unit) -> 'a -> unit
val create_job  : t -> Execution_context.t -> ('a -> unit) -> 'a -> Jobs.Job.t
val enqueue_job : t -> Jobs.Job.t -> free_job:bool -> unit

val main_execution_context : Execution_context.t
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
val set_check_access : t -> (unit -> unit) option -> unit
val check_access : t -> unit
val check_invariants : t -> bool
val set_check_invariants : t -> bool -> unit
val set_record_backtraces : t -> bool -> unit

val add_finalizer     : t -> 'a Heap_block.t -> ('a Heap_block.t -> unit) -> unit
val add_finalizer_exn : t -> 'a              -> ('a              -> unit) -> unit

val set_thread_safe_external_action_hook : t -> (unit -> unit) -> unit

val thread_safe_enqueue_external_action : t -> (unit -> unit) -> unit

val force_current_cycle_to_end : t -> unit

type 'a with_options =
  ?monitor:Monitor.t
  -> ?priority:Priority.t
  -> 'a
val within'   : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
val within    : ((unit -> unit         ) -> unit         ) with_options
val within_v  : ((unit -> 'a           ) -> 'a option    ) with_options
val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
val schedule  : ((unit -> unit         ) -> unit         ) with_options

val preserve_execution_context  : ('a -> unit)          -> ('a -> unit)          Staged.t
val preserve_execution_context' : ('a -> 'b Deferred.t) -> ('a -> 'b Deferred.t) Staged.t

val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t

val find_local : 'a Univ_map.Key.t -> 'a option
val with_local : 'a Univ_map.Key.t -> 'a option -> f:(unit -> 'b) -> 'b

val reset_in_forked_process : unit -> unit
