(** Internal to Async -- queues of jobs to run, one at each priority level. *)

open Core.Std
open Import

module Job : sig
  type t with sexp_of

  include Invariant.S with type t := t
end

type t with sexp_of

include Invariant.S with type t := t

val create : unit -> t

(** [length t] returns the number of waiting jobs *)
val length : t -> int

(** [is_empty t] returns true if there are no waiting jobs. *)
val is_empty : t -> bool

val enqueue     : t -> Execution_context.t -> ('a -> unit) -> 'a -> unit
val create_job  : t -> Execution_context.t -> ('a -> unit) -> 'a -> Job.t
val enqueue_job : t -> Job.t -> free_job:bool -> unit

(** [start_cycle t ~max_num_jobs_per_priority] enables subsequent calls of [run_all] to
    run up to [max_num_jobs_per_priority] jobs of each priority level. *)
val start_cycle
  :  t
  -> max_num_jobs_per_priority:Max_num_jobs_per_priority_per_cycle.t
  -> unit

(** [force_current_cycle_to_end] sets the number of normal jobs allowed to run in this
    cycle to zero.  Thus, after the currently running job completes, the scheduler will
    switch to low priority jobs and then end the current cycle. *)
val force_current_cycle_to_end : t -> unit

(** [run_all t f] removes jobs from [t] one at a time and applies [f] to them, stopping as
    soon as an unhandled exception is raised, or when no more jobs can be run at any
    priority, as per [~max_num_jobs_per_priority].  The [external_actions] queue is
    checked at each job, and any thunk that can be dequeued is run immediately. *)
val run_all
  :  t
  -> external_actions:(unit -> unit) Thread_safe_queue.t
  -> (unit, exn) Result.t

val current_execution_context : t -> Execution_context.t

val set_execution_context : t -> Execution_context.t -> unit

val uncaught_exn : t -> Error.t option

val got_uncaught_exn : t -> Error.t -> unit

val global_kill_index : t -> Kill_index.t

val inc_global_kill_index : t -> unit

val num_jobs_run : t -> int
