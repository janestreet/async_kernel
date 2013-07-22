(** Queues of jobs to run, one at each priority level. *)

open Core.Std
open Import

open Core.Std

type t with sexp_of

include Invariant.S with type t := t

val create : unit -> t

(** [length t] returns the number of waiting jobs *)
val length : t -> int

(** [is_empty t] returns true if there are no waiting jobs. *)
val is_empty : t -> bool

val add : t -> Priority.t -> Job.t -> unit

(** [clear t] removes all jobs from [t]. *)
val clear : t -> unit

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
    priority, as per [~max_num_jobs_per_priority]. *)
val run_all : t -> (Job.t -> unit) -> (unit, exn) Result.t
