open Core.Std

(** A [Jobs.t] has a queue of jobs at each priority level. *)
module Priority : sig
  type t with sexp_of

  val normal : t
  val low : t
  val to_string : t -> string
end

type 'job t with sexp_of

val invariant : _ t -> unit

val create : dummy:'a -> 'a t

(** [length t] returns the number of waiting jobs *)
val length : _ t -> int

(** [is_empty t] returns true if there are no waiting jobs. *)
val is_empty : _ t -> bool

val add : 'job t -> Priority.t -> 'job -> unit

(** [clear t] removes all jobs from [t]. *)
val clear : _ t -> unit

(** [start_cycle t ~max_num_jobs_per_priority] enables subsequent calls of [run_all] to
    run up to [max_num_jobs_per_priority] jobs of each priority level. *)
val start_cycle : _ t -> max_num_jobs_per_priority:int -> unit

(** [run_all t f] removes jobs from [t] one at a time and applies [f] to them, stopping as
    soon as an unhandled exception is raised, or when no more jobs can be run at any
    priority, as per [~max_num_jobs_per_priority]. *)
val run_all : 'job t -> ('job -> unit) -> (unit, exn) Result.t
