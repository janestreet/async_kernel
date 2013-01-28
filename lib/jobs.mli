open Core.Std

module Priority : sig
  type t with sexp_of

  val normal : t
  val low : t
  val to_string : t -> string
end

(* type of the jobs *)
type 'job t with sexp_of

val create : unit -> _ t

(** [length t] returns the number of waiting jobs *)
val length : _ t -> int

(** [is_empty t] returns true if there are no waiting jobs. *)
val is_empty : _ t -> bool

val add : 'job t -> Priority.t -> 'job -> unit

(** [start_cycle t ~max_num_jobs_per_priority] enables subsequent calls of [get] to
    return up to [max_num_jobs_per_priority] jobs of each priority level. *)
val start_cycle : _ t -> max_num_jobs_per_priority:int -> unit

(** [get t] gets all the jobs with the highest priority currently available, subject to
    [max_num_jobs_per_priority].  The obtained jobs are removed from [t]. *)
val get : 'job t -> 'job list
