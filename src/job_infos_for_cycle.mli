(** Store debugging info per job, such as start and end time. This is cleared at the start
    of each cycle. Note that this doesn't differentiate between external jobs, normal
    jobs, and low priority jobs; they all get added to the queues in the order that
    they're run. *)

open! Core
open! Import

type t [@@deriving sexp_of]

include Invariant.S with type t := t

val create : unit -> t
val latest_cycle_start_times : t -> Time_stamp_counter.t Queue.t
val latest_cycle_end_times : t -> Time_stamp_counter.t Queue.t

module Private : sig
  val before_job_run : t -> unit
  val after_job_finished : t -> unit
  val on_exception_in_run_jobs : t -> unit
  val on_cycle_start : t -> unit
end
