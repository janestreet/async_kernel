(** The context in which an Async job runs. *)

open Core.Std

type t =
  { monitor            : Raw_monitor.t
  ; priority           : Priority.t
  ; local_storage      : Univ_map.t
  ; backtrace_history  : Backtrace.t list
  (** [kill_index] is a "cached" copy of [monitor]'s [kill_index].  This allows the
      scheduler, when deciding whether to run a job, to do a fast check in the common
      case that the execution context's [kill_index] is up to date, i.e. equal to the
      scheduler's [global_kill_index].  If it's not, then the scheduler will update the
      [kill_index] for itself and [monitor] (and [monitor]'s ancestors). *)
  ; mutable kill_index : Kill_index.t
  }
with fields, sexp_of

include Invariant.S with type t := t

val main : t

val create_like
  :  ?monitor:Raw_monitor.t
  -> ?priority:Priority.t
  -> ?local_storage:Univ_map.t
  -> t
  -> t

val find_local : t -> 'a Univ_map.Key.t -> 'a option
val with_local : t -> 'a Univ_map.Key.t -> 'a option -> t

val record_backtrace : t -> t

val is_alive : t -> global_kill_index : Kill_index.t -> bool
