(** A synchronous version of [Async_kernel.Time_source].  [advance_by_alarms] runs
    alarms immediately, rather than enqueueing Async jobs. *)

open! Core_kernel
open! Import

module T1 : sig
  type -'rw t [@@deriving sexp_of]
end

module Read_write : sig
  type t = read_write T1.t [@@deriving sexp_of]
  include Invariant.S with type t := t
end

type t = read T1.t [@@deriving sexp_of]

include Invariant.S with type t := t

val read_only : [> read] T1.t -> t

type callback = unit -> unit

(** [create ~now ()] creates a new time source.  The default [timing_wheel_config] has 100
    microsecond precision, with levels of >1s, >1m, >1h, >1d.  [wrap_callback], if
    supplied, will be applied to each callback that is added. *)
val create
  :  ?timing_wheel_config : Timing_wheel_ns.Config.t
  -> ?wrap_callback       : (callback -> callback Staged.t)
  -> now : Time_ns.t
  -> unit
  -> read_write T1.t

val alarm_precision : [> read] T1.t -> Time_ns.Span.t

(** [is_wall_clock] reports whether this time source represents 'wall clock' time, or some
    alternate source of time. *)
val is_wall_clock : [> read] T1.t -> bool

(** The behavior of [now] is special for [wall_clock ()]; it always calls [Time_ns.now
    ()], so it can return times that the time source has not yet been advanced to. *)
val now : [> read] T1.t -> Time_ns.t

(** [advance_by_alarms t ~to_] advances [t]'s time to [to_], running callbacks for all
    alarms in [t] whose [at <= to_].  If [to_ <= now t], then [now t] does not change (and
    in particular does not go backward), but alarms with [at <= to_] may still may
    fire. *)
val advance_by_alarms : [> write] T1.t -> to_:Time_ns.t -> unit Or_error.t

(** [run_at t at f] schedules an alarm that will run [f] during the next subsequent
    [advance_by_alarms t ~to_] that causes [now t >= at].  If [at <= now t], then [f] will
    to run at the next call to [advance_by_alarms].  [f] is allowed to do all
    [Synchronous_time_source] operations except for [advance_by_alarms] (because [f] is
    already running during [advance_by_alarms].  Adding alarms is not zero-alloc and the
    underlying events live in the OCaml heap. *)
val run_at : [> read] T1.t -> Time_ns.t -> callback -> unit

(** [run_after t span f] is [run_at t (now t + span) f]. *)
val run_after : [> read] T1.t -> Time_ns.Span.t -> callback -> unit

(** [run_at_intervals t span f] causes [f] to run at intervals [now t + k * span], for
    k = 0, 1, 2, etc.  [run_at_intervals] raises if [span < alarm_precision t]. *)
val run_at_intervals : [> read] T1.t -> Time_ns.Span.t -> callback -> unit

module Event : sig
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  (** These are like the corresponding [run_*] functions, except they return an event that
      one can later [abort]. *)
  val at           : [> read] T1.t -> Time_ns.t      -> callback -> t
  val after        : [> read] T1.t -> Time_ns.Span.t -> callback -> t
  val at_intervals : [> read] T1.t -> Time_ns.Span.t -> callback -> t

  (** [abort t] aborts the event [t], if possible, and returns [Ok] if the event was
      aborted, or [Error] with the reason it could not be aborted. *)
  val abort : [> read] T1.t -> t -> unit Or_error.t
end

val default_timing_wheel_config : Timing_wheel_ns.Config.t
