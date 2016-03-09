(** A time source holds a time (possibly wall-clock time, possibly simulated time) and
    gives the ability to schedule Async jobs to run when that time advances.  There is a
    single wall-clock time source (returned by [wall_clock ()]) that the Async scheduler
    drives and uses for the [Clock_ns] module.  One can also create a user-controlled time
    source via [create], and advance its clock as desired.  This is useful so that state
    machines can depend on a notion of time that is distinct from wall-clock time.
*)

open! Core_kernel.Std
open! Import

module Deferred = Deferred1
module Time_source = Time_source0

module type Time_source = sig

  (** A time source has a phantom read-write parameter, where [write] gives permission to
      call [advance] and [fire_past_alarms]. *)
  module T1 : sig
    type -'rw t = 'rw Time_source.T1.t [@@deriving sexp_of]
  end

  module Read_write : sig
    type t = read_write T1.t [@@deriving sexp_of]
    include Invariant.S with type t := t
  end

  type t = read T1.t [@@deriving sexp_of]

  include Invariant.S with type t := t

  val read_only : [> read] T1.t -> t

  val create
    :  ?timing_wheel_config : Timing_wheel_ns.Config.t
    -> now                  : Time_ns.t
    -> unit
    -> read_write T1.t

  (** A time source with [now t] given by wall-clock time (i.e. [Time_ns.now]) and that is
      advanced automatically as time passes (specifically, at the start of each Async
      cycle). *)
  val wall_clock : unit -> t

  (** Accessors. *)
  val alarm_precision : [> read] T1.t -> Time_ns.Span.t
  val now             : [> read] T1.t -> Time_ns.t

  val advance          : [> write] T1.t -> to_:Time_ns.t -> unit
  val advance_by       : [> write] T1.t -> Time_ns.Span.t -> unit
  val fire_past_alarms : [> write] T1.t -> unit

  module Continue : sig
    type t

    val immediately : t
  end

  val run_repeatedly
    :  ?start             : unit Deferred.t  (** default is [return ()] *)
    -> ?stop              : unit Deferred.t  (** default is [Deferred.never ()] *)
    -> ?continue_on_error : bool             (** default is [true] *)
    -> [> read] T1.t
    -> f:(unit -> unit Deferred.t)
    -> continue : Continue.t
    -> unit

  (** The functions below here are the same as in clock_intf.ml, except they take an
      explicit [t] argument.  See clock_intf.ml for documentation. *)

  val run_at    : [> read] T1.t -> Time_ns.t      -> ('a -> unit) -> 'a -> unit
  val run_after : [> read] T1.t -> Time_ns.Span.t -> ('a -> unit) -> 'a -> unit

  val at    : [> read] T1.t -> Time_ns.t      -> unit Deferred.t
  val after : [> read] T1.t -> Time_ns.Span.t -> unit Deferred.t

  val with_timeout
    :  [> read] T1.t
    -> Time_ns.Span.t
    -> 'a Deferred.t
    -> [ `Timeout
       | `Result of 'a
       ] Deferred.t

  module Event: sig
    type ('a, 'h) t [@@deriving sexp_of]
    type t_unit = (unit, unit) t [@@deriving sexp_of]

    include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

    val scheduled_at : (_, _) t -> Time_ns.t

    val status
      : ('a, 'h) t -> [ `Aborted      of 'a
                      | `Happened     of 'h
                      | `Scheduled_at of Time_ns.t
                      ]

    val run_at    : [> read] T1.t -> Time_ns.     t -> ('z -> 'h) -> 'z -> (_, 'h) t
    val run_after : [> read] T1.t -> Time_ns.Span.t -> ('z -> 'h) -> 'z -> (_, 'h) t

    val abort : ('a, 'h) t -> 'a -> [ `Ok
                                    | `Previously_aborted  of 'a
                                    | `Previously_happened of 'h
                                    ]

    val abort_exn : ('a, 'h) t -> 'a -> unit

    val abort_if_possible : ('a, _) t -> 'a -> unit

    val fired : ('a, 'h) t -> [ `Aborted of 'a | `Happened of 'h ] Deferred.t

    val reschedule_at
      : ('a, 'h) t -> Time_ns.t      -> [ `Ok
                                        | `Previously_aborted     of 'a
                                        | `Previously_happened    of 'h
                                        | `Too_late_to_reschedule
                                        ]
    val reschedule_after
      : ('a, 'h) t -> Time_ns.Span.t -> [ `Ok
                                        | `Previously_aborted     of 'a
                                        | `Previously_happened    of 'h
                                        | `Too_late_to_reschedule
                                        ]

    val at    : [> read] T1.t -> Time_ns.t      -> (_, unit) t
    val after : [> read] T1.t -> Time_ns.Span.t -> (_, unit) t
  end

  val at_varying_intervals
    :  ?stop : unit Deferred.t
    -> [> read] T1.t
    -> (unit -> Time_ns.Span.t)
    -> unit Async_stream.t

  val at_intervals
    :  ?start : Time_ns.t
    -> ?stop  : unit Deferred.t
    -> [> read] T1.t
    -> Time_ns.Span.t
    -> unit Async_stream.t

  val every'
    :  ?start             : unit Deferred.t  (** default is [Deferred.unit] *)
    -> ?stop              : unit Deferred.t  (** default is [Deferred.never ()] *)
    -> ?continue_on_error : bool             (** default is [true] *)
    -> [> read] T1.t
    -> Time_ns.Span.t
    -> (unit -> unit Deferred.t)
    -> unit

  val every
    :  ?start             : unit Deferred.t   (** default is [Deferred.unit] *)
    -> ?stop              : unit Deferred.t   (** default is [Deferred.never ()] *)
    -> ?continue_on_error : bool              (** default is [true] *)
    -> [> read] T1.t
    -> Time_ns.Span.t
    -> (unit -> unit)
    -> unit

  val run_at_intervals'
    :  ?start             : Time_ns.t        (** default is [Time_ns.now ()] *)
    -> ?stop              : unit Deferred.t  (** default is [Deferred.never ()] *)
    -> ?continue_on_error : bool             (** default is [true] *)
    -> [> read] T1.t
    -> Time_ns.Span.t
    -> (unit -> unit Deferred.t)
    -> unit

  val run_at_intervals
    :  ?start             : Time_ns.t        (** default is [Time_ns.now ()] *)
    -> ?stop              : unit Deferred.t  (** default is [Deferred.never ()] *)
    -> ?continue_on_error : bool             (** default is [true] *)
    -> [> read] T1.t
    -> Time_ns.Span.t
    -> (unit -> unit)
    -> unit
end
