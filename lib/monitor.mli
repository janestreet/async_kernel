(** A monitor is a context that determines what to do when there is an unhandled
    exception.  Every Async computation runs within the context of some monitor, which,
    when the computation is running, is referred to as the "current" monitor.  Monitors
    are arranged in a tree -- when a new monitor is created, it is a child of the current
    monitor.

    One can listen to a monitor using Monitor.errors to learn when the monitor sees an
    error.

    If a computation raises an unhandled exception, the current monitor does one of two
    things.  If anyone is listening to the monitor (i.e. Monitor.errors has been called on
    the monitor), then the error stream is extended, and the listeners are responsible for
    doing something.  If no one is "listening" to the monitor, then the exception is
    raised to monitor's parent.  The initial monitor, i.e. the root of the monitor tree,
    prints an unhandled-exception message and calls exit 1.

    **************** NOTE ABOUT THE TOPLEVEL MONITOR ****************

    It is important to note that in the toplevel monitor, exceptions will only be caught
    in the async part of a computation.  For example, in:

    {[
      upon (f ()) g
    ]}

    if [f] raises, the exception will not go to a monitor; it will go to the next caml
    exception handler on the stack.  Any exceptions raised by [g] will be caught by the
    scheduler and propagated to the toplevel monitor.  Because of this it is advised to
    always use [Scheduler.schedule] or [Scheduler.within].  For example:

    {[
      Scheduler.within (fun () -> upon (f ()) g)
    ]}

    This code will catch an exception in either [f] or [g], and propagate it to the
    monitor.

    This is only relevant to the toplevel monitor because if you create another monitor
    and you wish to run code within it you have no choice but to use [Scheduler.within].
    [try_with] creates its own monitor and uses [Scheduler.within], so it does not have
    this problem. *)
open Core.Std

type t = Raw_monitor.t with sexp_of

type 'a with_optional_monitor_name =
  ?here : Source_code_position.t
  -> ?info : Info.t
  -> ?name : string
  -> 'a

(** [create ()] returns a new monitor whose parent is the current monitor. *)
val create : (unit -> t) with_optional_monitor_name

(** [name t] returns the name of the monitor, or a unique id if no name was supplied to
    [create]. *)
val name : t -> Info.t

(** [current ()] returns the current monitor *)
val current : unit -> t

(** [errors t] returns a stream of all subsequent errors that monitor [t] sees. *)
val errors : t -> exn Tail.Stream.t

(** [error t] returns a deferred that becomes defined if the monitor ever sees an error.
    Calling [error t] does not count as "listening for errors", and if no one has called
    [errors t] to listen, then errors will still be raised up the monitor tree. *)
val error : t -> exn Deferred.t

(** [extract_exn exn] extracts the exn from an error exn that comes from a monitor.  If it
    is not supplied such an error exn, it returns the exn itself. *)
val extract_exn : exn -> exn

(** [has_seen_error t] returns true iff the monitor has ever seen an error. *)
val has_seen_error : t -> bool

(** [send_exn t exn ?backtrace] sends the exception [exn] as an error to be handled
    monitor [t].  By default, the error will not contain a backtrace.  However, the caller
    can supply one using [`This], or use [`Get] to request that [send_exn] obtain one
    using [Exn.backtrace ()]. *)
val send_exn : t -> ?backtrace:[ `Get | `This of string ] -> exn -> unit


(** [try_with f] runs [f ()] in a monitor and returns the result as [Ok x] if [f] finishes
    normally, or returns [Error e] if there is some error.  It either runs [f] now, if
    [run = `Now], or schedules a job to run [f], if [run = `Schedule].  Once a result is
    returned, the rest of the errors raised by [f] are ignored or re-raised, as per
    [rest].  [try_with] never raises synchronously, and may only raise asynchronously with
    [rest = `Raise].

    The [name] argument is used to give a name to the monitor the computation will be
    running in.  This name will appear when printing errors.

    If [extract_exn = true], then in an [Error exn] result, the [exn] will be the actual
    exception raised by the computation.  If [extract_exn = false], then the [exn] will
    include additional information, like the monitor and backtrace.  One typically wants
    [extract_exn = false] due to the additional information.  However, sometimes one wants
    the concision of [extract_exn = true]. *)
val try_with
  : (?extract_exn : bool (** default is [false] *)
     -> ?run : [ `Now | `Schedule ]  (** default is [`Schedule] *)
     -> ?rest : [ `Ignore | `Raise ] (** default is [`Ignore] *)
     -> (unit -> 'a Deferred.t)
     -> ('a, exn) Result.t Deferred.t
  ) with_optional_monitor_name

(** [try_with_rest_handling] determines how [try_with f ~rest] determines the [rest] value
    it actually uses.  If [!try_with_rest_handling = `Default d], then [d] is the default
    value for [rest], but can be overriden by supplying [rest] to [try_with].  If
    [!try_with_rest_handling = Force f], then the [rest] supplied to [try_with] is not
    used, and [f] is.

    Initially, [!try_with_rest_handling = `Default `Ignore]. *)
val try_with_rest_handling
  : [ `Default of [ `Ignore | `Raise ]
    | `Force of   [ `Ignore | `Raise ]
    ] ref

(** [try_with_ignored_exn_handling] describes what should happen when [try_with]'s [rest]
    value is [`Ignore], as determined by [!try_with_rest_handling] and the [~rest]
    supplied to [try_with].

    Initially, [!try_with_ignored_exn_handling = `Ignore]. *)
val try_with_ignored_exn_handling
  : [ `Ignore              (* really ignore the exception *)
    | `Eprintf             (* eprintf the exception *)
    | `Run of exn -> unit  (* apply the function to the exception *)
    ] ref

(** [handle_errors ?name f handler] runs [f ()] inside a new monitor with the optionally
    supplied name, and calls [handler error] on every error raised to that monitor.  Any
    error raised by [handler] goes to the monitor in effect when [handle_errors] was
    called. *)
val handle_errors
  : ((unit -> 'a Deferred.t)
     -> (exn -> unit)
     -> 'a Deferred.t
  ) with_optional_monitor_name

(** [catch_stream ?name f] runs [f ()] inside a new monitor [m] and returns the stream of
    errors raised to [m]. *)
val catch_stream : ((unit -> unit) -> exn Tail.Stream.t) with_optional_monitor_name

(** [catch ?name f] runs [f ()] inside a new monitor [m] and returns the first error
    raised to [m]. *)
val catch : ((unit -> unit) -> exn Deferred.t) with_optional_monitor_name

(** [protect f ~finally] runs [f ()] and then [finally] regardless of the success or
    failure of [f].  It re-raises any exception thrown by [f] or returns whatever [f]
    returned.

    The [name] argument is used to give a name to the monitor the computation will be
    running in.  This name will appear when printing the errors. *)
val protect
  : ((unit -> 'a Deferred.t)
     -> finally:(unit -> unit Deferred.t)
     -> 'a Deferred.t
  ) with_optional_monitor_name

val main : t

(** [kill t] causes [t] and all of [t]'s descendants to never start another job.  The job
    that calls [kill] will complete, even if it is a descendant of [t].

    [kill] can break user expectations.  For example, users expect in [protect f ~finally]
    that [finally] will eventually run.  However, if the monitor in which [finally] would
    run is killed, then [finally] will never run. *)
val kill : t -> unit

(** [is_alive t] returns [true] iff none of [t] or its ancestors have been killed. *)
val is_alive : t -> bool

module Exported_for_scheduler : sig
  type 'a with_options =
    ?monitor:t
    -> ?priority:Priority.t
    -> 'a
  val within'   : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
  val within    : ((unit -> unit         ) -> unit         ) with_options
  val within_v  : ((unit -> 'a           ) -> 'a option    ) with_options
  val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
  val schedule  : ((unit -> unit         ) -> unit         ) with_options

  val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t

  val preserve_execution_context  : ('a -> unit)          -> ('a -> unit)          Staged.t
  val preserve_execution_context' : ('a -> 'b Deferred.t) -> ('a -> 'b Deferred.t) Staged.t

end
