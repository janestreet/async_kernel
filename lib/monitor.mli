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
    in the async part of a computation.  For example, in

    upon (f ()) g

    if [f] raises, the exception will not go to a monitor; it will go to the next caml
    exception handler on the stack.  Any exceptions raised by [g] will be caught by the
    scheduler and propagated to the toplevel monitor.  Because of this it is advised to
    always use [Scheduler.schedule] or [Scheduler.within].  For example,

    Scheduler.within (fun () -> upon (f ()) g)

    This code will catch an exception in either [f] or [g], and propagate it to the
    monitor.

    This is only relevent to the toplevel monitor because if you create another monitor
    and you wish to run code within it you have no choice but to use [Scheduler.within].
    [try_with] creates its own monitor and uses [Scheduler.within], so it does not have
    this problem. *)
open Core.Std

type t = Execution_context.t Raw_monitor.t with sexp_of

(** [create ()] returns a new monitor whose parent is the current monitor *)
val create : ?name:string -> unit -> t

(** [name t] returns the name of the monitor, or a unique id if no name was
    supplied to [create]. *)
val name : t -> string

(** [current ()] returns the current monitor *)
val current : unit -> t

(** [errors t] returns a stream of all subsequent errors that monitor [t]
    sees. *)
val errors : t -> exn Raw_async_stream.t

(** [error t] returns a deferred that becomes defined if the monitor ever
    sees an error.  Calling [error t] does not count as "listening for errors",
    and if no one has called [errors t] to listen, then errors will still be
    raised up the monitor tree. *)
val error : t -> exn Deferred.t

(** [extract_exn exn] extracts the exn from an error exn that comes from a monitor.
    If it is not supplied such an error exn, it returns the exn itself. *)
val extract_exn : exn -> exn

(** [has_seen_error t] returns true iff the monitor has ever seen an error. *)
val has_seen_error : t -> bool

(** [send_exn t exn ?backtrace] sends the exception [exn] as an error to be handled
    monitor [t].  By default, the error will not contain a backtrace.  However, the caller
    can supply one using [`This], or use [`Get] to request that [send_exn] obtain one
    using [Exn.backtrace ()]. *)
val send_exn : t -> ?backtrace:[ `Get | `This of string ] -> exn -> unit


(** [try_with f] schedules [f ()] to run in a monitor and returns the result as [Ok x] if
    [f] finishes normally, or returns [Error e] if there is some error.  Once a result is
    returned, any subsequent errors raised by [f ()] are ignored.  [try_with] always
    returns a deferred immediately and does not raise.

    The [name] argument is used to give a name to the monitor the computation will be
    running in.  This name will appear when printing errors. *)
val try_with
  :  ?name : string
  -> (unit -> 'a Deferred.t)
  -> ('a, exn) Result.t Deferred.t

(** [try_with_raise_rest f] is the same as [try_with f], except that subsequent errors
    raised by [f ()] are reraised to the monitor that called [try_with_raise_rest]. *)
val try_with_raise_rest
  :  ?name : string
  -> (unit -> 'a Deferred.t)
  -> ('a, exn) Result.t Deferred.t

(** [handle_errors ?name f handler] runs [f ()] inside a new monitor with the optionally
    supplied name, and calls [handler error] on every error raised to that monitor.  Any
    error raised by [handler] goes to the monitor in effect when [handle_errors] was
    called. *)
val handle_errors
  :  ?name:string
  -> (unit -> 'a Deferred.t)
  -> (exn -> unit)
  -> 'a Deferred.t

(** [catch_stream ?name f] runs [f ()] inside a new monitor [m] and returns the stream of
    errors raised to [m]. *)
val catch_stream : ?name:string -> (unit -> unit) -> exn Raw_async_stream.t

(** [catch ?name f] runs [f ()] inside a new monitor [m] and returns the first error
    raised to [m]. *)
val catch : ?name:string -> (unit -> unit) -> exn Deferred.t

(** [protect f ~finally] runs [f ()] and then [finally] regardless of the success
    or failure of [f].  Re-raises any exception thrown by [f] or returns whatever
    [f] returned.

    The [name] argument is used to give a name to the monitor the computation
    will be running in. This name will appear when printing the errors. *)
val protect
  :  ?name : string
  -> (unit -> 'a Deferred.t)
  -> finally:(unit -> unit Deferred.t) -> 'a Deferred.t

val main : t

module Exported_for_scheduler : sig
  type 'a with_options =
    ?block_group:Block_group.t
    -> ?monitor:t
    -> ?priority:Priority.t
    -> 'a
  val within'   : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
  val within    : ((unit -> unit         ) -> unit         ) with_options
  val within_v  : ((unit -> 'a           ) -> 'a option    ) with_options
  val schedule' : ((unit -> 'a Deferred.t) -> 'a Deferred.t) with_options
  val schedule  : ((unit -> unit         ) -> unit         ) with_options

  val within_context : Execution_context.t -> (unit -> 'a) -> ('a, unit) Result.t
end
