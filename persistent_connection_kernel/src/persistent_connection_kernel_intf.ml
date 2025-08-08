(** An actively maintained connection to some service that eagerly and repeatedly attempts
    to reconnect whenever the underlying connection is lost, until a new one can be
    established. *)

open! Core
open! Async_kernel

(** The address of a service to which one can connect. E.g. [Host_and_port.t] is a
    reasonable choice when making a TCP connection. *)
module type Address = sig
  type t [@@deriving equal, sexp_of]
end

module type Closable = sig
  (** a connection type *)
  type t

  (** [close t] closes the connection. The returned deferred becomes determined once any
      resources needed to maintain the connection have been released. *)
  val close : t -> unit Deferred.t

  (** [is_closed t] returns true if [close] has ever been called (even if the returned
      deferred has not yet been fulfilled).

      Note that some modules implementing [Closable] may call close internally upon
      noticing that the connection was closed by the other side. The interface of such a
      module ought to say that this is the case. *)
  val is_closed : t -> bool

  (** [close_finished t] becomes determined at the same time as the result of the first
      call to [close]. [close_finished] differs from [close] in that it does not have the
      side effect of initiating a close. *)
  val close_finished : t -> unit Deferred.t
end

module type Connection_error = sig
  type t [@@deriving equal ~localize, sexp_of]

  (** Used to convert uncaught exceptions raised in [connect] to a [t] *)
  val of_exception_error : Error.t -> t

  val to_error : t -> Error.t
end

module type S' = sig
  type t [@@deriving sexp_of]

  (** A connection, perhaps embellished with additional information upon connection. *)
  type conn

  (** The error type of the provided [connect] function. Use [S] if this is going to be
      [Error.t] *)
  type conn_error

  module Event : sig
    type 'address t = (conn, conn_error, 'address) Event.t [@@deriving sexp_of]
  end

  (** [create ~server_name ~on_event ~retry_delay get_address] returns a persistent
      connection to a server whose host and port are obtained via [get_address] every time
      we try to connect. For example, [get_address] might look up a server's host and port
      in catalog at a particular path to which multiple redundant copies of a service are
      publishing their location. If one copy dies, we get the address of the another one
      when looking up the address afterwards.

      All connection events (see the type above) are passed to the [on_event] callback, if
      given. When this callback becomes determined, we move on to the next step in our
      connection attempt (e.g. we won't actually attempt to connect until
      [on_event Attempting_to_connect] is finished). Note that [on_event Disconnected]
      will only be called once [on_event (Connected conn)] finishes even if the connection
      goes down during that callback.

      [`Failed_to_connect error] and [`Obtained_address addr] events are only reported if
      they are distinct from the most recent event of the same type that has taken place
      since the most recent [`Attempting_to_connect] event.

      Connection is by default retried after
      [Time.Span.randomize ~percent:(Percent.of_mult 0.3) (retry_delay ())]. The default
      for [retry_delay] is [const (sec 10.)]. Note that what this retry delay actually
      throttles is the delay between two connection attempts, so when a long-lived
      connection dies, connection is usually immediately retried, and if that failed, wait
      for another retry delay and retry.

      The [random_state] and [time_source] arguments are there to make persistent
      connection code more deterministically testable. They default to
      [`State Random.State.default] and [Time_source.wall_clock ()], respectively. If
      random_state is set to [`Non_random], retry_delay will be used directly. *)
  val create
    :  server_name:string
    -> ?on_event:('address Event.t -> unit Deferred.t)
    -> ?retry_delay:(unit -> Time_ns.Span.t)
    -> ?random_state:[ `Non_random | `State of Random.State.t ]
    -> ?time_source:Time_source.t
    -> connect:('address -> (conn, conn_error) Result.t Deferred.t)
    -> address:(module Address with type t = 'address)
    -> (unit -> ('address, conn_error) Result.t Deferred.t)
    -> t

  (** [connected] returns the first available connection from the time it is called. When
      currently connected, the returned deferred is already determined. If [closed] has
      been called, then the returned deferred is never determined. *)
  val connected : t -> conn Deferred.t

  (** [event] returns a bus which is written to whenever an event happens. Since the
      ['address] used in create is not exposed as a parameter of the [t] type, we replace
      it with (). *)
  val event_bus : t -> (unit Event.t -> unit) Bus.Read_only.t

  (** [connected_or_failed_to_connect] is immediately determined as [Ok _] if [t] is
      already connected. Otherwise it becomes determined the next time [t] becomes
      connected or fails to connect or when [t] is closed. *)
  val connected_or_failed_to_connect : t -> conn Or_error.t Deferred.t

  (** The current connection, if any. *)
  val current_connection : t -> conn option

  val server_name : t -> string

  (** [close t] closes the current connection and stops it from trying to reconnect. After
      the deferred it returns becomes determined, the last connection has been closed and
      no others will be attempted.

      Note: no [close] calls are ever generated internally in response to the connection
      being closed by the other side. *)
  include Closable with type t := t

  (** [close_when_current_connection_is_closed t] causes the persistent connection to not
      reconnect if the current connection closes or if it is not currently connected. It
      does not close any active connection. *)
  val close_when_current_connection_is_closed : t -> unit

  module Expert : sig
    (** Provides direct access to the connection state. Compared to calling [connected]
        from the non-[Expert] API, this:

        - Avoids attaching a new handler with each call.
        - Becomes determined if the persistent connection is closed (rather than never
          doing so).
        - Doesn't attempt to prevent a race condition by checking [Conn.is_closed conn]
          and retrying if [false].

        This makes it more suitable for use in a loop calling [Deferred.choose] or
        similar. *)
    val connection : t -> [ `Close_started | `Ok of conn ] Deferred.t

    (** The time source that the connection uses for retry delays *)
    val time_source : t -> Time_source.t
  end
end

module type S = S' with type conn_error := Error.t

module type Persistent_connection_kernel = sig
  module type Address = Address
  module type Closable = Closable
  module type Connection_error = Connection_error
  module type S = S
  module type S' = S'

  module Event = Event
  module Default_connection_error : Connection_error with type t = Error.t

  module Make' (Conn_err : Connection_error) (Conn : Closable) :
    S' with type conn = Conn.t and type conn_error = Conn_err.t

  module Make (Conn : Closable) :
    S with type conn = Conn.t and type t = Make'(Default_connection_error)(Conn).t
end
