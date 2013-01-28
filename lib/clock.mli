(** Clock includes functions to create deferreds that become determined at a certain time.

    The underlying implementation uses a heap of events, one for each deferred that needs
    to be determined at some time in the future.  It uses the timeout argument to select()
    in the core select loop to wake up and fill in the deferreds. *)

open Core.Std

(** Events provide a way to "abort" [at] and [after] requests. *)
module Event: sig
  type t

  val status : t -> [ `Happened | `Waiting | `Aborted ]
  val abort : t -> [ `Ok | `Previously_aborted | `Previously_happened ]
end

(** [after span] returns a deferred [d] that will become determined after the span of time
    passes.  If the [span] is nonpositive, then the deferred will be immediately
    determined.

    If you set up a lot of [after] events at the beginning of your program they will
    trigger at the same time.  Use [Time.Span.randomize] to even that out.

    For [after_event], if the event is aborted and [d] is not yet determined, then
    [d] will never become determined, and no more space will be required for tracking the
    event, i.e. the corresponding element will be removed from the heap. *)
val after       : Time.Span.t -> unit Deferred.t
val after_event : Time.Span.t -> [ `Happened | `Aborted ] Deferred.t * Event.t

(** [with_timeout span d] does pretty much what one can expect.  Note that at the point of
    checking, if [d] is determined and the timeout has expired, the resulting deferred
    will be determined with [`Result].  In other words, since there is inherent race
    between [d] and the timeout, the preference is given to [d]. *)
val with_timeout
  :  Time.Span.t
  -> 'a Deferred.t
  -> [ `Timeout
     | `Result of 'a
     ] Deferred.t

(** [at time] returns a deferred [d] that will become determined as soon as possible after
    the specified time.  Of course, it can't become determined before [at] is called, so
    calling [at] on a time not in the future will return a deferred that is immediately
    determined.

    For [at_event], if the event is aborted and the result of [at] is not yet determined,
    then the result will never become determined, and no more space will be required by
    async for tracking the [at], i.e. the corresponding element will be removed from the
    heap. *)
val at       : Time.t -> unit Deferred.t
val at_event : Time.t -> [ `Happened | `Aborted ] Deferred.t * Event.t

(** [at_varying_intervals ~stop f] returns a stream whose next element becomes determined
    by calling [f ()] and waiting for that amount of time, and then looping to determine
    subsequent elements.  The stream will end after [stop] becomes determined. *)
val at_varying_intervals :
  ?stop:unit Deferred.t -> (unit -> Time.Span.t) -> unit Async_stream.t

(** [at_intervals ~stop span] returns a stream whose elements will become determined span
    time apart.  The stream will end after stop becomes determined.

    [at_intervals ~stop span] = [at_varying_intervals ~stop (fun () -> span)] *)
val at_intervals : ?stop:unit Deferred.t -> Time.Span.t -> unit Async_stream.t

(** [every' ?start ?stop span f] runs [f()] every [span] amount of time starting when
    [start] becomes determined and stopping when [stop] becomes determined.  [every] waits
    until the result of [f()] becomes determined before waiting for the next [span].

    Note that it has [span] delay even before the first call of [f].

    It is guaranteed that if [stop] becomes determined, even during evaluation of [f],
    then [f] will not be called again by a subsequent iteration of the loop.

    It is an error for [span] to be nonpositive. *)
val every'
  :  ?start : unit Deferred.t
  -> ?stop : unit Deferred.t
  -> ?continue_on_error : bool
  -> Time.Span.t
  -> (unit -> unit Deferred.t) -> unit

(** [every ?start ?stop span f] is
    [every' ?start ?stop span (fun () -> f (); Deferred.unit)] *)
val every
  :  ?start : unit Deferred.t
  -> ?stop : unit Deferred.t
  -> ?continue_on_error : bool
  -> Time.Span.t
  -> (unit -> unit) -> unit

val with_timeout : Time.Span.t -> 'a Deferred.t -> [ `Timeout | `Result of 'a ] Deferred.t
