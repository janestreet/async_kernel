(** Schedule jobs to run at a time in the future.

    The underlying implementation uses a heap of events, one for each job that needs to
    run in the future.  The Async scheduler is responsible for waking up at the right time
    to run the jobs.
*)

open Core.Std

(** [run_at time f a] runs [f a] as soon as possible after [time].  If [time] is in the
    past, then [run_at] will immediately schedule a job t that will run [f a].  In no
    situation will [run_at] actually call [f] itself.  The call to [f] will always be in
    another job.

    [run_after] is like [run_at], except that one specifies a time span rather than an
    absolute time. *)
val run_at    : Time.t      -> ('a -> unit) -> 'a -> unit
val run_after : Time.Span.t -> ('a -> unit) -> 'a -> unit

(** [at time] returns a deferred [d] that will become determined as soon as possible after
    [time]

    [after] is like [at], except that one specifies a time span rather than an absolute
    time.

    If you set up a lot of [after] events at the beginning of your program they will
    trigger at the same time.  Use [Time.Span.randomize] to even that out. *)
val at    : Time.t      -> unit Deferred.t
val after : Time.Span.t -> unit Deferred.t

(** [with_timeout span d] does pretty much what one would expect.  Note that at the point
    of checking if [d] is determined and the timeout has expired, the resulting deferred
    will be determined with [`Result].  In other words, since there is an inherent race
    between [d] and the timeout, preference is given to [d]. *)
val with_timeout
  :  Time.Span.t
  -> 'a Deferred.t
  -> [ `Timeout
     | `Result of 'a
     ] Deferred.t

(** Events provide abortable versions of [at] and [after]. *)
module Event: sig
  type t with sexp_of

  include Invariant.S with type t := t

  val status : t -> [ `Happened | `Waiting | `Aborted ]
  val abort : t -> [ `Ok | `Previously_aborted | `Previously_happened ]

  (* [at time] returns a pair [t, d], where [d] is like [at time], except that if one
     calls [abort t] prior to [d] becoming determined, then [d] will become determined
     with [`Aborted].

     [after] is like [at], except that one specifies a time span rather than an absolute
     time.

     For both [at] and [after], once an event is aborted, Async doesn't use any space for
     tracking it. *)
  val at    : Time.t      -> t * [ `Happened | `Aborted ] Deferred.t
  val after : Time.Span.t -> t * [ `Happened | `Aborted ] Deferred.t
end

(** [at_varying_intervals f ?stop] returns a stream whose next element becomes determined
    by calling [f ()] and waiting for that amount of time, and then looping to determine
    subsequent elements.  The stream will end after [stop] becomes determined. *)
val at_varying_intervals
  : ?stop:unit Deferred.t -> (unit -> Time.Span.t) -> unit Async_stream.t

(** [at_intervals interval ?start ?stop] returns a stream whose elements will become
    determined at nonnegative integer multiples of [interval] after the [start] time,
    until [stop] becomes determined:

    {v
       start + 0 * interval
       start + 1 * interval
       start + 2 * interval
       start + 3 * interval
       ...
    v}

    If the interval is too small or the CPU is too loaded, [at_intervals] will skip
    until the next upcoming multiple of [interval] after start. *)
val at_intervals
  :  ?start:Time.t
  -> ?stop:unit Deferred.t
  -> Time.Span.t
  -> unit Async_stream.t

(** [every' ?start ?stop span f] runs [f()] every [span] amount of time starting when
    [start] becomes determined and stopping when [stop] becomes determined.  [every] waits
    until the result of [f()] becomes determined before waiting for the next [span].

    It is guaranteed that if [stop] becomes determined, even during evaluation of [f],
    then [f] will not be called again by a subsequent iteration of the loop.

    It is an error for [span] to be nonpositive.

    Exceptions raised by [f] are always sent to monitor in effect when [every'] was
    called, even with [~continue_on_error:true]. *)
val every'
  :  ?start : unit Deferred.t   (** default is [Deferred.unit] *)
  -> ?stop : unit Deferred.t    (** default is [Deferred.never ()] *)
  -> ?continue_on_error : bool  (** default is [true] *)
  -> Time.Span.t
  -> (unit -> unit Deferred.t) -> unit

(** [every ?start ?stop span f] is
    [every' ?start ?stop span (fun () -> f (); Deferred.unit)] *)
val every
  :  ?start : unit Deferred.t   (** default is [Deferred.unit] *)
  -> ?stop : unit Deferred.t    (** default is [Deferred.never ()] *)
  -> ?continue_on_error : bool  (** default is [true] *)
  -> Time.Span.t
  -> (unit -> unit) -> unit

(** [run_at_intervals' ?start ?stop span f] runs [f()] at increments of [start + i * span]
    for non-negative integers [i], until [stop] becomes determined.  [run_at_intervals']
    waits for the result of [f] to become determined before waiting for the next interval.

    Exceptions raised by [f] are always sent to monitor in effect when [run_at_intervals']
    was called, even with [~continue_on_error:true]. *)
val run_at_intervals'
  :  ?start : Time.t            (** default is [Time.now ()] *)
  -> ?stop : unit Deferred.t    (** default is [Deferred.never ()] *)
  -> ?continue_on_error : bool  (** default is [true] *)
  -> Time.Span.t
  -> (unit -> unit Deferred.t)
  -> unit

(** [run_at_intervals ?start ?stop ?continue_on_error span f] is equivalent to:

    {[
      run_at_intervals' ?start ?stop ?continue_on_error span
        (fun () -> f (); Deferred.unit)
    ]}
*)
val run_at_intervals
  :  ?start : Time.t            (** default is [Time.now ()] *)
  -> ?stop : unit Deferred.t    (** default is [Deferred.never ()] *)
  -> ?continue_on_error : bool  (** default is [true] *)
  -> Time.Span.t
  -> (unit -> unit)
  -> unit
