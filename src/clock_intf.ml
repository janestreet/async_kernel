(** Schedule jobs to run at a time in the future.

    The underlying implementation uses a heap of events, one for each job that needs to
    run in the future.  The Async scheduler is responsible for waking up at the right time
    to run the jobs.
*)

open Core_kernel.Std

module Deferred = Deferred1

module type Clock = sig
  module Time : sig
    module Span : sig
      type t
    end
    type t
  end

  (** [run_at time f a] runs [f a] as soon as possible after [time].  If [time] is in the
      past, then [run_at] will immediately schedule a job t that will run [f a].  In no
      situation will [run_at] actually call [f] itself.  The call to [f] will always be in
      another job.

      [run_after] is like [run_at], except that one specifies a time span rather than an
      absolute time. *)
  val run_at    : Time.t      -> ('a -> unit) -> 'a -> unit
  val run_after : Time.Span.t -> ('a -> unit) -> 'a -> unit

  (** [at time] returns a deferred [d] that will become determined as soon as possible
      after [time]

      [after] is like [at], except that one specifies a time span rather than an absolute
      time.

      If you set up a lot of [after] events at the beginning of your program they will
      trigger at the same time.  Use [Time.Span.randomize] to even that out. *)
  val at    : Time.t      -> unit Deferred.t
  val after : Time.Span.t -> unit Deferred.t

  (** [with_timeout span d] does pretty much what one would expect.  Note that at the
      point of checking if [d] is determined and the timeout has expired, the resulting
      deferred will be determined with [`Result].  In other words, since there is an
      inherent race between [d] and the timeout, preference is given to [d]. *)
  val with_timeout
    :  Time.Span.t
    -> 'a Deferred.t
    -> [ `Timeout
       | `Result of 'a
       ] Deferred.t

  (** Events provide variants of [run_at] and [run_after] with the ability to abort or
      reschedule an event that hasn't yet happened.  Once an event happens or is aborted,
      Async doesn't use any space for tracking it. *)
  module Event: sig
    type ('a, 'h) t [@@deriving sexp_of]
    type t_unit = (unit, unit) t [@@deriving sexp_of]

    include Invariant.S2 with type ('a, 'b) t := ('a, 'b) t

    val scheduled_at : (_, _) t -> Time.t

    (** If [status] returns [`Scheduled_at time], it is possible that [time < Time.now
        ()], if Async's scheduler hasn't yet gotten the chance to update its clock, e.g.
        due to user jobs running. *)
    val status
      : ('a, 'h) t -> [ `Aborted      of 'a
                      | `Happened     of 'h
                      | `Scheduled_at of Time.t
                      ]

    (** Let [t = run_at time f z].  At [time], this runs [f z] and transitions [status t]
        to [`Happened h], where [h] is result of [f z].

        More precisely, at [time], provided [abort t a] has not previously been called,
        this will call [f z], with the guarantee that [status t = `Scheduled_at time].  If
        [f z] returns [h] and did not call [abort t a], then [status t] becomes [`Happened
        h].  If [f z] calls [abort t a], then the result of [f] is ignored, and [status t]
        is [`Aborted a].

        If [f z] raises, then [status t] does not transition and remains [`Scheduled_at
        time], and the exception is sent to the monitor in effect when [run_at] was
        called. *)
    val run_at    : Time.     t -> ('z -> 'h) -> 'z -> (_, 'h) t
    val run_after : Time.Span.t -> ('z -> 'h) -> 'z -> (_, 'h) t

    (** [abort t] changes [status t] to [`Aborted] and returns [`Ok], unless [t]
        previously happened or was previously aborted. *)
    val abort : ('a, 'h) t -> 'a -> [ `Ok
                                    | `Previously_aborted  of 'a
                                    | `Previously_happened of 'h
                                    ]

    (** [abort_exn t a] returns unit if [abort t a = `Ok], and otherwise raises. *)
    val abort_exn : ('a, 'h) t -> 'a -> unit

    (** [abort_if_possible t a = ignore (abort t a)]. *)
    val abort_if_possible : ('a, _) t -> 'a -> unit

    val fired : ('a, 'h) t -> [ `Aborted of 'a | `Happened of 'h ] Deferred.t

    (** [reschedule_at t] and [reschedule_after t] change the time that [t] will fire, if
        possible, and if not, give a reason why.  [`Too_late_to_reschedule] means that the
        Async job to fire [t] has been enqueued, but has not yet run.

        Like [run_at], if the requested time is in the past, the event will be scheduled
        to run immediately.  If [reschedule_at t time = `Ok], then subsequently
        [scheduled_at t = time].  *)
    val reschedule_at
      : ('a, 'h) t -> Time.t      -> [ `Ok
                                     | `Previously_aborted     of 'a
                                     | `Previously_happened    of 'h
                                     | `Too_late_to_reschedule
                                     ]
    val reschedule_after
      : ('a, 'h) t -> Time.Span.t -> [ `Ok
                                     | `Previously_aborted     of 'a
                                     | `Previously_happened    of 'h
                                     | `Too_late_to_reschedule
                                     ]

    (** [at time]    is [run_at    time ignore ()].
        [after time] is [run_after time ignore ()].

        You should generally prefer to use the [run_*] functions, which allow one to
        *synchronously* update state via a user-supplied function when the event
        transitions to [`Happened].  That is, there is an important difference between:

        {[
          let t = run_at time f ()
        ]}

        and:

        {[
          let t = at time in
          fired t
          >>> function
          | `Happened () -> f ()
          | `Aborted () -> ()
        ]}

        With [run_at], if [status t = `Happened], one knows that [f] has run.  With [at]
        and [fired], one does not know whether [f] has yet run; it may still be scheduled
        to run.  Thus, with [at] and [fired], it is easy to introduce a race.  For
        example, consider these two code snippets:

        {[
          let t = Event.after (sec 2.) in
          upon (Event.fired t) (function
            | `Aborted () -> ()
            | `Happened () -> printf "Timer fired");
          upon deferred_event (fun () ->
            match Event.abort t () with
            | `Ok -> printf "Event occurred"
            | `Previously_aborted () -> assert false
            | `Previously_happened () -> printf "Event occurred after timer fired");
        ]}

        {[
          let t = Event.run_after (sec 2.) printf "Timer fired" in
          upon deferred_event (fun () ->
            match Event.abort t () with
            | `Ok -> printf "Event occurred"
            | `Previously_aborted () -> assert false
            | `Previously_happened () -> printf "Event occurred after timer fired");
        ]}

        In both snippets, if [Event.abort] returns [`Ok], "Timer fired" is never printed.
        However, the first snippet might print "Event occurred after timer fired" and then
        "Timer fired".  This confused ordering cannot happen with [Event.run_after]. *)
    val at    : Time.t      -> (_, unit) t
    val after : Time.Span.t -> (_, unit) t
  end

  (** [at_varying_intervals f ?stop] returns a stream whose next element becomes
      determined by calling [f ()] and waiting for that amount of time, and then looping
      to determine subsequent elements.  The stream will end after [stop] becomes
      determined. *)
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

  (** [every' ?start ?stop span f] runs [f ()] every [span] amount of time starting when
      [start] becomes determined and stopping when [stop] becomes determined.  [every]
      waits until the result of [f ()] becomes determined before waiting for the next
      [span].

      It is guaranteed that if [stop] becomes determined, even during evaluation of [f],
      then [f] will not be called again by a subsequent iteration of the loop.

      It is an error for [span] to be nonpositive.

      With [~continue_on_error:true], when [f] asynchronously raises, iteration continues.
      With [~continue_on_error:false], if [f] asynchronously raises, then iteration only
      continues when the result of [f] becomes determined.

      Exceptions raised by [f] are always sent to monitor in effect when [every'] was
      called, even with [~continue_on_error:true].
  *)
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

  (** [run_at_intervals' ?start ?stop span f] runs [f()] at increments of [start + i *
      span] for non-negative integers [i], until [stop] becomes determined.
      [run_at_intervals'] waits for the result of [f] to become determined before waiting
      for the next interval.

      Exceptions raised by [f] are always sent to monitor in effect when
      [run_at_intervals'] was called, even with [~continue_on_error:true]. *)
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
end
