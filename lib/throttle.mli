(** Throttles to limit the number of concurrent computations.

    A throttle is essentially a pipe to which one can feed jobs.

    A throttle schedules asynchronous jobs so that at any given point in time no more than
    [max_concurrent_jobs] jobs are running.  A job [f] is considered to be running from
    the time [f ()] is executed until the deferred returned by [f ()] becomes determined,
    or [f ()] raises.  The throttle intiates jobs first-come first-served.

    One can use [create_with] to create a throttle with "resources" that one would
    like to make available to concurrent jobs and to guarantee that different jobs
    access different resources.

    A throttle "dies" if one of its jobs throws an exception, and the throttle has
    [continue_on_error = false].  If a throttle dies, then all jobs in it that haven't yet
    started will be aborted, i.e. they will not start and will become determined with
    [`Aborted].  Jobs that had already started will continue, and return [`Ok] or
    [`Raised] as usual when they finish.  Attempts to enqueue a job into a dead throttle
    will fail.
*)

open Core.Std

(** We use a phantom type to distinguish between throttles, which have
    [max_concurrent_jobs >= 1], and sequencers, which have [max_concurrent_jobs = 1].  All
    operations are available on both.  We make the distinction because it is sometimes
    useful to know from the type of a throttle that it is a sequencer and that at most one
    job can be running at a time. *)
type ('a, 'kind) t_ with sexp_of

type 'a t = ('a, [`throttle]) t_ with sexp_of

include Invariant.S1 with type 'a t := 'a t

(** [create ~continue_on_error ~max_concurrent_jobs] returns a throttle that will run up
    to [max_concurrent_jobs] concurrently.

    If some job raises an exception, then the throttle will stop, unless
    [continue_on_error] is true. *)
val create
  :  continue_on_error:bool
  -> max_concurrent_jobs:int
  -> unit t

(** [create_with ~continue_on_error job_resources] returns a throttle that will run up to
    [List.length job_resources] concurrently, and will ensure that all running jobs are
    supplied distinct elements of [job_resources]. *)
val create_with
  :  continue_on_error:bool
  -> 'a list
  -> 'a t

type 'a outcome = [ `Ok of 'a | `Aborted | `Raised of exn ]

(** [enqueue t job] schedules [job] to be run as soon as possible.  Jobs are guaranteed to
    be started in the order they are [enqueue]d.

    [enqueue] raises an exception if the throttle is dead. *)
val enqueue' : ('a, _) t_ -> ('a -> 'b Deferred.t) -> 'b outcome Deferred.t
val enqueue  : ('a, _) t_ -> ('a -> 'b Deferred.t) -> 'b         Deferred.t

(** [prior_jobs_done t] becomes determined when all of the jobs that were previously
    enqueued in [t] have completed. *)
val prior_jobs_done : (_, _) t_ -> unit Deferred.t

(** [max_concurrent_jobs t] returns the maximum number of jobs that [t] will run
    concurrently. *)
val max_concurrent_jobs : (_, _) t_ -> int

(** [num_jobs_running t] returns the number of jobs that [t] is currently running.  It
    is guaranteed that if [num_jobs_running t < max_concurrent_jobs t] then
    [num_jobs_waiting_to_start t = 0].  That is, the throttle always uses its maximum
    concurrency if possible. *)
val num_jobs_running : (_, _) t_ -> int

(** [num_jobs_waiting_to_start t] returns the number of jobs that have been [enqueue]d but
    have not yet started. *)
val num_jobs_waiting_to_start : (_ , _) t_ -> int

(** [capacity_available t] becomes determined the next time that [t] has fewer than
    [max_concurrent_jobs t] running, and hence an [enqueue]d job would start
    immediately. *)
val capacity_available : (_, _) t_ -> unit Deferred.t

(** A sequencer is a throttle that is specialized to only allow one job at a time and to,
    by default, not continue on error. *)
module Sequencer : sig
  type 'a t = ('a, [`sequencer]) t_ with sexp_of

  val create : ?continue_on_error:bool (* defaults to false *) -> 'a -> 'a t
end
