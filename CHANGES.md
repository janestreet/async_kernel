## 112.06.00

- Added `Deferred.Sequence` module, analogous to `Deferred.List` but for
  `Core_kernel.Std.Sequence`.
- Modernized code style.

## 112.01.00

- Optimized `Monitor.try_with ~run:\`Now f` to return a determined
  deferred if `f ()` returns a determined deferred.

  Previously, `Monitor.try_with ~run:\`Now f` always introduced a
  `Deferred.map`, which made it impossible to do some optimizations
  that bypass the scheduler overhead.
- Added an `ASYNC_CONFIG` field that causes the program to dump core
  if Async jobs are delayed too much.

  The new field is `dump_core_on_job_delay`.
- Switched `Async_kernel` from using `Core.Sys` to `Pervasives.Sys`
  eliminating one of the dependencies on `Core`.

## 111.25.00

- Fixed `Clock.run_at_intervals` to make the initial callback at an
  interval multiple.

  Previously, if `start` was in the past, `f` would run immediately
  rather than waiting for an interval of the form `start + i * span`.
  Now it always waits for an interval, even the first time it runs.

## 111.17.00

- Renamed `Monitor.errors` to `Monitor.detach_and_get_error_stream`
  and `Monitor.error` as `Monitor.get_next_error`.

  The use of `detach` in the name is intended to make clear that
  errors do not propagate to the parent.

  Added several other non-stream =Monitor= functions to capture common
  use cases of `Monitor.detach_and_get_error_stream`:

  ```ocaml
  detach_and_get_next_error
  detach_and_iter_errors
  detach
  ```

## 111.11.00

- Added `Clock.run_at_intervals`, which runs a job at regular
  intervals.

## 111.08.00

- Changed low-level error messages to use `Sexp.to_string_hum` rather
  than `to_string_mach`.

## 111.06.00

- Improved the performance of `Pipe.filter_map` by using batching.

## 110.01.00

- Added `Deferred.Result.map_error`.

## 109.60.00

- Changed the scheduler to clear a job from its queue when it runs the
  job, eliminating a performance regression from 109.57.

    Clearing avoids spurious promotion of what would otherwise be dead
    data associated with already-executed jobs.

## 109.58.00

- Renamed the `Async_core` library as `Async_kernel`, to parallel
  `Core_kernel`.

  Someday `Async_core` will depend only on `Core_kernel`, but not yet.
- Added a thread-safe queue of "external actions" that is checked
  after each job.
- Fixed a race condition in `Clock.Event.abort`.

  Here is the race condition:

  * `Clock.Event.at` adds an alarm, its value is a job (let's call it
    job1) with this run function:

    ```ocaml
    let fire () =
      t := Happened;
      Ivar.fill ready `Happened;
    ```
  * later a job (let's call it job2) aborting the clock event is
    queued in the async scheduler
  * in the same cycle, the `Timing_wheel.advance_clock` fires the
    alarm and job1 scheduled
  * at this point:
      + job1 and job2 are still pending
      + the alarm was removed so it is invalid
      + the clock event is still in the state `Waiting`
  * job2 is executed before job1: the clock event is still in the
    `Waiting` state, so the abort tries to remove the alarm from the
    timing wheel: CRASH

    The bugfix is for `Clock.Event.abort` to check if the alarm has
    already been removed from the timing wheel and if so, don't remove
    it again.
- Changed `Monitor.try_with` when run with ``~rest:`Ignore``, the
  default, so that the created monitor is detached from the monitor
  tree.

  The detached monitor has no parent, rather than being a child of the
  current monitor.  This will eliminate recently observed space leaks
  in `Sequencer_table` and `Throttle`, like:

  ```ocaml
  let leak () =
    let seq = Throttle.Sequencer.create () in
    let rec loop n =
      Throttle.enqueue seq (fun () ->
        loop (n + 1);
        Deferred.unit
      )
      |> don't_wait_for
    in
    loop 0
  ```
- Changed Async's scheduler to pool jobs rather than heap allocate
  them, decreasing the cost of a job by 30-40%.

    Changed the main scheduler queue of jobs to be an `Obj_array.t` that
    is essentially a specialized `Flat_queue` (the specialization was
    necessary for speed).

    Also, cleaned up the scheduler run-job loop.

    With these changes, the cost of a simple job decreases significantly
    (30-40%), across a range of live data sizes.  Here are the
    nanoseconds-per-job numbers for a microbenchmark with the old and
    new approaches.

    | num live jobs | old ns/job | new ns/job |
    |---------------|------------|------------|
    |             1 |         74 |         53 |
    |             2 |         75 |         47 |
    |             4 |         76 |         41 |
    |             8 |         63 |         39 |
    |            16 |         62 |         38 |
    |            32 |         61 |         37 |
    |            64 |         61 |         37 |
    |           128 |         60 |         37 |
    |           256 |         60 |         38 |
    |           512 |         60 |         38 |
    |          1024 |         60 |         39 |
    |          2048 |         61 |         40 |
    |          4096 |         67 |         41 |
    |          8192 |         65 |         45 |
    |         16384 |         75 |         56 |
    |         32768 |        115 |         67 |
    |         65536 |        171 |        108 |
    |        131072 |        255 |        158 |
    |        262144 |        191 |        130 |
    |        524288 |        216 |        139 |
    |       1048576 |        238 |        152 |

    See async/bench/nanos\_per\_job.ml for the benchmark.
- Removed `debug_space_leaks` from Async's internals.  It hadn't been
  used in years.

## 109.52.00

- Changed `Pipe.iter_without_pushback` to never call `f` after
  `Pipe.close_read` has been called.

    The new behavior is like `Pipe.iter`.

- Changed the implementation of `Pipe.fold_gen` and `Pipe.transfer_gen`
  to be analogous to `Pipe.iter_without_pushback`, and in particular to
  process as many elements as possible before calling `values_available`.

## 109.47.00

- Fix a bug introduced in `Monitor.error` in 109.28, in which the error wasn't seen unless someone is listening to the monitor.

## 109.45.00

- Removed internal `Backpatched` module.

    Async used to use this module, but it doesn't anymore.

## 109.44.00

- Documented that `Throttle.enqueue t f` never runs `f` immediately,
  and added unit tests.

## 109.42.00

- In `ASYNC_CONFIG`, replaced `alarm_precision` and `timing_wheel_level_bits` with `timing_wheel_config`.

    This parallels the new `Timing_wheel.Config` module.

## 109.35.00

- Added new configuration options for Async, `max_inter_cycle_timeout`
  and `max_num_jobs_per_priority_per_cycle`.

    ```ocaml
    val max_inter_cycle_timeout : Time.Span.t
    val max_num_jobs_per_priority_per_cycle : int
    ```

    These are configurable as usual via `ASYNC_CONFIG`.

- Added an `ASYNC_CONFIG` option to debug the `Shutdown` module.
- Added `find` and `find_map` to `Deferred.Monad_sequence`.

## 109.34.00

- Added a function to `Pipe` that merges a list of sorted pipes

    `val Pipe.merge : 'a Reader.t list -> cmp:('a -> 'a -> int) -> 'a Reader.t`

- Improved the performance of `Ivar.squash` by removing the allocation of the list of ivars.

    Instead, `squash` does one loop to find the end of the chain and a
    second loop to set all the indirections.

- Changed `Scheduler.run_cycles_until_no_jobs_remain` to raise an exception if there is an unhandled exception when it finishes.

## 109.32.00

- Improved the batching of `Pipe.fold` and other `Pipe` functions that handle batches.

  Previously, such functions used a loop with `Pipe.read`.  This
  didn't batch as well as it might.  If values were put in the pipe
  after the `read` becomes determined but before the values are
  handled, then they wouldn't be handled until the next batch.  Now,
  batching functions use `values_available` and then pull elements out
  of the pipe synchronously after waking up.  This makes the batch as
  large as possible.

## 109.30.00

- Added function `Throttle.kill`.

  `Throttle.kill` aborts all jobs that have been enqueued but not
  started, and immediately aborts all jobs subsequently enqueued.

  Split out `Throttle` debugging and unit-testing code into their own
  modules.

- Changed the semantics of `Throttle.enqueue` on a dead throttle so that
  the exception is sent to the monitor rather than raised synchronously.

  This gives more uniform treatment to the race between enqueueing a
  job and an already running job raising.  Now the enqueued job is
  always aborted, whether enqueued before or after the raise.

- Added an ASYNC_CONFIG option to print debug messages when `Monitor.send_exn` is called.

  This is useful when one is debugging an application in which an
  exception is being unexpectedly swallowed.

- Allow one to dynamically configure the behavior of =Monitor.try_with=.

  This is to allow experimentation with different handling of
  asynchronous exceptions after `Monitor.try_with` has become
  determined.

## 109.28.00

- Eliminated a messy dependency cycle in `async_core`, so that
  `Monitor.t` no longer contains a `Tail.t`.

  `async_core` was messy because of cycle between the following types:

  ```
  Execution_context
  --> Scheduler
  --> Ivar
  --> Deferred
  --> Tail
  --> Monitor
  ```

  This messiness caused the need for the `Raw` signature, for the
  various `Scheduler_dependent` functors, for making various types
  polymorphic in `execution_context`, and then instantiating the
  polymorphism later.

  The cause of the problem was that `Monitor` contained a `Tail`.  We
  eliminated that, so that there is no longer a cycle, and defined
  things in order:

  ```
  Monitor
  Execution_context
  Job
  Scheduler
  Ivar
  Deferred
  Tail
  Stream
  ```

  Replaced the `errors` tail from the monitor type:

  ```ocaml
  errors : (exn, 'execution_context) Raw_tail.t;
  ```

  with a list of handlers:

  ```ocaml
  mutable error_handlers : (exn -> unit) list;
  ```

  Cleaned up all the messiness caused by the cycle -- eliminated the
  `Raw` signature, the `Scheduler_dependent` functors, and the
  unnecessary polymorphism.

  Cleaned up the long standing annoyance with `Async.Stream`, in which
  we couldn't expose the `next` sum type and people had to use
  annoying `Stream.of_raw` calls.  `Stream.of_raw` is now gone.

## 109.27.00

- Fixed `Monitor.catch_stream` to prevent missing a synchronous
  exception.

## 109.24.00

- Reworked the `Throttle` module.

  Made both `Throttle.t` and `Throttle.Sequencer.t` instances of the
  same type, using a phantom type to distinguish them.  Removed all
  `Throttle.Sequencer` functions -- one can now use the `Throttle`
  functions directly.

  Added new functions:

  ```ocaml
  (*** [max_concurrent_jobs t] returns the maximum number of jobs that [t] will run
       concurrently. *)
  val max_concurrent_jobs : (_, _) t_ -> int

  (*** [num_jobs_running t] returns the number of jobs that [t] is currently running.  It
       is guaranteed that if [num_jobs_running t < max_concurrent_jobs t] then
       [num_jobs_waiting_to_start t = 0].  That is, the throttle always uses its maximum
       concurrency if possible. *)
  val num_jobs_running : (_, _) t_ -> int

  (*** [num_jobs_waiting_to_start t] returns the number of jobs that have been [enqueue]d but
       have not yet started. *)
  val num_jobs_waiting_to_start : (_ , _) t_ -> int

  (*** [capacity_available t] becomes determined the next time that [t] has fewer than
       [max_concurrent_jobs t] running, and hence an [enqueue]d job would start
       immediately. *)
  val capacity_available : (_, _) t_ -> unit Deferred.t
  ```

  Replaced the `Pipe` used inside a `Throttle` with a `Queue`, and
  simplified the implementation.  This fixed a bug in
  `num_jobs_waiting_to_start`, which could have missed a job that was
  not in the pipe but had not started.

## 109.20.00

- Added the ability for a `Throttle` to have resources that are exclusively available to running jobs.

## 109.13.00

- Fixed `Pipe.iter`'s handling of a closed pipe.

  Fixed the handling by `Pipe.iter` and related foldy functions that
  handle one element at a time, which behaved surprisingly with a pipe
  whose read end has been closed.  These functions had worked by
  reading a queue as a batch and then applying the user function to
  each queue element.  But if the pipe's read end is closed during the
  processing of one queue element, no subsequent element should be
  processed.  Prior to this fix, the `iter` didn't notice the pipe was
  closed for read until it went to read the next batch.
- Renamed `Pipe.read_one` as `Pipe.read_one`', and added
  `Pipe.read_one` that reads a single element.

## 109.11.00

- Extended `Deferred.Or_error` to parallel almost all of the
  `Core.Or_error` interface.
- Improved the performance of `Clock.at`, and added a more efficient
  version, `Clock.run_at`.

  Reworked the async heap of clock alarms to use async jobs as alarms.

  Reworked `Clock.at` to use this and to not use abortable events,
  which is a performance improvement.

  Added a more efficient version of `Clock.at`, for the common
  situation when one doesn't need a deferred.

  ```ocaml
  (*** [run_at time ~f] is a more efficient version of [at time >>> f]. *)
  val run_at : Time.t -> f:(unit -> unit) -> unit
  ```

## 109.09.00

- Fixed bug in `Async.Throttle`, in which jobs weren't started in order.

