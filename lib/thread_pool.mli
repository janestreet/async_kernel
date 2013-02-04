(** A thread pool is a set of OCaml threads used to do work, where each piece of work is
    simply a thunk.  One creates a thread pool, and then uses [add_work_for_group] to
    submit work to it.  Work is done first-come-first-served by available threads in the
    pool.  Any of the available threads in the pool could be used to do work submitted to
    the pool (except helper threads, see below).

    A thread pool starts with no threads.  As work is added, the thread pool creates new
    threads to do the work, up to the maximum number of allowed threads,
    [max_num_threads], supplied to [create].  Thread-pool threads never die.  They just
    get created up until [max_num_threads] is reached and then live forever, doing work.
    Each thread in the pool is in a loop, waiting for a piece of work, running the thunk,
    and then repeating.  It may be that all the threads in the pool are not doing
    anything, but in this case, the threads still exist, and are simply blocked waiting
    for work.

    Sometimes one wants work to run in a dedicated thread, e.g. some C libraries require
    this.  To do this, use [Helper_thread], see below.

    All of the functions exposed by this module are thread safe; they synchronize using
    a mutex on the thread pool.

    One can control the priority of threads in the pool (in the sense of
    [Linux_ext.setpriority]).  Work added to the pool can optionally be given a priority,
    and the pool will set the priority of the thread that runs it for the duration of the
    work.  Helper threads can also be given a priority, which will be used for all work
    run by the helper thread, unless the work has an overriding priority.  The thread pool
    has a "default" priority that will be used for all work and helper threads that have
    no specified priority.  The default is simply the priority in effect when [create] is
    called.

    Behavior is unspecified if work calls [setpriority] directly. *)

open Core.Std
open Import

module Priority : module type of Linux_ext.Priority with type t = Linux_ext.Priority.t

type t with sexp_of

include Invariant.S with type t := t

(** [create ~max_num_threads] returns a new thread pool.  It is an error if
    [max_num_threads < 1]. *)
val create : max_num_threads:int -> t Or_error.t

(** [finished_with t] destroys all the threads in [t], and makes [t] no longer usable.

    It is an error to call [finished_with] if the thread pool has unfinished work or
    unfinished helper threads.  It is an error to call any other operation on [t] after
    calling [finished_with t]. *)
val finished_with : t -> unit Or_error.t

(** [max_num_threads t] returns the maximum number of threads that [t] is allowed to
    create. *)
val max_num_threads : t -> int

(** [num_threads t] returns the number of threads that the pool [t] has created. *)
val num_threads : t -> int

(** [default_priority t] returns the priority that will be used for work performed by
    [t], unless that work is added with an overriding priority. *)
val default_priority : t -> Priority.t

module Work_group : sig
  (** Each piece of work in the thread pool is associated with a "work group", which is
      used to control the number of threads used for work in the group.  When a thread is
      performing work for a work group, it is said to be "assigned" to that work group.
      Each work group has two optional limits: [min_assignable_threads] and
      [max_assigned_threads].

      The thread pool guarantees that requests to have threads assigned to this work group
      will be met by at least [min_assignable_threads] threads.  The thread pool will
      never assign more than [max_assigned_threads] to the work group.  The thread pool
      does not actually reserve specific threads for the work group.  It uses the same set
      of threads for all work groups.  Over time, a single thread may do work for
      different groups.  Work groups are just an accounting mechanism to make sure the
      number of threads from the global pool that are being used for each work group meet
      the requirements of that group.

      Each work group has its own dedicated work queue.  If a client requests to do some
      work in a group, and that group already has [min_assignable_threads] threads
      assigned to it, and there are no other available threads or the group already has
      [max_assigned_threads] assigned to it, then the work will be placed on the work
      group's queue, and will be handled in the future when threads become available to
      the group.

      If multiple work groups have work waiting to be done, the thread pool will
      round-robin among them as threads become available. *)
  type t with sexp_of
end

(** [create_work_group t ~min_assignable_threads ~max_assigned_threads] creates a new
    work group.

    The thread pool does not internally refer to the [Work_group.t] it returns.  So, it is
    OK for client code to use a finalizer to detect it becoming unused.

    It is an error if any of the following are true:

    - the thread pool can not guarantee that it can satisfy requests to use
      [min_assignable_threads], i.e. the number of threads reserved by other work groups
      plus [min_assignable_threads] is greater than [max_num_threads].
    - [min_assignable_threads < 0]
    - [min_assignable_threads > max_assigned_threads]
    - [max_assigned_threads > max_num_threads t] *)
val create_work_group
  :  ?min_assignable_threads:int (* default is 0 *)
  -> ?max_assigned_threads:int (* default is [max_num_threads t] *)
  -> t
  -> Work_group.t Or_error.t

(** [add_work_for_group ?priority ?name t work_group f] enqueues [f] to be done by some
    thread in the pool, subject to the thread-usage limits of [work_group].

    Exceptions raised by [f] are silently ignored.

    It is an error to call [add_work_for_group t work_group] after having called
    [finished_with_work_group t work_group].

    While the work is run, the name of the thread running the work will be set (via
    [Linux_ext.pr_set_name]) to [name] and the priority of the thread will be set
    to [priority]. *)
val add_work_for_group
  :  ?priority:Priority.t (* default is [default_priority t] *)
  -> ?name:string         (* default is "thread-pool thread" *)
  -> t
  -> Work_group.t
  -> (unit -> unit)
  -> unit Or_error.t

(** [finished_with_work_group t work_group] informs thread pool [t] that the [work_group]
    will no longer be used.

    It is an error to call [finished_with_work_group work_group] if [work_group] has
    unfinished work or has helper_threads for which [finished_with_helper_thread] hasn't
    yet been called. *)
val finished_with_work_group : t -> Work_group.t -> unit Or_error.t

module Helper_thread : sig
  (** A helper thread is a thread with its own dedicated work queue.  Work added for the
      helper thread is guaranteed to be run by that thread.  The helper thread only runs
      work explicitly supplied to it. *)
  type t

  (** [default_name t] returns the name that will be used for work performed by [t],
      unless that work is added with an overriding name *)
  val default_name : t -> string

  (** [default_priority t] returns the priority that will be used for work performed by
      [t], unless that work is added with an overriding priority. *)
  val default_priority : t -> Priority.t
end

(** [create_helper_thread ?priority ?name t work_group] creates a new helper thread that
    is part of [work_group], i.e. until [finished_with_helper_thread] is called, the
    helper thread counts as one of the threads assigned to the [work_group].

    The thread pool does not internally refer to the [Helper_thread.t] it returns.  So, it
    is OK for client code to use a finalizer to detect it becoming unused.

    It is an error if no threads are available.

    When the helper thread runs work, it will be at the helper thread's name and priority,
    except for work that is added with an overriding priority or name. *)
val create_helper_thread
  :  ?priority:Priority.t (* default is [default_priority t] *)
  -> ?name:string         (* default is "helper thread" *)
  -> t
  -> Work_group.t
  -> Helper_thread.t Or_error.t

(** [add_work_for_helper_thread ?priority ?name t helper_thread f] enqueues [f] on
    [helper_thread]'s work queue.

    Exceptions raised by [f] are silently ignored.

    It is an error to call [add_work_for_helper_thread t] after
    [finished_with_helper_thread t].

    When the helper thread runs [f], it will be at the helper thread's name and priority,
    unless overriden by [name] or [priority]. *)
val add_work_for_helper_thread
  :  ?priority:Priority.t (* default is [Helper_thread.default_priority helper_thread] *)
  -> ?name:string         (* default is [Helper_thread.name helper_thread] *)
  -> t
  -> Helper_thread.t
  -> (unit -> unit)
  -> unit Or_error.t

(** [finished_with_helper_thread t helper_thread] returns the helper thread to the
    general thread pool.

    It is an error to call [finished_with_helper_thread] if the helper thread has
    unfinished work. *)
val finished_with_helper_thread : t -> Helper_thread.t -> unit Or_error.t
