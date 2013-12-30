(** Settings that globally affect the behavior of Async.

    These can be set by setting an environment variable, [ASYNC_CONFIG], to a sexp
    representation of the config.  Also, setting [ASYNC_CONFIG] to an invalid sexp
    (e.g. the empty string), will cause your program to print to stderr a usage message
    describing how to configure [ASYNC_CONFIG], and exit nonzero.  For example, the
    following shell command should print the usage message:

    {v
      ASYNC_CONFIG= foo.exe
    v}
*)

open Core.Std

module Epoll_max_ready_events              : Validated with type raw := int
module Max_inter_cycle_timeout             : Validated with type raw := Time.Span.t
module Max_num_open_file_descrs            : Validated with type raw := int
module Max_num_threads                     : Validated with type raw := int
module Max_num_jobs_per_priority_per_cycle : Validated with type raw := int

type t with sexp_of

val t : t

val environment_variable : string

module Print_debug_messages_for : sig
  val clock              : bool
  val fd                 : bool
  val file_descr_watcher : bool
  val finalizers         : bool
  val interruptor        : bool
  val monitor            : bool
  val monitor_send_exn   : bool
  val parallel           : bool
  val reader             : bool
  val scheduler          : bool
  val shutdown           : bool
  val thread_pool        : bool
  val thread_safe        : bool
  val writer             : bool
end

module File_descr_watcher : sig
  type t = Epoll | Select with sexp_of
end

(** Documentation on these is in strings in config.ml, so it can be output in the
    help message. *)
val abort_after_thread_pool_stuck_for   : Time.Span.t
val check_invariants                    : bool
val detect_invalid_access_from_thread   : bool
val epoll_max_ready_events              : Epoll_max_ready_events.t
val file_descr_watcher                  : File_descr_watcher.t
val max_inter_cycle_timeout             : Max_inter_cycle_timeout.t
val max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t
val max_num_open_file_descrs            : Max_num_open_file_descrs.t
val max_num_threads                     : Max_num_threads.t
val record_backtraces                   : bool
val report_thread_pool_stuck_for        : Time.Span.t
val timing_wheel_config                 : Timing_wheel.Config.t
