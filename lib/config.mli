(** Settings that globally affect the behavior of async.

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
open Import

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
  val parallel           : bool
  val reader             : bool
  val run_job            : bool
  val scheduler          : bool
  val thread_pool        : bool
  val thread_safe        : bool
  val writer             : bool
end

module File_descr_watcher : sig
  type t = Epoll | Select with sexp_of
end

val alarm_precision                   : Time.Span.t
val check_invariants                  : bool
val detect_invalid_access_from_thread : bool
val epoll_max_ready_events            : int
val file_descr_watcher                : File_descr_watcher.t
val max_num_open_file_descrs          : int
val max_num_threads                   : int
val record_backtraces                 : bool
val timing_wheel_level_bits           : Timing_wheel.Level_bits.t
