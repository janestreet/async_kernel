(** [Config] has various settings that globally affect the behavior of async.  These are
    set by setting an environment variable to a sexp representation of the config. *)
open Core.Std

type t with sexp_of

val t : t

val environment_variable : string

module Print_debug_messages_for : sig
  val clock              : bool
  val fd                 : bool
  val file_descr_watcher : bool
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
  type t = Epoll | Select
end

val check_invariants                  : bool
val detect_invalid_access_from_thread : bool
val epoll_max_ready_events            : int
val file_descr_watcher                : File_descr_watcher.t
val max_num_open_file_descrs          : int
val max_num_threads                   : int
val record_backtraces                 : bool
