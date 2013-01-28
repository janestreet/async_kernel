open Core.Std

let debug = is_some (Sys.getenv "DEBUG_ASYNC")

(** When [RECORD_ASYNC_BACKTRACES] is set, the Async scheduler records a backtrace
    everytime it schedules a job, and will include the backtrace history in any exceptions
    raised to monitors, and in particular, in an unhandled exception that reaches the main
    monitor. *)
let record_backtraces =
  match Backtrace.get with
  | Ok _ -> is_some (Sys.getenv "RECORD_ASYNC_BACKTRACES")
  | Error _ -> (* unimplemented *) false

(* Calls to [Debug.log] should look like [if Debug.debug then Debug.log ...]. *)

let log message a sexp_of_a =
  eprintf "%s\n%!"
    (Sexp.to_string_mach
       (<:sexp_of< Pid.t * int * Time.t * string * a >>
           (Unix.getpid (), Thread.id (Thread.self ()), Time.now (), message, a)))
;;

let () = if debug then log "record_backtraces" record_backtraces <:sexp_of< bool >>

let log_string message = log message () <:sexp_of< unit >>
