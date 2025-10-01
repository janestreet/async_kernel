module Time_ns_in_this_directory = Time_ns
open Core
module Time_ns = Time_ns_in_this_directory

(* module Time_ns = Time_ns_in_this_directory *)
include Async_kernel_config.Print_debug_messages_for

let task_id () = (Atomic.get Async_kernel_config.task_id) ()

let log message a sexp_of_a =
  prerr_endline
    (Sexp.to_string_hum
       ([%sexp_of: Sexp.t * Time_ns.t * string * a]
          (task_id (), Time_ns.now (), message, a)))
;;

let log_string message = log message () [%sexp_of: unit]
