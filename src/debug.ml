module Time_ns_in_this_directory = Time_ns
open Core_kernel.Std
module Time_ns = Time_ns_in_this_directory

include Config.Print_debug_messages_for

let log message a sexp_of_a =
  eprintf "%s\n%!"
    (Sexp.to_string_hum
       ([%sexp_of: Sexp.t * Time_ns.t * string * a]
          (!Config.task_id (), Time_ns.now (), message, a)))
;;

let log_string message = log message () [%sexp_of: unit]
