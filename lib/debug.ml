open Core.Std

include Config.Print_debug_messages_for

let log message a sexp_of_a =
  eprintf "%s\n%!"
    (Sexp.to_string_hum
       (<:sexp_of< [ `pid of Pid.t ] * [ `thread_id of int ] * Time.t * string * a >>
          (`pid (Unix.getpid ()),
           `thread_id (Thread.id (Thread.self ())),
           Time.now (),
           message,
           a)))
;;

let log_string message = log message () <:sexp_of< unit >>
