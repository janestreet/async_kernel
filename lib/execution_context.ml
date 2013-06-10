open Core.Std

module Monitor = Raw_monitor

type t =
  { monitor : Monitor.t;
    priority : Priority.t;
    backtrace_history : Backtrace.t list;
    (* [kill_index] is a "cached" copy of [monitor]'s [kill_index].  This allows the
       scheduler, when deciding whether to run a job, to do a fast check in the common
       case that the execution context's [kill_index] is up to date, i.e. equal to the
       scheduler's [global_kill_index].  If it's not, then the scheduler will update the
       [kill_index] for itself and [monitor] (and [monitor]'s ancestors). *)
    mutable kill_index : Kill_index.t;
  }
with fields, sexp_of

let invariant (_ : t) = ()

let main =
  { monitor = Monitor.main;
    priority = Priority.normal;
    backtrace_history = [];
    kill_index = Kill_index.initial;
  }
;;

let create_like ?monitor ?priority t =
  let monitor = Option.value monitor ~default:t.monitor in
  { monitor;
    priority = Option.value priority ~default:t.priority;
    backtrace_history = t.backtrace_history;
    kill_index = Monitor.kill_index monitor;
  }
;;

let record_backtrace =
  match Backtrace.get with
  | Error _ -> Fn.id
  | Ok get -> fun t -> { t with backtrace_history = get () :: t.backtrace_history }
;;
