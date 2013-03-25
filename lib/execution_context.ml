open Core.Std

module Monitor = Raw_monitor

type t =
  { (* We need to define the main execution context prior to defining the main monitor.
       So, we make the main monitor backpatchable, and backpatch it in monitor.ml.

       The cyclic dependence that necessitates backpatching the main monitor
       is essentially the following:

         Execution_context.main
         --> Scheduler
         --> Ivar
         --> Deferred
         --> Tail
         --> Monitor.main

       We have to backpatch the main work group because we don't even create it in
       this library (async_core).  It is only created by the async_unix library, and
       then only if an async-unix scheduler is created. *)
    monitor : t Monitor.t_ Backpatched.t;
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

let main_monitor_hole = Backpatched.Hole.create ~name:"monitor"

let main =
  { monitor = Backpatched.of_hole main_monitor_hole;
    priority = Priority.normal;
    backtrace_history = [];
    kill_index = Kill_index.initial;
  }
;;

let monitor t = Backpatched.get_exn t.monitor

let create_like ?monitor ?priority t =
  let monitor =
    match monitor with
    | None -> t.monitor
    | Some monitor -> Backpatched.create monitor
  in
  { monitor;
    priority = Option.value priority ~default:t.priority;
    backtrace_history = t.backtrace_history;
    kill_index = Monitor.kill_index (Backpatched.get_exn monitor);
  }
;;

let record_backtrace =
  match Backtrace.get with
  | Error _ -> Fn.id
  | Ok get -> fun t -> { t with backtrace_history = get () :: t.backtrace_history }
;;
