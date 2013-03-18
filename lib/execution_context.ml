open Core.Std

module Monitor = Raw_monitor

type t =
  { (* We need to define the main execution context prior to defining the main
       [work_group] or [monitor].  So, we make [work_group] and [monitor] be
       backpatchable, and backpatch the the main [monitor] in in monitor.ml, and
       the main [work_group] in async/unix/lib/raw_scheduler.ml.

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
    work_group : Work_group.t Backpatched.t;
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

let main_work_group_hole = Backpatched.Hole.create ~name:"work group"

let main_monitor_hole = Backpatched.Hole.create ~name:"monitor"

let main =
  { work_group = Backpatched.of_hole main_work_group_hole;
    monitor = Backpatched.of_hole main_monitor_hole;
    priority = Priority.normal;
    backtrace_history = [];
    kill_index = Kill_index.initial;
  }
;;

let work_group t = Backpatched.get_exn t.work_group

let monitor t = Backpatched.get_exn t.monitor

let create_like ?work_group ?monitor ?priority t =
  let backpatched o z =
    match o with
    | None -> z
    | Some a -> Backpatched.create a
  in
  let get o z = match o with None -> z | Some x -> x in
  let monitor = backpatched monitor t.monitor in
  { work_group        = backpatched work_group t.work_group;
    monitor;
    priority          = get priority t.priority ;
    backtrace_history = t.backtrace_history;
    kill_index        = Monitor.kill_index (Backpatched.get_exn monitor);
  }
;;

let record_backtrace =
  match Backtrace.get with
  | Error _ -> Fn.id
  | Ok get -> fun t -> { t with backtrace_history = get () :: t.backtrace_history }
;;
