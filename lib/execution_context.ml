open Core.Std
open Import

module Monitor = Raw_monitor

type t =
  { block_group : Block_group.t;
    monitor : t Monitor.t;
    priority : Priority.t;
    backtrace_history : Backtrace.t list;
  }
with fields, sexp_of

let bogus =
  { block_group = Block_group.bogus;
    monitor = Monitor.bogus ();
    priority = Priority.normal;
    backtrace_history = [];
  }
;;

let create_like ?block_group ?monitor ?priority t =
  let get o z = match o with None -> z | Some x -> x in
  { block_group = get block_group t.block_group;
    monitor     = get monitor     t.monitor    ;
    priority    = get priority    t.priority   ;
    backtrace_history = t.backtrace_history;
  }
;;

let record_backtrace =
  match Backtrace.get with
  | Ok get ->
    fun t ->
      { t with
        backtrace_history = get () :: t.backtrace_history;
      }
  | Error _ -> Fn.id
;;
