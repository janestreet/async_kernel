open! Core_kernel
open! Import
include Types.Scheduler

let events t = t.time_source.events

let set_execution_context t execution_context =
  (* Avoid a caml_modify in most cases. *)
  if not (phys_equal t.current_execution_context execution_context)
  then (
    if t.current_execution_context.tid <> execution_context.tid then (!Tracing.fns).trace_thread_switch execution_context ;
    t.current_execution_context <- execution_context )
;;
