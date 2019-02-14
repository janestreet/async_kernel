open! Core_kernel
open! Import
include Types.Scheduler

let events t = t.time_source.events

let set_execution_context t execution_context =
  (* Avoid a caml_modify in most cases. *)
  (* XXX: see where job_queue also modifies current_execution_context *)
  if not (phys_equal t.current_execution_context execution_context) then (
    if t.cycle_started && execution_context.tid <> 0 then (
      !Tracing.fns.trace_thread_switch execution_context ;
      t.cycle_started <- false )
    else if
      (not t.cycle_started)
      && t.current_execution_context.tid <> execution_context.tid
    then !Tracing.fns.trace_thread_switch execution_context ;
    t.current_execution_context <- execution_context )
