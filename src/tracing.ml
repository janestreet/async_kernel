type tracing_fns = 
{ trace_thread_switch : Execution_context.t -> unit }

let fns = ref { trace_thread_switch= fun _ -> () }