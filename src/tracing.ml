type tracing_fns = 
{ trace_thread_switch : Execution_context.t -> unit 
; trace_new_thread : string -> Execution_context.t -> unit
}

let fns = ref { trace_thread_switch= (fun _ -> ()); trace_new_thread= (fun _ _ -> ()) }