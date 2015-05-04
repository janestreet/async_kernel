include Raw_handler

let create run =
  { execution_context = Scheduler.(current_execution_context (t ()))
  ; run;
  }
;;

let install t d =
  let handler = Ivar.Deferred.add_handler d t.run t.execution_context in
  fun () -> Ivar.Deferred.remove_handler d handler
;;

let schedule t a = Scheduler.(enqueue (t ())) t.execution_context t.run a
