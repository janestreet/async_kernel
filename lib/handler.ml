include Raw_handler

let create run =
  { execution_context = Scheduler.(current_execution_context (t ()));
    run;
  }
;;

let install t d =
  let u = Ivar.Deferred.install_removable_handler d t in
  fun () -> Unregister.unregister u;
;;

let schedule t a = Scheduler.add_job t.execution_context t.run a
