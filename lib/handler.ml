open Core.Std
open Import

module Handler = Raw_handler

open Handler.T

type 'a t = ('a, Execution_context.t) Handler.t

include struct
  open Handler
  let filter = filter
  let prepend = prepend
end

let create run =
  { execution_context = Scheduler.current_execution_context ();
    run;
  }
;;

module Deferred = Raw_deferred.Scheduler_dependent (Raw_scheduler)

let install t d =
  let u = Deferred.install_removable_handler d t in
  fun () -> Unregister.unregister u;
;;

let schedule t a = Scheduler.add_job t.execution_context t.run a
