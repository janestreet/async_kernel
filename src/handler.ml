open! Core_kernel.Std
open! Import

type 'a t =
  { run               : 'a -> unit
  ; execution_context : Execution_context.t
  }

let filter t ~f = { t with run = fun a -> if f a then t.run a }

let prepend t ~f = { t with run = fun a -> t.run (f a) }

let create run =
  { execution_context = Scheduler.(current_execution_context (t ()))
  ; run;
  }
;;

let install t d =
  let handler = Deferred0.add_handler d t.run t.execution_context in
  fun () -> Deferred0.remove_handler d handler
;;

let schedule t a = Scheduler.(enqueue (t ())) t.execution_context t.run a
