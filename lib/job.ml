(* We represent a job with an existential type (via a GADT) because doing so allows us to
   use a single block to represent the job.  Without this, we would need to hide the ['a]
   type using a closure, which would cause much more allocation. *)

open Core.Std  let _ = _squelch_unused_module_warning_

type t = T : Execution_context.t * 'a * ('a -> unit) -> t

let execution_context (T (e, _, _)) = e

let invariant (T (execution_context, _, _)) =
  Execution_context.invariant execution_context;
;;

let run (T (_, a, f)) = f a

let sexp_of_t (T (execution_context, _, _)) =
  Execution_context.sexp_of_t execution_context
;;

let create execution_context f a = T (execution_context, a, f)

let do_nothing = create Execution_context.main ignore ()
