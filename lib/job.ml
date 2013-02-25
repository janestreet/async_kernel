(* We represent a job with an existential type (via a GADT) because doing so allows us to
   use a single block to represent the job.  Without this, we would need to hide the ['a]
   type using a closure, which would cause much more allocation. *)

open Core.Std  let _ = _squelch_unused_module_warning_

type 'execution_context t =
  T : 'execution_context * 'a * ('a -> unit) -> 'execution_context t

let execution_context (T (e, _, _)) = e

let invariant execution_context_invariant (T (execution_context, _, _)) =
  execution_context_invariant execution_context;
;;

let run (T (_, a, f)) = f a

let sexp_of_t sexp_of_execution_context (T (execution_context, _, _)) =
  sexp_of_execution_context execution_context;
;;

let create execution_context f a = T (execution_context, a, f)

let do_nothing execution_context = create execution_context ignore ()
