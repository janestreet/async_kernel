open Core.Std

(* We use a first class module to represent a job because it allows us to use a single
   4-word block to represent the job.  Without a first-class module, we would need to
   hide the [a] type using a closure, e.g.:

     type 'execution_context t =
       { execution_context: 'execution_context
         run : unit -> unit;
       }

   But this would then require allocating a 3-word block for [t], and a 4-word block
   for [run]. *)

module type T = sig
  type a
  type execution_context
  val execution_context : execution_context
  val f : a -> unit
  val a : a
end

type 'execution_context t = (module T with type execution_context = 'execution_context)

let execution_context (type execution_context) t =
  let module T = (val t : T with type execution_context = execution_context) in
  T.execution_context;
;;

let run (type execution_context) t =
  let module T = (val t : T with type execution_context = execution_context) in
  T.f T.a;
;;

let create (type a_) (type execution_context_) execution_context f a =
  let module T = struct
    type execution_context = execution_context_
    type a = a_
    let execution_context = execution_context
    let f = f
    let a = a
  end in
  (module T : T with type execution_context = execution_context_)
;;
