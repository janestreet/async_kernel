open Core.Std

(* This module exists to break a dependency cycle when building with ocamlbuild,
   which considers both the '.ml' and the '.mli' as part of a single compilation
   unit. It is referenced directly by monitor.mli. *)

type ('a, 'execution_context) next_ = ('a, 'execution_context) Raw_stream.next =
  Nil | Cons of 'a * ('a, 'execution_context) Raw_stream.t

type 'a next = ('a, Execution_context.t) next_

type 'a t = ('a, Execution_context.t) Raw_stream.t with sexp_of

let next = Raw_stream.next

