open Core.Std
open Import

module Deferred = Raw_deferred

type ('a, 'execution_context) t =
  (('a, 'execution_context) next, 'execution_context) Deferred.t
and ('a, 'execution_context) next = Nil | Cons of 'a * ('a, 'execution_context) t

let next t = t

let sexp_of_t sexp_of_a _ t =
  let rec loop d ac =
    match Deferred.peek d with
    | None -> Sexp.List (List.rev (Sexp.Atom "..." :: ac))
    | Some Nil -> Sexp.List (List.rev ac)
    | Some (Cons (a, t)) -> loop t (sexp_of_a a :: ac)
  in
  loop t []
;;
