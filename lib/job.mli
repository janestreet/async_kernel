open Core.Std

type 'execution_context t with sexp_of

include Invariant.S1 with type 'a t := 'a t

val create : 'execution_context -> ('a -> unit) -> 'a -> 'execution_context t

val do_nothing : 'execution_context -> 'execution_context t

val execution_context : 'execution_context t -> 'execution_context

val run : _ t -> unit
