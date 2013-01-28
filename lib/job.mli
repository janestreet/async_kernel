open Core.Std

type 'execution_context t

val create : 'execution_context -> ('a -> unit) -> 'a -> 'execution_context t
val execution_context : 'execution_context t -> 'execution_context
val run : _ t -> unit
