(** Internal to async -- an atomic unit of work to be run by the scheduler. *)

open Core.Std

type t with sexp_of

include Invariant.S with type t := t

val create : Execution_context.t -> ('a -> unit) -> 'a -> t

val do_nothing : t

val execution_context : t -> Execution_context.t

val run : t -> unit
