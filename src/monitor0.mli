@@ portable

open Core

val debug : bool

module Forwarding = Types.Forwarding

type t = Types.Monitor.t =
  { name : Info.t
  ; here : Source_code_position.t
  ; id : int
  ; mutable next_error : exn Types.Ivar.t
  ; mutable handlers_for_all_errors : (Types.Execution_context.t * (exn -> unit)) Bag.t
  ; mutable tails_for_all_errors : exn Types.Tail.t list
  ; mutable has_seen_error : bool
  ; mutable forwarding : Forwarding.t
  }
[@@deriving fields ~getters ~iterators:iter]

val description : t -> Sexp.t
val descriptions : t -> Sexp.t list
val sexp_of_t : t -> Sexp.t
val next_id : unit -> int
val create_with_parent : here:[%call_pos] -> ?info:Info.t -> ?name:string -> t option -> t
val main : t @@ nonportable
