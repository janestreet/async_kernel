open Core.Std
open Import

module Deferred = Raw_deferred
module Ivar = Raw_ivar
module Stream = Raw_stream

type ('a, 'execution_context) t =
  { (* [next] points at the tail of the stream *)
    mutable next: (('a, 'execution_context) Stream.next, 'execution_context) Ivar.t;
  }

let sexp_of_t _ _ t =
  Sexp.Atom (if Ivar.is_empty t.next then "<open tail>" else "<closed tail>")
;;

let create () = { next = Ivar.create () }

let collect t = Deferred.of_ivar t.next
