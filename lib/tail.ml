open Core.Std
open Import

module Stream = Raw_stream
module Tail = Raw_tail

type ('a, 'execution_context) t_ = ('a, 'execution_context) Tail.t =
  { mutable next: (('a, 'execution_context) Stream.next, 'execution_context) Raw_ivar.t;
  }

type 'a t = ('a, Execution_context.t) Tail.t with sexp_of

let create = Tail.create

let collect = Tail.collect

let is_closed t = Ivar.is_full t.next

let fill_exn t v =
  if is_closed t then
    failwith "stream is closed"
  else
    Ivar.fill t.next v
;;

let close_exn t = fill_exn t Stream.Nil

let close_if_open t = if not (is_closed t) then Ivar.fill t.next Stream.Nil

let extend t v =
  let next = Ivar.create () in
  fill_exn t (Stream.Cons (v, Ivar.read next));
  t.next <- next;
;;
