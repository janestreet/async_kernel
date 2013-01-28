open Core.Std

module Tail = Raw_tail

module Stream = struct
  type 'a t = ('a, Execution_context.t) Raw_stream.next Deferred.t

  let sexp_of_t sexp_of_a t = Raw_stream.sexp_of_t sexp_of_a () (Deferred.to_raw t)

  let of_raw = Deferred.of_raw
  let to_raw = Deferred.to_raw

  type ('a, 'execution_context) next_
  = ('a, 'execution_context) Raw_stream.next
  = Nil
  | Cons of 'a * ('a, 'execution_context) Raw_stream.t

  type 'a next = ('a, Execution_context.t) next_

  let next t = Deferred.of_raw (to_raw t)
end

type 'a t = ('a, Execution_context.t) Tail.t with sexp_of

let of_raw = Fn.id
let to_raw = Fn.id

let create = Tail.create

let next t = Ivar.of_raw t.Tail.next

let collect t = Ivar.read (next t)

let is_closed t = Ivar.is_full (next t)

let fill_exn t v =
  if is_closed t then
    failwith "stream is closed"
  else
    Ivar.fill (next t) v
;;

let close_exn t = fill_exn t Raw_stream.Nil

let close_if_open t = if not (is_closed t) then Ivar.fill (next t) Stream.Nil

let extend t v =
  let next = Ivar.create () in
  fill_exn t (Stream.Cons (v, Deferred.to_raw (Ivar.read next)));
  t.Tail.next <- Ivar.to_raw next;
;;
