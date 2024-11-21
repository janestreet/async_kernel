open! Core
open! Import

module T = struct
  type +'a t = 'a Deferred1.t [@@deriving sexp_of]

  let return = Deferred1.return

  let bind t ~f =
    if Deferred1.is_determined t then f (Deferred1.value_exn t) else Deferred1.bind t ~f
  ;;

  let map t ~f =
    if Deferred1.is_determined t
    then return (f (Deferred1.value_exn t))
    else Deferred1.map t ~f
  ;;

  let map = `Custom map
end

include T
include Monad.Make (T)

let create = Deferred1.create
let don't_wait_for = Deferred1.don't_wait_for
let invariant = Deferred1.invariant
let is_determined = Deferred1.is_determined
let never = Deferred1.never
let peek = Deferred1.peek
let unit = Deferred1.unit
let value_exn = Deferred1.value_exn
let upon t f = if is_determined t then f (value_exn t) else Deferred1.upon t f

let both t1 t2 =
  create (fun result ->
    upon t1 (fun a1 -> upon t2 (fun a2 -> Ivar.fill_exn result (a1, a2))))
;;

let ok t = if is_determined t then return (Ok (value_exn t)) else Deferred1.ok t
let ignore_m t = if is_determined t then unit else Deferred1.ignore_m t

let any ts =
  match List.find ts ~f:is_determined with
  | Some x -> return (value_exn x)
  | None -> Deferred1.any ts
;;

let any_unit ts =
  if List.exists ts ~f:(is_determined : unit t -> bool)
  then unit
  else Deferred1.any_unit ts
;;

module Infix = struct
  include Monad_infix

  let ( >>> ) = upon
end

let repeat_until_finished state f =
  let open Infix in
  create (fun finished ->
    let rec loop state =
      f state
      >>> function
      | `Repeat state -> loop state
      | `Finished result -> Ivar.fill_exn finished result
    in
    loop state)
;;

let all_unit = `use_list_module
