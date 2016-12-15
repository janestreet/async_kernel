open! Core_kernel.Std
open! Import

module T = struct
  type +'a t = 'a Deferred.t [@@deriving sexp_of]

  let return = Deferred.return

  let bind t ~f =
    if Deferred.is_determined t
    then (f (Deferred.value_exn t))
    else (Deferred.bind t ~f)
  ;;

  let map t ~f =
    if Deferred.is_determined t
    then (return (f (Deferred.value_exn t)))
    else (Deferred.map t ~f)
  ;;

  let map = `Custom map
end

include T

include Monad.Make(T)

let create             = Deferred.create
let don't_wait_for     = Deferred.don't_wait_for
let invariant          = Deferred.invariant
let is_determined      = Deferred.is_determined
let never              = Deferred.never
let peek               = Deferred.peek
let unit               = Deferred.unit
let value_exn          = Deferred.value_exn

let upon t f =
  if is_determined t
  then (f (value_exn t))
  else (Deferred.upon t f)
;;

let both t1 t2 =
  create (fun result ->
    upon t1 (fun a1 -> upon t2 (fun a2 -> Ivar.fill result (a1, a2))))
;;

let ok t =
  if is_determined t
  then (return (Ok (value_exn t)))
  else (Deferred.ok t)
;;

let ignore t =
  if is_determined t
  then unit
  else (Deferred.ignore t)
;;

let any ts =
  match List.find ts ~f:is_determined with
  | Some x -> return (value_exn x)
  | None   -> Deferred.any ts
;;

let any_unit ts =
  if List.exists ts ~f:is_determined
  then unit
  else (Deferred.any_unit ts)
;;

let rec all_unit = function
  | [] -> return ()
  | hd :: tl ->
    if is_determined hd
    then (all_unit tl)
    else (Deferred.bind hd ~f:(fun () -> all_unit tl))
;;

module Infix = struct
  include Monad_infix
  let (>>>) = upon
end
