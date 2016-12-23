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

module Infix = struct
  include Monad_infix
  let (>>>) = upon
end

module List = struct
  type 'a t = 'a List.t

  open Infix
  open Let_syntax

  let foldi t ~init ~f =
    create
      (fun result ->
         let rec loop t i b =
           match t with
           | [] -> Ivar.fill result b
           | x :: xs -> f i b x >>> fun b -> loop xs (i + 1) b
         in
         loop t 0 init)
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a -> f a)

  let seqmap t ~f =
    fold t ~init:[] ~f:(fun bs a -> let%map b = f a in b :: bs)
    >>| List.rev
  ;;

  let all ds = seqmap ds ~f:Fn.id

  let all_unit ds = ignore (fold ds ~init:() ~f:(fun () d -> d) : unit T.t)

  let iteri ?(how = `Sequential) t ~f =
    match how with
    | `Parallel | `Max_concurrent_jobs _ as how ->
      all_unit (List.mapi t ~f:(unstage (Throttle.monad_sequence_how2 ~how ~f)))
    | `Sequential -> foldi t ~init:() ~f:(fun i () x -> f i x)
  ;;

  let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)

  let map ?(how = `Sequential) t ~f =
    match how with
    | `Parallel | `Max_concurrent_jobs _ as how ->
      all (List.map t ~f:(unstage (Throttle.monad_sequence_how ~how ~f)))
    | `Sequential -> seqmap t ~f
  ;;

  let init ?how n ~f = map ?how (List.init n ~f:Fn.id) ~f

  let filter ?how t ~f =
    let%map bools = map t ?how ~f in
    List.rev (List.fold2_exn t bools ~init:[]
                ~f:(fun ac x b -> if b then (x :: ac) else ac))
  ;;

  let filter_map ?how t ~f = map t ?how ~f >>| List.filter_opt

  let concat_map ?how t ~f = map t ?how ~f >>| List.concat

  let rec find_map t ~f =
    match t with
    | [] -> return None
    | hd :: tl ->
      match%bind f hd with
      | None -> find_map tl ~f
      | Some _ as some -> return some
  ;;

  let find t ~f =
    find_map t ~f:(fun elt ->
      if%map f elt
      then (Some elt)
      else None)
  ;;

end

let all_unit = List.all_unit

