open Core_kernel.Std
open Deferred_std

module Deferred = Deferred1

type 'a t = 'a List.t

let foldi t ~init ~f =
  Deferred.create
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
  fold t ~init:[] ~f:(fun bs a -> f a >>| fun b -> b :: bs)
  >>| List.rev
;;

let all ds = seqmap ds ~f:Fn.id

let all_unit ds = Deferred.ignore (fold ds ~init:() ~f:(fun () d -> d))

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
  map t ?how ~f
  >>| fun bools ->
  List.rev (List.fold2_exn t bools ~init:[]
              ~f:(fun ac x b -> if b then x :: ac else ac))
;;

let filter_map ?how t ~f = map t ?how ~f >>| List.filter_opt

let concat_map ?how t ~f = map t ?how ~f >>| List.concat

let rec find_map t ~f =
  match t with
  | [] -> return None
  | hd :: tl ->
    f hd >>= function
    | None -> find_map tl ~f
    | Some _ as some -> return some
;;

let find t ~f =
  find_map t ~f:(fun elt -> f elt >>| fun b -> if b then Some elt else None)
;;
