open Core_kernel.Std
open Deferred_std

module Deferred = Deferred1

type 'a t = 'a Array.t

let foldi t ~init ~f =
  Deferred.create
    (fun result ->
       let rec loop i b =
         if i = Array.length t
         then Ivar.fill result b
         else f i b t.(i) >>> fun b -> loop (i + 1) b
       in
       loop 0 init)
;;

let fold t ~init ~f = foldi t ~init ~f:(fun _ a -> f a)

let seqmap t ~f =
  fold t ~init:[] ~f:(fun bs a -> f a >>| fun b -> b :: bs)
  >>| fun bs -> Array.of_list (Core_kernel.Std.List.rev bs)
;;

let all ds = seqmap ds ~f:Fn.id

let all_unit ds = Deferred.ignore (fold ds ~init:() ~f:(fun () d -> d))

let iteri ?(how = `Sequential) t ~f =
  match how with
  | `Parallel | `Max_concurrent_jobs _ as how ->
    all_unit (Array.mapi t ~f:(unstage (Throttle.monad_sequence_how2 ~how ~f)))
  | `Sequential -> foldi t ~init:() ~f:(fun i () x -> f i x)
;;

let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)

let map ?(how = `Sequential) t ~f =
  match how with
  | `Parallel | `Max_concurrent_jobs _ as how ->
    all (Array.map t ~f:(unstage (Throttle.monad_sequence_how ~how ~f)))
  | `Sequential -> seqmap t ~f
;;

let init ?how n ~f = map ?how (Array.init n ~f:Fn.id) ~f

let filter ?how t ~f =
  map t ?how ~f
  >>| fun bools ->
  Array.of_list_rev
    (Array.fold2_exn t bools ~init:[] ~f:(fun ac x b ->
       if b then x :: ac else ac))
;;

let filter_map ?how t ~f = map t ?how ~f >>| Array.filter_opt

let concat_map ?how t ~f = map t ?how ~f >>| fun t -> Array.concat (Array.to_list t)

let find_map t ~f =
  let rec aux i =
    if i = Array.length t
    then return None
    else
      f t.(i) >>= function
      | None -> aux (i + 1)
      | Some _ as some -> return some
  in
  aux 0
;;

let find t ~f =
  find_map t ~f:(fun elt -> f elt >>| fun b -> if b then Some elt else None)
;;
