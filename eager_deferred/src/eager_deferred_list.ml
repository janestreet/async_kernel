open! Core
open! Import
module Ivar = Async_kernel.Ivar
module Deferred = Async_kernel.Deferred
module Throttle = Async_kernel.Throttle
open Eager_deferred0

module List = struct
  open Infix
  open Let_syntax

  let foldi t ~init ~f =
    create (fun result ->
      let rec loop t i b =
        match t with
        | [] -> Ivar.fill_exn result b
        | x :: xs -> f i b x >>> fun b -> loop xs (i + 1) b
      in
      loop t 0 init)
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a x -> f a x)

  let seqmapi l ~f =
    create (fun result ->
      let rec loop i l f acc =
        match l with
        | [] -> Ivar.fill_exn result (List.rev acc)
        | h :: l ->
          let b = f i h in
          (* for some reason, inlining [upon] here by hand gives a significant boost in
             benchmarks (maybe avoids the closure allocation) *)
          if is_determined b
          then loop (i + 1) l f (value_exn b :: acc)
          else Deferred.upon b (fun b -> loop (i + 1) l f (b :: acc))
      in
      loop 0 l f [])
  ;;

  let all ds = seqmapi ds ~f:(fun _ x -> x)
  let all_unit ds = ignore_m (fold ds ~init:() ~f:(fun () d -> d) : unit Deferred.t)

  let iteri ~how t ~f =
    match how with
    | (`Parallel | `Max_concurrent_jobs _) as how ->
      all_unit (List.mapi t ~f:(unstage (Throttle.monad_sequence_how2 ~how ~f)))
    | `Sequential -> foldi t ~init:() ~f:(fun i () x -> f i x)
  ;;

  let mapi ~how t ~f =
    match how with
    | (`Parallel | `Max_concurrent_jobs _) as how ->
      all (List.mapi t ~f:(unstage (Throttle.monad_sequence_how2 ~how ~f)))
    | `Sequential -> seqmapi t ~f
  ;;

  let filteri ~how t ~f =
    let%map bools = mapi t ~how ~f in
    List.rev
      (List.fold2_exn t bools ~init:[] ~f:(fun ac x b -> if b then x :: ac else ac))
  ;;

  let filter_mapi ~how t ~f = mapi t ~how ~f >>| List.filter_opt
  let concat_mapi ~how t ~f = mapi t ~how ~f >>| List.concat

  let find_mapi t ~f =
    let rec find_mapi t ~f i =
      match t with
      | [] -> return None
      | hd :: tl ->
        (match%bind f i hd with
         | None -> find_mapi tl ~f (i + 1)
         | Some _ as some -> return some)
    in
    find_mapi t ~f 0
  ;;

  let findi t ~f =
    find_mapi t ~f:(fun i elt ->
      let%map b = f i elt in
      if b then Some (i, elt) else None)
  ;;

  let find t ~f = find_mapi t ~f:(fun _ elt -> if%map f elt then Some elt else None)

  let existsi t ~f =
    match%map
      find_mapi t ~f:(fun i elt ->
        let%map b = f i elt in
        if b then Some () else None)
    with
    | Some () -> true
    | None -> false
  ;;

  let for_alli t ~f =
    match%map
      find_mapi t ~f:(fun i elt ->
        let%map b = f i elt in
        if not b then Some () else None)
    with
    | Some () -> false
    | None -> true
  ;;

  let iter ~how t ~f = iteri ~how t ~f:(fun _ a -> f a)
  let map ~how t ~f = mapi ~how t ~f:(fun _ a -> f a)
  let filter ~how t ~f = filteri ~how t ~f:(fun _ a -> f a)
  let filter_map ~how t ~f = filter_mapi ~how t ~f:(fun _ a -> f a)
  let concat_map ~how t ~f = concat_mapi ~how t ~f:(fun _ a -> f a)
  let find_map t ~f = find_mapi t ~f:(fun _ a -> f a)
  let exists t ~f = existsi t ~f:(fun _ a -> f a)
  let for_all t ~f = for_alli t ~f:(fun _ a -> f a)
  let init ~how n ~f = map ~how (List.init n ~f:Fn.id) ~f
end
