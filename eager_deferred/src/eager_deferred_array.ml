open! Core
open! Import
module Ivar = Async_kernel.Ivar
module Throttle = Async_kernel.Throttle
open Eager_deferred0

module Array = struct
  open Infix
  open Let_syntax

  let foldi t ~init ~f =
    create (fun result ->
      let rec loop i b =
        if i = Array.length t
        then Ivar.fill_exn result b
        else f i b t.(i) >>> fun b -> loop (i + 1) b
      in
      loop 0 init)
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a x -> f a x)

  let seqmapi t ~f =
    create (fun result ->
      let rec loop i output =
        if i = Array.length t
        then Ivar.fill_exn result output
        else
          f i (Array.get t i)
          >>> fun b ->
          let output = if i = 0 then Array.create ~len:(Array.length t) b else output in
          Array.set output i b;
          loop (i + 1) output
      in
      loop 0 [||])
  ;;

  let all ds = seqmapi ds ~f:(fun _ x -> x)
  let all_unit ds = ignore_m (fold ds ~init:() ~f:(fun () d -> d))

  let iteri ~how t ~f =
    match how with
    | (`Parallel | `Max_concurrent_jobs _) as how ->
      all_unit
        (Array.mapi
           t
           ~f:
             (unstage
                (Throttle.monad_sequence_how2 ~on_error:(`Abort `Never_return) ~how ~f)))
    | `Sequential -> foldi t ~init:() ~f:(fun i () x -> f i x)
  ;;

  let mapi ~how t ~f =
    match how with
    | (`Parallel | `Max_concurrent_jobs _) as how ->
      all
        (Array.mapi
           t
           ~f:
             (unstage
                (Throttle.monad_sequence_how2 ~on_error:(`Abort `Never_return) ~how ~f)))
    | `Sequential -> seqmapi t ~f
  ;;

  let filteri ~how t ~f =
    let%map bools = mapi t ~how ~f in
    Array.of_list_rev
      (Array.fold2_exn t bools ~init:[] ~f:(fun ac x b -> if b then x :: ac else ac))
  ;;

  let filter_mapi ~how t ~f = mapi t ~how ~f >>| Array.filter_opt

  let concat_mapi ~how t ~f =
    let%map t = mapi t ~how ~f in
    Array.concat (Array.to_list t)
  ;;

  let find_mapi t ~f =
    let rec aux i =
      if i = Array.length t
      then return None
      else (
        match%bind f i t.(i) with
        | None -> aux (i + 1)
        | Some _ as some -> return some)
    in
    aux 0
  ;;

  let findi t ~f =
    find_mapi t ~f:(fun i elt ->
      let%map b = f i elt in
      if b then Some (i, elt) else None)
  ;;

  let find t ~f =
    find_mapi t ~f:(fun _ elt ->
      let%map b = f elt in
      if b then Some elt else None)
  ;;

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
  let filter_map ~how t ~f = filter_mapi ~how t ~f:(fun _ a -> f a)
  let filter ~how t ~f = filteri ~how t ~f:(fun _ a -> f a)
  let concat_map ~how t ~f = concat_mapi ~how t ~f:(fun _ a -> f a)
  let find_map t ~f = find_mapi t ~f:(fun _ a -> f a)
  let exists t ~f = existsi t ~f:(fun _ a -> f a)
  let for_all t ~f = for_alli t ~f:(fun _ a -> f a)
  let init ~how n ~f = map ~how (Array.init n ~f:Fn.id) ~f
end
