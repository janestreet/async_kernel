open! Core
open! Async_kernel
open! Import
module Deferred = Eager_deferred0
open Deferred.Let_syntax

(* Copied from [deferred_result.ml].  There should be no diffs below this line. *)

module T = struct
  type ('a, 'error) t = ('a, 'error) Result.t Deferred.t
end

include T

let combine t1 t2 ~ok ~err =
  let%map t1 and t2 in
  Result.combine t1 t2 ~ok ~err
;;

include Monad.Make2 (struct
    include T

    let return a = Deferred.return (Ok a)

    let bind t ~f =
      Deferred.bind t ~f:(function
        | Ok a -> f a
        | Error _ as error -> Deferred.return error)
    ;;

    let map t ~f = Deferred.map t ~f:(fun r -> Result.map r ~f)
    let map = `Custom map
  end)

let fail x = Deferred.return (Error x)
let failf format = Printf.ksprintf fail format
let map_error t ~f = Deferred.map t ~f:(fun r -> Result.map_error r ~f)

let rec repeat_until_finished state f =
  bind (f state) ~f:(function
    | `Repeat state -> repeat_until_finished state f
    | `Finished state -> return state)
;;

module List = struct
  open Let_syntax

  let foldi list ~init:acc ~f =
    let rec loop i acc = function
      | [] -> return acc
      | hd :: tl ->
        let%bind acc = f i acc hd in
        loop (i + 1) acc tl
    in
    loop 0 acc list
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a x -> f a x)

  let seqmapi t ~f =
    foldi t ~init:[] ~f:(fun i bs a ->
      let%map b = f i a in
      b :: bs)
    >>| List.rev
  ;;

  let iteri t ~f = foldi t ~init:() ~f:(fun i () x -> f i x)
  let mapi t ~f = seqmapi t ~f
  let filter_mapi t ~f = mapi t ~f >>| List.filter_opt
  let concat_mapi t ~f = mapi t ~f >>| List.concat

  let filteri t ~f =
    filter_mapi t ~f:(fun i x ->
      let%map b = f i x in
      if b then Some x else None)
  ;;

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

  let find_map t ~f = find_mapi t ~f:(fun _ a -> f a)

  let findi t ~f =
    find_mapi t ~f:(fun i elt ->
      let%map b = f i elt in
      if b then Some (i, elt) else None)
  ;;

  let find t ~f =
    find_map t ~f:(fun elt ->
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

  let iter t ~f = iteri t ~f:(fun _ a -> f a)
  let map t ~f = mapi t ~f:(fun _ a -> f a)
  let filter t ~f = filteri t ~f:(fun _ a -> f a)
  let filter_map t ~f = filter_mapi t ~f:(fun _ a -> f a)
  let concat_map t ~f = concat_mapi t ~f:(fun _ a -> f a)
  let find_map t ~f = find_mapi t ~f:(fun _ a -> f a)
  let exists t ~f = existsi t ~f:(fun _ a -> f a)
  let for_all t ~f = for_alli t ~f:(fun _ a -> f a)
  let init n ~f = map (List.init n ~f:Fn.id) ~f
end
