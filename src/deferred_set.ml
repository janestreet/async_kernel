open Core
open Deferred_std
module Sequence = Deferred_sequence

let for_all t ~f = Set.to_sequence t |> Sequence.for_all ~f
let find t ~f = Set.to_sequence t |> Sequence.find ~f
let find_map t ~f = Set.to_sequence t |> Sequence.find_map ~f
let fold t ~init ~f = Set.to_sequence t |> Sequence.fold ~init ~f

let fold_right t ~init ~f =
  Set.to_sequence ~order:`Decreasing t |> Sequence.fold ~init ~f:(Fn.flip f)
;;

let iter ~how t ~f = Set.to_sequence t |> Sequence.iter ~how ~f

let filter_map (type elt cmp) ((module M) : (elt, cmp) Comparator.Module.t) ~how t ~f =
  Set.to_sequence t
  |> Sequence.fold_mapi
       ~init:(Set.Using_comparator.empty ~comparator:M.comparator)
       ~how
       ~mapi_f:(fun _i a -> f a)
       ~fold_f:(fun acc v ->
         match v with
         | None -> acc
         | Some v -> Set.add acc v)
;;

let filter ~how t ~f =
  filter_map (Set.comparator_s t) ~how t ~f:(fun x -> if%map f x then Some x else None)
;;

let map comparator ~how t ~f =
  filter_map comparator ~how t ~f:(fun x ->
    let%map y = f x in
    Some y)
;;

let count ~how t ~f = Set.to_sequence t |> Sequence.count ~how ~f

let sum (type a) (module M : Base.Container.Summable with type t = a) ~how t ~f =
  Set.to_sequence t |> Sequence.sum (module M) ~how ~f
;;
