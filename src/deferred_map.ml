open Core_kernel.Std
open Deferred_std

module Deferred = Deferred1
module List = Deferred_list


type ('a, 'b, 'c) t = ('a, 'b, 'c) Map.t

let change t k ~f = f (Map.find t k) >>| fun opt -> Map.change t k ~f:(fun _ -> opt)

let update t k ~f = f (Map.find t k) >>| fun data -> Map.add t ~key:k ~data

let iteri ?how t ~f =
  List.iter ?how (Map.to_alist t) ~f:(fun (key, data) -> f ~key ~data)
;;

(* DEPRECATED - leaving here for a little while so as to ease the transition for
   external core users. (But marking as deprecated in the mli *)
let iter = iteri

let fold t ~init ~f =
  let alist_in_increasing_key_order =
    Map.fold_right t ~init:[] ~f:(fun ~key ~data alist -> (key, data) :: alist)
  in
  List.fold alist_in_increasing_key_order ~init
    ~f:(fun ac (key, data) -> f ~key ~data ac)
;;

let fold_right t ~init ~f =
  let alist_in_decreasing_key_order =
    Map.fold t ~init:[] ~f:(fun ~key ~data alist -> (key, data) :: alist)
  in
  List.fold alist_in_decreasing_key_order ~init
    ~f:(fun ac (key, data) -> f ~key ~data ac)
;;

module Job = struct
  type ('a, 'b, 'c) t =
    { key            : 'a
    ; data           : 'b
    ; mutable result : 'c option
    }
  [@@deriving fields]
end

let filter_mapi ?how t ~f =
  let jobs = ref [] in
  let job_map =
    Map.mapi t ~f:(fun ~key ~data ->
      let job = { Job. key; data; result = None } in
      jobs := job :: !jobs;
      job)
  in
  List.iter ?how !jobs ~f:(function { Job. key; data; result=_ } as job ->
    f ~key ~data >>| fun x -> job.result <- x)
  >>| fun () ->
  Map.filter_map job_map ~f:Job.result
;;

let filter_map ?how t ~f = filter_mapi ?how t ~f:(fun ~key:_ ~data -> f data)

let filteri ?how t ~f =
  filter_mapi ?how t ~f:(fun ~key ~data ->
    f ~key ~data
    >>| fun b ->
    if b then Some data else None)
;;

(* DEPRECATED - leaving here for a little while so as to ease the transition for
   external core users. (But marking as deprecated in the mli *)
let filter = filteri

let mapi ?how t ~f =
  filter_mapi ?how t ~f:(fun ~key ~data -> f ~key ~data >>| fun z -> Some z)
;;

let map ?how t ~f = mapi ?how t ~f:(fun ~key:_ ~data -> f data)

let merge ?how t1 t2 ~f =
  filter_map ?how (Map.merge t1 t2 ~f:(fun ~key z -> Some (fun () -> f ~key z)))
    ~f:(fun thunk -> thunk ())
;;

let all t = map t ~f:Fn.id
