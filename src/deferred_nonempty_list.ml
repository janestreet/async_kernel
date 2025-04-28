open Core
open Deferred_std
module Deferred = Deferred1

let foldi (type a) (t : a Nonempty_list.t) ~init ~f =
  Deferred.create (fun result ->
    let rec loop (x :: xs : a Nonempty_list.t) i b =
      match xs with
      | [] -> f i b x >>> fun b -> Ivar.fill_exn result b
      | a :: rest -> f i b x >>> fun b -> loop (Nonempty_list.create a rest) (i + 1) b
    in
    loop t 0 init)
;;

let fold t ~init ~f = foldi t ~init ~f:(fun (_ : int) a x -> f a x)

let seqmapi (type a) (t : a Nonempty_list.t) ~f =
  match t with
  | a :: b ->
    let%bind init = f 0 a >>| Nonempty_list.singleton in
    Deferred_list.foldi b ~init ~f:(fun i bs a ->
      let%map b = f (i + 1) a in
      Nonempty_list.cons b bs)
    >>| Nonempty_list.reverse
;;

let all ds = seqmapi ds ~f:(fun (_ : int) x -> x)
let all_unit ds = Deferred.ignore_m (fold ds ~init:() ~f:(fun () d -> d))

let iteri (type a) ~how (t : a Nonempty_list.t) ~f =
  match how with
  | `Parallel as how ->
    all_unit
      (Nonempty_list.mapi
         t
         ~f:
           (unstage
              (Throttle.monad_sequence_how2 ~on_error:(`Abort `Never_return) ~how ~f)))
  | `Max_concurrent_jobs job_count ->
    let rec gen_computation idx (x :: xs : a Nonempty_list.t) =
      match xs with
      | [] -> Throttled.of_thunk (fun () -> Throttled.job (fun () -> f idx x))
      | a :: rest ->
        Throttled.of_thunk (fun () ->
          Throttled.both_unit
            (Throttled.job (fun () -> f idx x))
            (gen_computation (idx + 1) (Nonempty_list.create a rest)))
    in
    Throttled.run (gen_computation 0 t) ~max_concurrent_jobs:job_count
  | `Sequential -> foldi t ~init:() ~f:(fun i () x -> f i x)
;;

let mapi (type a) ~how (t : a Nonempty_list.t) ~f =
  match how with
  | `Parallel as how ->
    all
      (Nonempty_list.mapi
         t
         ~f:
           (unstage
              (Throttle.monad_sequence_how2 ~on_error:(`Abort `Never_return) ~how ~f)))
  | `Max_concurrent_jobs job_count ->
    let rec gen_computation idx (x :: xs : a Nonempty_list.t) =
      match xs with
      | [] ->
        Throttled.of_thunk (fun () ->
          Throttled.apply
            (Throttled.return Nonempty_list.singleton)
            (Throttled.job (fun () -> f idx x)))
      | a :: rest ->
        Throttled.of_thunk (fun () ->
          Throttled.map2
            (Throttled.job (fun () -> f idx x))
            (gen_computation (idx + 1) (Nonempty_list.create a rest))
            ~f:(fun y ys -> Nonempty_list.cons y ys))
    in
    Throttled.run (gen_computation 0 t) ~max_concurrent_jobs:job_count
  | `Sequential -> seqmapi t ~f
;;

let concat_mapi ~how t ~f = mapi t ~how ~f >>| Nonempty_list.concat
let filter_mapi ~how t ~f = mapi t ~how ~f >>| Nonempty_list.filter_opt
let filter_map ~how t ~f = filter_mapi ~how t ~f:(fun (_ : int) a -> f a)
let iter ~how t ~f = iteri ~how t ~f:(fun (_ : int) a -> f a)
let map ~how t ~f = mapi ~how t ~f:(fun (_ : int) a -> f a)
let concat_map ~how t ~f = concat_mapi ~how t ~f:(fun (_ : int) a -> f a)
