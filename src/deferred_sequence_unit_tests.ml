open! Core_kernel.Std
open! Import
open! Deferred_std

let stabilize = Scheduler.run_cycles_until_no_jobs_remain

let numbers = Sequence.range 1 100

let assert_sequences_equal s1 s2 =
  assert (Sequence.to_list s1 = Sequence.to_list s2)
;;

let deferred_result d =
  let deferred_result = ref None in
  upon d (fun v -> deferred_result := Some v);
  stabilize ();
  Option.value_exn !deferred_result
;;

open Deferred.Sequence

type nonrec 'a t = 'a Sequence.t

let fold = fold

let%test_unit _ =
  let init = 0 in
  let f acc v = v + acc in
  let deferred_acc =
    deferred_result
      (Deferred.Sequence.fold numbers ~init ~f:(fun acc v -> return (f acc v)))
  in
  assert (Sequence.fold numbers ~init ~f = deferred_acc)
;;

let foldi = foldi

let%test_unit _ =
  let init = 0 in
  let f i acc _v = i + acc in
  let deferred_acc =
    deferred_result
      (Deferred.Sequence.foldi numbers ~init ~f:(fun i acc v -> return (f i acc v)))
  in
  assert (Sequence.foldi numbers ~init ~f = deferred_acc)
;;

let filter = filter

let%test_unit _ =
  let f i = i % 2 = 0 in
  let deferred_result =
    deferred_result
      (Deferred.Sequence.filter numbers ~f:(fun i -> return (f i)))
  in
  assert_sequences_equal (Sequence.filter numbers ~f) deferred_result
;;

let filter_map = filter_map

let%test_unit _ =
  let f i = if i % 2 = 0 then Some i else None in
  let deferred_result =
    deferred_result
      (Deferred.Sequence.filter_map numbers ~f:(fun i -> return (f i)))
  in
  assert_sequences_equal (Sequence.filter_map numbers ~f) deferred_result
;;

let concat_map = concat_map

let%test_unit _ =
  let f i = Sequence.init i ~f:(fun j -> i + j) in
  let deferred_result =
    deferred_result
      (Deferred.Sequence.concat_map numbers ~f:(fun i -> return (f i)))
  in
  assert_sequences_equal (Sequence.concat_map numbers ~f) deferred_result
;;

let map = map

let%test_unit _ =
  let f i = i * 2 in
  let serial_result =
    deferred_result
      (Deferred.Sequence.map ~how:`Sequential numbers ~f:(fun i -> return (f i)))
  in
  let parallel_result =
    deferred_result
      (Deferred.Sequence.map ~how:`Parallel   numbers ~f:(fun i -> return (f i)))
  in
  assert_sequences_equal (Sequence.map numbers ~f) serial_result;
  assert_sequences_equal (Sequence.map numbers ~f) parallel_result
;;

let iter = iter

let%test_unit _ =
  let side_effect = ref 0 in
  deferred_result
    (Deferred.Sequence.iter numbers ~f:(fun _ ->
       incr side_effect;
       Deferred.unit));
  assert (!side_effect <> 0);
  Sequence.iter numbers ~f:(fun _ -> decr side_effect);
  assert (!side_effect = 0)
;;

let iteri = iteri

let%test_unit _ =
  let side_effect = ref 0 in
  deferred_result
    (Deferred.Sequence.iteri numbers ~f:(fun i _ ->
       side_effect := !side_effect + i;
       Deferred.unit));
  assert (!side_effect <> 0);
  Sequence.iteri numbers ~f:(fun i _ -> side_effect := !side_effect - i);
  assert (!side_effect = 0)
;;

let all      = all
let all_unit = all_unit
let find     = find
let find_map = find_map
let init     = init

let%test_unit _ =
  for n = 0 to 5 do
    [%test_result: int Sequence.t] ~expect:(Sequence.init n ~f:Fn.id)
      (deferred_result (all (Sequence.init n ~f:return)));
    [%test_result: unit] ~expect:()
      (deferred_result (all_unit (Sequence.init n ~f:(fun _ -> return ()))));
    [%test_result: int option] ~expect:(if n = 0 then None else Some (n - 1))
      (deferred_result (find (Sequence.init n ~f:Fn.id) ~f:(fun i -> return (i = n-1))));
    [%test_result: string option] ~expect:(if n = 0 then None else Some "yes")
      (deferred_result (find_map (Sequence.init n ~f:Fn.id) ~f:(fun i ->
         return (if i = n-1 then Some "yes" else None))));
    [%test_result: int Sequence.t] ~expect:(Sequence.init n ~f:Fn.id)
      (deferred_result (init n ~f:return));
    [%test_result: int Sequence.t] ~expect:(Sequence.init n ~f:(fun i -> i - 1))
      (deferred_result (map (Sequence.init n ~f:Fn.id) ~f:(fun i -> return (i - 1))));
  done
;;
