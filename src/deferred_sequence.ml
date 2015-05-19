open Core_kernel.Std
open Deferred_std

module Deferred = Deferred1

type 'a t = 'a Sequence.t

(* [fold_mapi ?how t ~init ~mapi_f ~fold_f] is a more efficient version of:

   {[
     fold ~init ~f:(fun b a -> return (fold_f b a)) (mapi t ?how ~f:mapi_f)
   ]}

   It avoids creating the intermediate sequence that would result from [mapi], and
   allows the [fold] to proceed concurrently with the [mapi], so that one can accumulate
   the result as soon as possible, possibly avoiding creating an intermediate structure
   (e.g. [iteri] and [filter_map] uses [fold_mapi] to do this). *)
let fold_mapi
      (type a) (type b) (type c)
      ?(how = `Sequential)
      (t : a t)
      ~(init : c)
      ~(mapi_f : int -> a -> b Deferred.t)
      ~(fold_f : c -> b -> c)
  : c Deferred.t
  =
  match how with
  | `Sequential ->
    let rec loop i t (c : c) =
      match Sequence.next t with
      | None -> return c
      | Some (a, t) ->
        mapi_f i a
        >>= fun b ->
        loop (i + 1) t (fold_f c b)
    in
    loop 0 t init
  | `Parallel ->
    let rec loop i t (c : c Deferred.t) =
      match Sequence.next t with
      | None -> c
      | Some (a, t) ->
        loop (i + 1) t (mapi_f i a
                        >>= fun b ->
                        c
                        >>| fun c ->
                        fold_f c b)
    in
    loop 0 t (return init)
  | `Max_concurrent_jobs max_concurrent_jobs ->
    let throttle = Throttle.create ~max_concurrent_jobs ~continue_on_error:false in
    (* [loop] forces the input sequence and enqueues a throttle job only if there is
       capacity available. *)
    let rec loop i t (c : c Deferred.t) =
      Throttle.capacity_available throttle
      >>= fun () ->
      match Sequence.next t with
      | None -> c
      | Some (a, t) ->
        loop (i + 1) t (Throttle.enqueue throttle (fun () -> mapi_f i a)
                        >>= fun b ->
                        c
                        >>| fun c ->
                        fold_f c b)
    in
    loop 0 t (return init)
;;

let foldi t ~init ~f =
  Sequence.delayed_fold t ~init:(0, init)
    ~f:(fun (i, b) a ~k -> f i b a >>= fun b -> k (i + 1, b))
    ~finish:(fun (_, b) -> return b)
;;

(* [fold] is not implemented in terms of [foldi] to save the intermediate closure
   allocation. *)
let fold t ~init ~f =
  Sequence.delayed_fold t ~init
    ~f:(fun b a ~k -> f b a >>= k)
    ~finish:return
;;

let all t =
  fold t ~init:[] ~f:(fun accum d -> d >>| fun a -> a :: accum)
  >>| fun res ->
  Sequence.of_list (List.rev res)
;;

let all_unit t = fold t ~init:() ~f:(fun () v -> v)

let rec find_map t ~f =
  match Sequence.next t with
  | None           -> return None
  | Some (v, rest) ->
    f v >>= function
    | None           -> find_map rest ~f
    | Some _ as some -> return some
;;

let find t ~f =
  find_map t ~f:(fun elt -> f elt >>| fun b -> if b then Some elt else None)
;;

let iteri ?how t ~f : unit Deferred.t =
  fold_mapi ?how t ~mapi_f:f ~init:() ~fold_f:(fun () () -> ())
;;

let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)

let map ?how t ~f =
  fold_mapi ?how t ~mapi_f:(fun _ a -> f a) ~init:[] ~fold_f:(fun bs b -> b :: bs)
  >>| fun bs ->
  Sequence.of_list (List.rev bs)
;;

(* [filter_map] is implemented using [fold_mapi] rather than [map] so that we never need
   to keep a long stream of intermediate [None] results in the accumulator, only to later
   filter them all out. *)
let filter_map ?how t ~f =
  fold_mapi t ?how ~mapi_f:(fun _ a -> f a) ~init:[] ~fold_f:(fun bs maybe_v ->
    match maybe_v with
    | None   -> bs
    | Some b -> b :: bs)
  >>| fun bs ->
  Sequence.of_list (List.rev bs)
;;

let concat_map ?how t ~f = map ?how t ~f >>| Sequence.concat

let filter ?how t ~f =
  filter_map ?how t ~f:(fun a ->
    f a
    >>| function
    | true -> Some a
    | false -> None)
;;

let init ?how n ~f = map ?how (Sequence.init n ~f:Fn.id) ~f
