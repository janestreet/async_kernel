open Core.Std
open Deferred_intf

module Scheduler = Raw_scheduler

include Ivar.Deferred

(* To avoid a space leak, it is necessary that [never] allocates a new ivar whenever it is
   called.  Code can bind on [never ()], so if we re-used the ivar, we could endlessly
   accumulate handlers. *)
let never () = Ivar.read (Ivar.create ())

include Monad.Make (struct
  include Ivar.Deferred
  let map t ~f = create (fun i -> upon t (fun a -> Ivar.fill i (f a)))
  let map = `Custom map
end)

(* We shadow [all] on-purpose here, since the default definition introduces a chain of
   binds as long as the list. *)
let all = `Make_sure_to_define_all_elsewhere
let _ = all

let unit = return ()

module Infix = struct
  include Monad_infix

  let (>>>) = upon
end

open Infix

module Deferred = struct
  type nonrec 'a t = 'a t
  let bind = bind
  let map = map
  let return = return
end

let both t1 t2 =
  create (fun result ->
    upon t1 (fun a1 -> upon t2 (fun a2 -> Ivar.fill result (a1, a2))))
;;

let don't_wait_for (_ : unit t) = ()

type +'a choice = Choice : 'b t * ('b -> 'a) -> 'a choice

module Unregister = struct
  (* This representation saves 2n words for a list of n choices. *)
  type t =
    | Nil : t
    | Cons : 'a Ivar.Deferred.t * 'a Ivar.Deferred.Handler.t * t -> t

  let rec process = function
    | Nil -> ()
    | Cons (t, handler, rest) ->
      remove_handler t handler;
      process rest
  ;;
end

let choice t f = Choice (t, f)

let enabled choices =
  let result = Ivar.create () in
  let unregisters = ref Unregister.Nil in
  let ready _ =
    if Ivar.is_empty result then begin
      Unregister.process !unregisters;
      Ivar.fill result (fun () ->
        List.rev
          (List.fold choices ~init:[] ~f:(fun ac (Choice (t, f)) ->
             match peek t with
             | None -> ac
             | Some v -> f v :: ac)))
    end
  in
  let execution_context = Scheduler.(current_execution_context (t ())) in
  unregisters :=
    List.fold choices ~init:Unregister.Nil ~f:(fun acc (Choice (t, _)) ->
      Cons (t,
            Ivar.Deferred.add_handler t ready execution_context,
            acc));
  Ivar.read result
;;

let rec choose_result choices =
  match choices with
  | [] -> assert false
  | Choice (t, f) :: choices ->
    match peek t with
    | None -> choose_result choices
    | Some v -> f v
;;

let choose choices =
  let result = Ivar.create () in
  let unregisters = ref Unregister.Nil in
  let ready _ =
    if Ivar.is_empty result then begin
      Unregister.process !unregisters;
      Ivar.fill result (choose_result choices)
    end
  in
  let execution_context = Scheduler.(current_execution_context (t ())) in
  unregisters :=
    List.fold choices ~init:Unregister.Nil ~f:(fun acc (Choice (t, _)) ->
      Cons (t,
            Ivar.Deferred.add_handler t ready execution_context,
            acc));
  Ivar.read result
;;

let any_f ts f = choose (List.map ts ~f:(fun t -> choice t f))
let any      ts = any_f ts Fn.id
let any_unit ts = any_f ts Fn.ignore

let repeat_until_finished state f =
  create (fun finished ->
    let rec loop state =
      f state
      >>> function
      | `Repeat state -> loop state
      | `Finished result -> Ivar.fill finished result
    in
    loop state)
;;

let forever state f =
  repeat_until_finished state (fun state -> f state >>| fun state -> `Repeat state)
  >>> fun () ->
  assert false
;;

module type Monad_sequence = Monad_sequence with type 'a monad := 'a t

module Sequence = struct
  type 'a t = 'a Sequence.t

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

  let maybe_force ?(how = `Sequential) t =
    match how with
    | `Parallel   -> Sequence.force_eagerly t
    | `Sequential -> t
  ;;

  let iteri ?how t ~f = all_unit (maybe_force ?how (Sequence.mapi t ~f))

  let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)

  let map ?how t ~f = all (maybe_force ?how (Sequence.map t ~f))

  (* [filter_map] is implemented separately from [map] so that we never need to keep a
     long stream of intermediate [None] results in the accumulator, only to later filter
     them all out. *)
  let filter_map ?how t ~f =
    fold (maybe_force ?how (Sequence.map t ~f)) ~init:[] ~f:(fun acc maybe_v ->
      maybe_v
      >>| function
      | None   -> acc
      | Some v -> v :: acc)
    >>| fun s ->
    Sequence.of_list (List.rev s)
  ;;

  let filter ?how t ~f =
    filter_map ?how t ~f:(fun a ->
      f a
      >>| function
      | true -> Some a
      | false -> None)
  ;;

  let init ?how n ~f = map ?how (Sequence.init n ~f:Fn.id) ~f
end

module List = struct
  type 'a t = 'a List.t

  let foldi t ~init ~f =
    create
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

  let all_unit ds = ignore (fold ds ~init:() ~f:(fun () d -> d))

  let iteri ?(how = `Sequential) t ~f =
    match how with
    | `Parallel -> all_unit (List.mapi t ~f)
    | `Sequential -> foldi t ~init:() ~f:(fun i () x -> f i x)
  ;;

  let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)

  let map ?(how = `Sequential) t ~f =
    match how with
    | `Parallel -> all (List.map t ~f)
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

end

let all = List.all
let all_unit = List.all_unit

module Array = struct
  type 'a t = 'a Array.t

  let foldi t ~init ~f =
    create
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
    >>| fun bs -> Array.of_list (Core.Std.List.rev bs)
  ;;

  let all ds = seqmap ds ~f:Fn.id

  let all_unit ds = ignore (fold ds ~init:() ~f:(fun () d -> d))

  let iteri ?(how = `Sequential) t ~f =
    match how with
    | `Parallel -> all_unit (Array.mapi t ~f)
    | `Sequential -> foldi t ~init:() ~f:(fun i () x -> f i x)
  ;;

  let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)

  let map ?(how = `Sequential) t ~f =
    match how with
    | `Parallel -> all (Array.map t ~f)
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

end

module Queue = struct
  (* We implement all of the [Queue] operations by converting the queue to a list and then
     using the corresponding [List] operation.  We use lists rather than arrays because
     arrays longer than a certain length are allocated in the major heap, which can cause
     unnecessary promotion of the elements in the queue.  Also, when one is folding or
     iterating over an array, the entire array must be kept alive.  When folding or
     iterating over a list, only the remaining tail of the list is kept alive.  So, using
     arrays rather than lists would increase the live-space needed by the program. *)

  type 'a t = 'a Queue.t

  let foldi t ~init ~f = List.foldi (Queue.to_list t) ~init ~f

  let fold t ~init ~f = List.fold (Queue.to_list t) ~init ~f

  let all t = List.all (Queue.to_list t) >>| Queue.of_list

  let all_unit t = List.all_unit (Queue.to_list t)

  let iter ?how t ~f = List.iter ?how (Queue.to_list t) ~f

  let iteri ?how t ~f = List.iteri ?how (Queue.to_list t) ~f

  let map ?how t ~f = List.map ?how (Queue.to_list t) ~f >>| Queue.of_list

  let init ?how n ~f = List.init ?how n ~f >>| Queue.of_list

  let filter ?how t ~f = List.filter ?how (Queue.to_list t) ~f >>| Queue.of_list

  let filter_map ?how t ~f = List.filter_map ?how (Queue.to_list t) ~f >>| Queue.of_list

  let find_map t ~f = List.find_map (Queue.to_list t) ~f

  let find t ~f = List.find (Queue.to_list t) ~f
end

module Map = struct

  type ('a, 'b, 'c) t = ('a, 'b, 'c) Map.t

  let change t k f = f (Map.find t k) >>| fun opt -> Map.change t k (fun _ -> opt)

  let iter ?how t ~f =
    List.iter ?how (Map.to_alist t) ~f:(fun (key, data) -> f ~key ~data)
  ;;

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
    with fields
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

  let filter ?how t ~f =
    filter_mapi ?how t ~f:(fun ~key ~data ->
      f ~key ~data
      >>| fun b ->
      if b then Some data else None)
  ;;

  let mapi ?how t ~f =
    filter_mapi ?how t ~f:(fun ~key ~data -> f ~key ~data >>| fun z -> Some z)
  ;;

  let map ?how t ~f = mapi ?how t ~f:(fun ~key:_ ~data -> f data)

  let merge ?how t1 t2 ~f =
    filter_map ?how (Map.merge t1 t2 ~f:(fun ~key z -> Some (fun () -> f ~key z)))
      ~f:(fun thunk -> thunk ())
  ;;

end

module Result = struct
  module T = struct
    type ('a, 'error) t = ('a, 'error) Result.t Deferred.t
  end

  include T

  include Monad.Make2 (struct
    include T

    let return a = Deferred.return (Ok a)

    let bind t f =
      Deferred.bind t (function
        | Ok a -> f a
        | Error _ as error -> Deferred.return error)
    ;;

    let map t ~f = Deferred.map t ~f:(fun r -> Result.map r ~f)
    let map = `Custom map
  end)

  let map_error t ~f = Deferred.map t ~f:(fun r -> Result.map_error r ~f)
end

module Option = struct
  module T = struct
    type 'a t = 'a Option.t Deferred.t
  end

  include T

  include Monad.Make (struct
    include T

    let return a = Deferred.return (Some a)

    let bind t f =
      Deferred.bind t (function
        | Some a -> f a
        | None -> Deferred.return None)
    ;;

    let map t ~f = Deferred.map t ~f:(fun r -> Option.map r ~f)
    let map = `Custom map
  end)
end
