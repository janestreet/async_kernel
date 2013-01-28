open Core.Std

module Scheduler = Raw_scheduler

module T = Raw_deferred.Scheduler_dependent (Scheduler) (Ivar.Deferred) (Ivar)

include T

let create = create

let debug_space_leaks = Raw_ivar.debug_space_leaks

let never () = Ivar.read (Ivar.create ())

include (Monad.Make (T))

(* We shadow [all] on-purpose here, since the default definition introduces a chain of
   binds as long as the list. *)
let all = `Make_sure_to_define_all_elsewhere
let _ = all

(* We shadow [map] from Monad with a more efficient implementation *)
let map t ~f = create (fun i -> upon t (fun a -> Ivar.fill i (f a)))

let unit = return ()

module Infix = struct
  include Monad_infix

  let (>>>) = upon

  (* We use the more efficient implementation for the infix map *)
  let (>>|) t f = map t ~f
end

open Infix

module Deferred = struct
  type 'a t = 'a T.t
  let bind = bind
  let map = map
  let return = return
end

let both t1 t2 =
  create (fun result ->
    upon t1 (fun a1 -> upon t2 (fun a2 -> Ivar.fill result (a1, a2))))
;;

let don't_wait_for (_ : unit t) = ()

type 'a choice =
  { register : ready:(unit -> unit) -> Unregister.t;
    check : unit -> (unit -> 'a) option;
  }

let choice t f =
  let check () =
    match peek t with
    | None -> None
    | Some v -> Some (fun () -> f v)
  in
  let register ~ready = upon' t (fun _ -> ready ()) in
  { check; register }
;;

let enabled' choices =
  create (fun result ->
    (* We keep track of all the deferreds we are waiting on so that we can unregister
       ourselves when one of them does become determined.  Else there would be a space
       leak. *)
    let unregisters = Stack.create () in
    (* The list produced by the following [rev_map] is then reversed again by the
       [fold] in [enabled], thus producing a result in the same order as [choices]. *)
    let checks = List.rev_map choices ~f:(fun choice -> choice.check) in
    let ready () =
      if Ivar.is_empty result then begin
        Stack.iter unregisters ~f:Unregister.unregister;
        Ivar.fill result checks;
      end
    in
    List.iter choices ~f:(fun choice ->
      Stack.push unregisters (choice.register ~ready)))
;;

let enabled choices =
  enabled' choices
  >>| fun checks ->
  (fun () ->
    List.fold checks ~init:[] ~f:(fun ac check ->
      match check () with
      | None -> ac
      | Some f -> f () :: ac))
;;

let choose choices =
  enabled' choices
  >>| fun checks ->
  match List.find_map checks ~f:(fun f -> f ()) with
  | None -> assert false
  | Some f -> f ()
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

module List = struct
  type 'a t = 'a List.t

  let fold t ~init ~f =
    create
      (fun result ->
        let rec loop t b =
          match t with
          | [] -> Ivar.fill result b
          | x :: xs -> f b x >>> fun b -> loop xs b
        in
        loop t init)
  ;;

  let seqmap t ~f =
    fold t ~init:[] ~f:(fun bs a -> f a >>| fun b -> b :: bs)
    >>| List.rev
  ;;

  let all ds = seqmap ds ~f:Fn.id

  let all_unit ds = ignore (fold ds ~init:() ~f:(fun () d -> d))

  let iter ?(how = `Sequential) t ~f =
    match how with
    | `Parallel -> all_unit (List.map t ~f)
    | `Sequential -> fold t ~init:() ~f:(fun () a -> f a)
  ;;

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

  let filter_map ?how t ~f =
    map t ?how ~f
    >>| List.filter_opt
  ;;
end

let all = List.all
let all_unit = List.all_unit

module Array = struct
  type 'a t = 'a Array.t

  let fold t ~init ~f =
    create
      (fun result ->
        let rec loop i b =
          if i = Array.length t then
            Ivar.fill result b
          else
            f b t.(i) >>> fun b -> loop (i + 1) b
        in
        loop 0 init)
  ;;

  let seqmap t ~f =
    fold t ~init:[] ~f:(fun bs a -> f a >>| fun b -> b :: bs)
    >>| fun bs -> Array.of_list (Core.Std.List.rev bs)
  ;;

  let all ds = seqmap ds ~f:Fn.id

  let all_unit ds = ignore (fold ds ~init:() ~f:(fun () d -> d))

  let iter ?(how = `Sequential) t ~f =
    match how with
    | `Parallel -> all_unit (Array.map t ~f)
    | `Sequential -> fold t ~init:() ~f:(fun () a -> f a)
  ;;

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

end

module Queue = struct

  type 'a t = 'a Queue.t

  let fold t ~init ~f = Array.fold (Queue.to_array t) ~init ~f

  let all t = Array.all (Queue.to_array t) >>| Queue.of_array

  let all_unit t = Array.all_unit (Queue.to_array t)

  let iter ?how t ~f = Array.iter ?how (Queue.to_array t) ~f

  let map ?how t ~f = Array.map ?how (Queue.to_array t) ~f >>| Queue.of_array

  let init ?how n ~f = Array.init ?how n ~f >>| Queue.of_array

  let filter ?how t ~f = Array.filter ?how (Queue.to_array t) ~f >>| Queue.of_array

  let filter_map ?how t ~f =
    Array.filter_map ?how (Queue.to_array t) ~f >>| Queue.of_array
  ;;

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
      { key : 'a;
        data : 'b;
        mutable result : 'c option;
      }
    with fields
  end

  let filter_mapi ?how t ~f =
    let jobs = ref [] in
    let job_map =
      Map.mapi t ~f:(fun ~key ~data ->
        let job = { Job.key; data; result = None } in
        jobs := job :: !jobs;
        job)
    in
    List.iter ?how !jobs ~f:(function { Job.key; data; _ } as job ->
      f ~key ~data >>| fun x -> job.Job.result <- x)
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
  end)

  let map t ~f = Deferred.map t ~f:(fun r -> Result.map r ~f)

  let (>>|) t f = map t ~f
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
  end)

  let map t ~f = Deferred.map t ~f:(fun r -> Option.map r ~f)

  let (>>|) t f = map t ~f
end

