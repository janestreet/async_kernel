open Core.Std
open Import

module Deferred = Raw_deferred
module Scheduler = Raw_scheduler

open Deferred

module T = struct
  include Raw_deferred.Scheduler_dependent (Scheduler)
  let return = return
end

include T

let create = create

let peek = peek

let is_determined = is_determined

let debug_space_leaks = Raw_ivar.debug_space_leaks

let never () = Ivar.read (Ivar.create ())

let (>>>) = upon

include (Monad.Make (T))

(* We shadow [all] on-purpose here, since the default definition introduces a chain of
   binds as long as the list. *)
let all = `Make_sure_to_define_all_elsewhere

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

let both t1 t2 =
  create (fun result ->
    upon t1 (fun a1 -> upon t2 (fun a2 -> Ivar.fill result (a1, a2))))
;;

let whenever _ = ()

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

let choose_ident choices = choose (List.map choices ~f:(fun t -> choice t ident))

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

  let filter ?how t ~f = Array.filter ?how (Queue.to_array t) ~f >>| Queue.of_array

  let filter_map ?how t ~f = Array.filter_map ?how (Queue.to_array t) ~f >>| Queue.of_array

end

module Map = struct

  type ('a, 'b) t = ('a, 'b) Map.Poly.t

  let filter_mapi t ~f =
    List.fold (Map.to_alist t) ~init:Map.Poly.empty ~f:(fun map (key, data) ->
      f ~key ~data
      >>| function
      | Some data -> Map.add map ~key ~data
      | None -> map)
end
