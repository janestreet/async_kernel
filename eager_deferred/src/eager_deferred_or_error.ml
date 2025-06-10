open! Core
open! Async_kernel
open! Import
module Deferred = Eager_deferred0
module Deferred_array = Eager_deferred_array.Array
module Deferred_list = Eager_deferred_list.List
module Deferred_result = Eager_deferred_result

module Monitor = struct
  let try_with ?(run = `Now) = Monitor.try_with ~run
end

(* Copied from [deferred_or_error.ml].  There should be no diffs below this line. *)

include (Deferred_result : Monad.S2 with type ('a, 'b) t := ('a, 'b) Deferred_result.t)

type 'a t = 'a Or_error.t Deferred.t

include Applicative.Make (struct
    type nonrec 'a t = 'a t

    let return = return

    let apply f x =
      Deferred_result.combine
        f
        x
        ~ok:(fun f x -> f x)
        ~err:(fun e1 e2 -> Error.of_list [ e1; e2 ])
    ;;

    let map = `Custom map
  end)

module Let_syntax = struct
  let return = return

  include Monad_infix

  module Let_syntax = struct
    let return = return
    let map = map
    let bind = bind
    let both = both

    (* from Applicative.Make *)
    module Open_on_rhs = struct end
  end
end

open Let_syntax

let fail error = Deferred.return (Result.fail error)
let ok_exn t = Deferred.map t ~f:Or_error.ok_exn
let of_exn ?backtrace exn = Deferred.return (Or_error.of_exn ?backtrace exn)
let of_exn_result ?backtrace t = Deferred.map t ~f:(Or_error.of_exn_result ?backtrace)
let error msg v sexp_of = Deferred.return (Or_error.error msg v sexp_of)
let error_s sexp = Deferred.return (Or_error.error_s sexp)
let error_string msg = Deferred.return (Or_error.error_string msg)
let errorf format = ksprintf error_string format
let tag t ~tag = Deferred.map t ~f:(Or_error.tag ~tag)
let tag_s t ~tag = Deferred.map t ~f:(Or_error.tag_s ~tag)
let tag_s_lazy t ~tag = Deferred.map t ~f:(Or_error.tag_s_lazy ~tag)

let tag_arg t message a sexp_of_a =
  Deferred.map t ~f:(fun t -> Or_error.tag_arg t message a sexp_of_a)
;;

let unimplemented msg = Deferred.return (Or_error.unimplemented msg)
let combine_errors l = Deferred.map (Deferred.all l) ~f:Or_error.combine_errors
let combine_errors_unit l = Deferred.map (Deferred.all l) ~f:Or_error.combine_errors_unit

let filter_ok_at_least_one l =
  Deferred.map (Deferred.all l) ~f:Or_error.filter_ok_at_least_one
;;

let find_map_ok l ~f =
  match l with
  | [] -> Deferred.return (Or_error.error_string "find_map_ok called on empty list")
  | l ->
    Deferred.repeat_until_finished (l, []) (fun (l, errors) ->
      match l with
      | [] ->
        let errors = Error.of_list (List.rev errors) in
        Deferred.return (`Finished (Error errors))
      | hd :: tl ->
        Deferred.map (f hd) ~f:(function
          | Error current_error -> `Repeat (tl, current_error :: errors)
          | Ok result -> `Finished (Ok result)))
;;

let ok_unit = return ()

let try_with ?extract_exn ?run ?rest ~(here : [%call_pos]) ?name f =
  Deferred.map (Monitor.try_with ?extract_exn ?run ?rest ~here ?name f) ~f:(function
    | Error exn -> Error (Error.of_exn exn)
    | Ok _ as ok -> ok)
;;

let try_with_join ?extract_exn ?run ?rest ~(here : [%call_pos]) ?name f =
  Deferred.map (try_with ?extract_exn ?run ?rest ~here ?name f) ~f:Or_error.join
;;

module type Extras = sig
  type 'a deferred_or_error := 'a t
  type 'a t

  val seqmapi : 'a t -> f:(int -> 'a -> 'b deferred_or_error) -> 'b t deferred_or_error

  val find_mapi
    :  'a t
    -> f:(int -> 'a -> 'b option deferred_or_error)
    -> 'b option deferred_or_error

  (* We could define [foldi] in terms of [Container.foldi], but we use a custom definition
     in order to preserve legacy binding pattern of [Deferred.List.foldi]. *)
  val foldi
    :  'a t
    -> init:'acc
    -> f:(int -> 'acc -> 'a -> 'acc deferred_or_error)
    -> 'acc deferred_or_error
end

module Make_indexed_container
    (Container : sig
       type 'a t

       include
         Base.Indexed_container.Generic_with_creators
         with type ('a, _, _) t := 'a t
          and type ('a, _, _) concat := 'a list
          and type 'a elt := 'a
     end)
    (Monadic_container : Monad_sequence.S
                         with type 'a t := 'a Container.t
                          and type 'a monad := 'a Deferred.t)
    (Extras : Extras with type 'a t := 'a Container.t) :
  Monad_sequence.S with type 'a t := 'a Container.t and type 'a monad := 'a t = struct
  include Extras

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a x -> f a x)

  let all t =
    match%map.Deferred
      Monadic_container.all t
      |> Deferred.map ~f:(Container.partition_map ~f:Result.to_either)
    with
    | ok, errors when Container.is_empty errors -> Ok ok
    | _, errors -> Error (errors |> Container.to_list |> Error.of_list)
  ;;

  let all_unit ts =
    Monadic_container.all ts
    |> Deferred.map ~f:(fun xs -> Container.to_list xs |> Or_error.all_unit)
  ;;

  let iteri ~how t ~f =
    match how with
    | (`Parallel | `Max_concurrent_jobs _) as how ->
      all_unit
        (Container.mapi
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
        (Container.mapi
           t
           ~f:
             (unstage
                (Throttle.monad_sequence_how2 ~on_error:(`Abort `Never_return) ~how ~f)))
    | `Sequential -> seqmapi t ~f
  ;;

  let filter_mapi ~how t ~f = mapi t ~how ~f >>| Container.filter_map ~f:Fn.id
  let concat_mapi ~how t ~f = mapi t ~how ~f >>| Container.concat_map ~f:Fn.id

  let filteri ~how t ~f =
    filter_mapi ~how t ~f:(fun i x ->
      let%map b = f i x in
      if b then Some x else None)
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

  let iter ~how t ~f = iteri ~how t ~f:(fun _ a -> f a)
  let map ~how t ~f = mapi ~how t ~f:(fun _ a -> f a)
  let filter ~how t ~f = filteri ~how t ~f:(fun _ a -> f a)
  let filter_map ~how t ~f = filter_mapi ~how t ~f:(fun _ a -> f a)
  let concat_map ~how t ~f = concat_mapi ~how t ~f:(fun _ a -> f a)
  let find_map t ~f = find_mapi t ~f:(fun _ a -> f a)
  let exists t ~f = existsi t ~f:(fun _ a -> f a)
  let for_all t ~f = for_alli t ~f:(fun _ a -> f a)
  let init ~how n ~f = map ~how (Container.init n ~f:Fn.id) ~f
end

module Array =
  Make_indexed_container (Array) (Deferred_array)
    (struct
      let seqmapi t ~f =
        if Array.is_empty t
        then return [||]
        else (
          let%bind x0 = f 0 (Array.unsafe_get t 0) in
          let a = Array.create ~len:(Array.length t) x0 in
          let rec loop i =
            if i = Array.length t
            then return ()
            else (
              let%bind x = f i (Array.unsafe_get t i) in
              Array.unsafe_set a i x;
              loop (i + 1))
          in
          let%bind () = loop 1 in
          return a)
      ;;

      let find_mapi t ~f =
        let rec loop i =
          if i = Array.length t
          then return None
          else (
            match%bind f i (Array.unsafe_get t i) with
            | None -> loop (i + 1)
            | Some _ as some -> return some)
        in
        loop 0
      ;;

      let foldi t ~init ~f =
        Array.foldi t ~init:(return init) ~f:(fun n acc elt ->
          let%bind acc in
          f n acc elt)
      ;;
    end)

module Iarray = struct
  (* All uses of [unsafe_to_array] are of the form [unsafe_to_array t |> func <args?> <~f?>]
     or [unsafe_to_array |> func <args?> <~f?> >>| unsafe_of_array]. In all cases:
     1) [func] does not mutate the input [array], nor hold a reference to it;
     2) [func] never gives [f] (if relevant) a reference to the whole [array];
     3) [func] never returns the original [array].

     These together ensure it is safe to cast the [iarray] into an [array]. *)
  let unsafe_to_array = Iarray.unsafe_to_array__promise_no_mutation

  (* All uses of [unsafe_of_array] are on [array]s which were just created by a function
     which does not hold a reference to the [array], nor give [f] (if relevant) a reference
     to the whole [array]. This means we have a unique reference to the [array], so it's
     safe to cast it into an [iarray]. *)
  let unsafe_of_array = Iarray.unsafe_of_array__promise_no_mutation
  let fold t ~init ~f = unsafe_to_array t |> Array.fold ~init ~f
  let foldi t ~init ~f = unsafe_to_array t |> Array.foldi ~init ~f
  let find t ~f = unsafe_to_array t |> Array.find ~f
  let findi t ~f = unsafe_to_array t |> Array.findi ~f
  let find_map t ~f = unsafe_to_array t |> Array.find_map ~f
  let find_mapi t ~f = unsafe_to_array t |> Array.find_mapi ~f
  let exists t ~f = unsafe_to_array t |> Array.exists ~f
  let existsi t ~f = unsafe_to_array t |> Array.existsi ~f
  let for_all t ~f = unsafe_to_array t |> Array.for_all ~f
  let for_alli t ~f = unsafe_to_array t |> Array.for_alli ~f
  let all t = unsafe_to_array t |> Array.all >>| unsafe_of_array
  let all_unit t = unsafe_to_array t |> Array.all_unit
  let init ~how n ~f = Array.init ~how n ~f >>| unsafe_of_array
  let iter ~how t ~f = unsafe_to_array t |> Array.iter ~how ~f
  let iteri ~how t ~f = unsafe_to_array t |> Array.iteri ~how ~f
  let map ~how t ~f = unsafe_to_array t |> Array.map ~how ~f >>| unsafe_of_array
  let mapi ~how t ~f = unsafe_to_array t |> Array.mapi ~how ~f >>| unsafe_of_array
  let filter ~how t ~f = unsafe_to_array t |> Array.filter ~how ~f >>| unsafe_of_array
  let filteri ~how t ~f = unsafe_to_array t |> Array.filteri ~how ~f >>| unsafe_of_array

  let filter_map ~how t ~f =
    unsafe_to_array t |> Array.filter_map ~how ~f >>| unsafe_of_array
  ;;

  let filter_mapi ~how t ~f =
    unsafe_to_array t |> Array.filter_mapi ~how ~f >>| unsafe_of_array
  ;;

  let concat_map ~how t ~f = map t ~how ~f >>| Iarray.concat
  let concat_mapi ~how t ~f = mapi t ~how ~f >>| Iarray.concat
end

module List =
  Make_indexed_container (List) (Deferred_list)
    (struct
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

      let foldi t ~init ~f =
        let rec loop i acc = function
          | [] -> return acc
          | hd :: tl ->
            let%bind acc = f i acc hd in
            loop (i + 1) acc tl
        in
        loop 0 init t
      ;;

      let seqmapi t ~f =
        foldi t ~init:[] ~f:(fun i bs a ->
          let%map b = f i a in
          b :: bs)
        >>| List.rev
      ;;
    end)

let repeat_until_finished = Deferred_result.repeat_until_finished
