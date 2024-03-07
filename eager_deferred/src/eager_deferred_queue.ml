open! Core
module Deferred = Async_kernel.Deferred
module Ivar = Async_kernel.Ivar
open Eager_deferred0
open Infix

module Queue = struct
  let with_how ~how ~seq ~general q ~f =
    match how with
    | `Sequential | `Max_concurrent_jobs 1 -> seq q ~f
    | (`Parallel | `Max_concurrent_jobs _) as how -> general ~how q ~f
  ;;

  let init n ~f =
    let result = Queue.create ~capacity:n () in
    Deferred.create (fun result_ivar ->
      let iref = ref 0 in
      let rec loop () =
        let i = !iref in
        if i = n
        then Ivar.fill_exn result_ivar result
        else (
          iref := i + 1;
          f i >>> step)
      and step x =
        Queue.enqueue result x;
        loop ()
      in
      loop ())
  ;;

  let init ~how n ~f = with_how ~how ~seq:init ~general:Deferred.Queue.init n ~f

  let foldi q ~init ~f =
    let iter = Queue.Iteration.start q in
    let len = Queue.length q in
    let iref = ref 0 in
    Deferred.create (fun result_ivar ->
      let rec loop acc =
        let i = !iref in
        Queue.Iteration.assert_no_mutation_since_start iter q;
        if i = len
        then Ivar.fill_exn result_ivar acc
        else (
          let x = Queue.get q i in
          iref := i + 1;
          f i acc x >>> loop)
      in
      loop init)
  ;;

  let fold q ~init ~f = foldi q ~init ~f:(fun (_ : int) acc x -> f acc x)
  let iter q ~f = foldi q ~init:() ~f:(fun _ () a -> f a)
  let iter ~how q ~f = with_how ~how ~seq:iter ~general:Deferred.Queue.iter q ~f
  let iteri q ~f = foldi q ~init:() ~f:(fun i () a -> f i a)
  let iteri ~how q ~f = with_how ~how ~seq:iteri ~general:Deferred.Queue.iteri q ~f

  let map q ~f =
    let res = Queue.create ~capacity:(Queue.length q) () in
    let enqueue fx = Queue.enqueue res fx in
    foldi q ~init:() ~f:(fun _ () x -> f x >>| enqueue) >>| fun () -> res
  ;;

  let map ~how q ~f = with_how ~how ~seq:map ~general:Deferred.Queue.map q ~f

  let mapi q ~f =
    let res = Queue.create ~capacity:(Queue.length q) () in
    let enqueue fx = Queue.enqueue res fx in
    foldi q ~init:() ~f:(fun i () x -> f i x >>| enqueue) >>| fun () -> res
  ;;

  let mapi ~how q ~f = with_how ~how ~seq:mapi ~general:Deferred.Queue.mapi q ~f

  (* To match deferred/monad-sequence, concat map gets queues from [f] instead of lists
     like Queue.concat_map. *)
  let concat_map q ~f =
    let res = Queue.create () in
    let enqueue_all fx = Queue.iter fx ~f:(fun y -> Queue.enqueue res y) in
    foldi q ~init:() ~f:(fun _ () x -> f x >>| enqueue_all) >>| fun () -> res
  ;;

  let concat_map ~how q ~f =
    with_how ~how ~seq:concat_map ~general:Deferred.Queue.concat_map q ~f
  ;;

  let concat_mapi q ~f =
    let res = Queue.create () in
    let enqueue_all fx = Queue.iter fx ~f:(fun y -> Queue.enqueue res y) in
    foldi q ~init:() ~f:(fun i () x -> f i x >>| enqueue_all) >>| fun () -> res
  ;;

  let concat_mapi ~how q ~f =
    with_how ~how ~seq:concat_mapi ~general:Deferred.Queue.concat_mapi q ~f
  ;;

  type 'a filter_map_generic_helper =
    | Filtered_out
    | Enqueue of 'a

  let filter_mapi_generic q ~f ~(of_result : 'a -> 'b -> 'c filter_map_generic_helper) =
    let iter = Queue.Iteration.start q in
    let len = Queue.length q in
    let result = Queue.create () in
    let iref = ref 0 in
    Deferred.create (fun result_ivar ->
      let rec loop () : unit =
        let i = !iref in
        Queue.Iteration.assert_no_mutation_since_start iter q;
        if i = len
        then Ivar.fill_exn result_ivar result
        else (
          let x = Queue.get q i in
          let fx = f i x in
          fx >>> step)
      and step r =
        let i = !iref in
        iref := i + 1;
        Queue.Iteration.assert_no_mutation_since_start iter q;
        match of_result (Queue.get q i) r with
        | Filtered_out -> loop ()
        | Enqueue x ->
          Queue.enqueue result x;
          loop ()
      in
      loop ())
  ;;

  let filter q ~f =
    filter_mapi_generic
      q
      ~f:(fun _ x -> f x)
      ~of_result:(fun x keep -> if keep then Enqueue x else Filtered_out)
  ;;

  let filter ~how q ~f = with_how ~how ~seq:filter ~general:Deferred.Queue.filter q ~f

  let filteri q ~f =
    filter_mapi_generic q ~f ~of_result:(fun x keep ->
      if keep then Enqueue x else Filtered_out)
  ;;

  let filteri ~how q ~f = with_how ~how ~seq:filteri ~general:Deferred.Queue.filteri q ~f

  let filter_map q ~f =
    filter_mapi_generic
      q
      ~f:(fun _ x -> f x)
      ~of_result:(fun _ x ->
        match x with
        | None -> Filtered_out
        | Some x -> Enqueue x)
  ;;

  let filter_map ~how q ~f =
    with_how ~how ~seq:filter_map ~general:Deferred.Queue.filter_map q ~f
  ;;

  let filter_mapi q ~f =
    filter_mapi_generic q ~f ~of_result:(fun _ x ->
      match x with
      | None -> Filtered_out
      | Some x -> Enqueue x)
  ;;

  let filter_mapi ~how q ~f =
    with_how ~how ~seq:filter_mapi ~general:Deferred.Queue.filter_mapi q ~f
  ;;

  type 'a fold_result_helper =
    | Continue
    | Finish of 'a

  let iter_result q ~f ~(consider : _ -> _ -> _ fold_result_helper) ~finish =
    let iter = Queue.Iteration.start q in
    let len = Queue.length q in
    let iref = ref 0 in
    Deferred.create (fun result_ivar ->
      let rec loop () =
        let i = !iref in
        Queue.Iteration.assert_no_mutation_since_start iter q;
        if i = len
        then Ivar.fill_exn result_ivar (finish ())
        else (
          let x = Queue.get q i in
          f i x >>> step)
      and step fx =
        match consider !iref fx with
        | Continue ->
          incr iref;
          loop ()
        | Finish x -> Ivar.fill_exn result_ivar x
      in
      loop ())
  ;;

  let findi q ~f =
    iter_result
      q
      ~f
      ~consider:(fun i keep ->
        if keep then Finish (Some (i, Queue.get q i)) else Continue)
      ~finish:(fun () -> None)
  ;;

  let find q ~f =
    findi q ~f:(fun _ x -> f x) >>| fun res -> Option.map res ~f:(fun (_i, x) -> x)
  ;;

  let find_mapi q ~f =
    iter_result
      q
      ~f
      ~consider:(fun _ r ->
        match r with
        | None -> Continue
        | Some _ -> Finish r)
      ~finish:(fun () -> None)
  ;;

  let find_map q ~f = find_mapi q ~f:(fun _ x -> f x)

  let existsi q ~f =
    iter_result
      q
      ~f
      ~consider:(fun _ r -> if r then Finish true else Continue)
      ~finish:(fun () -> false)
  ;;

  let exists q ~f = existsi q ~f:(fun _ x -> f x)

  let for_alli q ~f =
    iter_result
      q
      ~f
      ~consider:(fun _ r -> if r then Continue else Finish false)
      ~finish:(fun () -> true)
  ;;

  let for_all q ~f = for_alli q ~f:(fun _ x -> f x)
  let all q = map ~how:`Sequential q ~f:Fn.id

  let all_unit q =
    if Queue.is_empty q
    then return ()
    else
      create (fun result_ivar ->
        let to_go = ref (Queue.length q) in
        let decr () =
          decr to_go;
          if !to_go = 0 then Ivar.fill_exn result_ivar ()
        in
        Queue.iter q ~f:(fun (x : unit Deferred.t) -> upon x decr))
  ;;
end
