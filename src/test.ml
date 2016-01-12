open Core_kernel.Std
open Std

let%test_module _ = (module struct
  let stabilize () = Scheduler.run_cycles_until_no_jobs_remain ()

  let%test_unit _ =
    let i1 = Ivar.create () in
    let i2 = Ivar.create () in
    let c = Deferred.any [ Ivar.read i1; Ivar.read i2 ] in
    stabilize ();
    assert (Deferred.peek c = None);
    Ivar.fill i1 13;
    stabilize ();
    assert (Deferred.peek c = Some 13);
    Ivar.fill i2 14;
    stabilize ();
    assert (Deferred.peek c = Some 13)
  ;;

  module Deferred_map = struct
    module M = Deferred.Map
    type how = Monad_sequence.how [@@deriving sexp_of]
    let hows = [ `Sequential; `Parallel ]
    type t = string Int.Map.t [@@deriving sexp_of]
    type k = int [@@deriving sexp_of]
    let k1 = 1 and k2 = 2 and k3 = 3
    let ks = [ k1; k2; k3 ]
    let t1 = Int.Map.of_alist_exn []
    let t2 = Int.Map.of_alist_exn [ (k1, "one"); (k2, "two"); (k3, "three") ]
    let ts = [ t1; t2 ]

    let equal = Int.Map.equal String.equal

    let%test_unit _ =
      let fs =
        [ (fun _ -> None)
        ; (fun _ -> Some "z")
        ; (function None -> Some "None" | Some x -> Some ("Some " ^ x))
        ]
      in
      List.iter ts ~f:(fun t ->
        List.iter ks ~f:(fun k ->
          List.iter fs ~f:(fun f ->
            let d = M.change t k ~f:(fun x -> return (f x)) in
            stabilize ();
            let o1 = Deferred.peek d in
            let o2 = Some (Core_kernel.Std.Map.change t k ~f) in
            if not (Option.equal equal o1 o2)
            then failwiths "Deferred.Map.change failed" (t, k, o1, o2)
                   [%sexp_of: t * k * t option * t option])))
    ;;

    let%test_unit _ =
      let fs =
        [ (fun _ -> "z")
        ; (function None -> "None" | Some x -> "Some " ^ x)
        ]
      in
      List.iter ts ~f:(fun t ->
        List.iter ks ~f:(fun k ->
          List.iter fs ~f:(fun f ->
            let d = M.update t k ~f:(fun x -> return (f x)) in
            stabilize ();
            let o1 = Deferred.peek d in
            let o2 = Some (Core_kernel.Std.Map.update t k ~f) in
            if not (Option.equal equal o1 o2)
            then failwiths "Deferred.Map.update failed" (t, k, o1, o2)
                   [%sexp_of: t * k * t option * t option])))
    ;;

    let%test_unit _ =
      List.iter ts ~f:(fun t ->
        List.iter hows ~f:(fun how ->
          let r = ref 0 in
          ignore (M.iteri t ~how ~f:(fun ~key ~data:_ -> return (r := !r + key)));
          stabilize ();
          let i1 = !r in
          let i2 = Core_kernel.Std.Map.fold t ~init:0 ~f:(fun ~key ~data:_ ac -> key + ac) in
          if i1 <> i2
          then failwiths "Deferred.Map.iteri failed" (t, how, i1, i2)
                 [%sexp_of: t * how * int * int]))
    ;;

    let test_map_like name f =
      List.iter ts ~f:(fun t ->
        List.iter hows ~f:(fun how ->
          let (c, d) = f t ~how in
          stabilize ();
          let o1 = Deferred.peek d in
          let o2 = Some c in
          if not (Option.equal equal o1 o2)
          then failwiths ("Deferred.Map."^name^" failed") (t, o1, o2)
                 [%sexp_of: t * t option * t option]))
    ;;

    let%test_unit _ =
      List.iter [ fun x -> x ^ "zzz" ]
        ~f:(fun f ->
          test_map_like "map"
            (fun t ~how ->
               (Core_kernel.Std.Map.map t ~f,
                M.map t ~how ~f:(fun x -> return (f x)))))
    ;;

    let%test_unit _ =
      List.iter [ fun ~key ~data -> Int.to_string key ^ data ]
        ~f:(fun f ->
          test_map_like "mapi"
            (fun t ~how ->
               (Core_kernel.Std.Map.mapi t ~f,
                M.mapi ~how t ~f:(fun ~key ~data -> return (f ~key ~data)))))
    ;;

    let%test_unit _ =
      List.iter
        [ (fun ~key:_ ~data:_ -> false)
        ; (fun ~key:_ ~data:_ -> true)
        ; (fun ~key ~data -> key = 1 || data = "two")
        ]
        ~f:(fun f ->
          test_map_like "filteri"
            (fun t ~how ->
               (Core_kernel.Std.Map.filteri t ~f,
                M.filteri ~how t ~f:(fun ~key ~data -> return (f ~key ~data)))))
    ;;

    let%test_unit _ =
      List.iter
        [ (fun _ -> None)
        ; (fun _ -> Some "z")
        ; (fun data -> Some data)
        ; (fun data -> if data = "one" then None else Some data)
        ]
        ~f:(fun f ->
          test_map_like "filter_map"
            (fun t ~how ->
               (Core_kernel.Std.Map.filter_map t ~f,
                M.filter_map ~how t ~f:(fun data -> return (f data)))))
    ;;

    let%test_unit _ =
      List.iter
        [ (fun ~key:_ ~data:_ -> None)
        ; (fun ~key:_ ~data:_ -> Some "z")
        ; (fun ~key ~data -> Some (Int.to_string key ^ data))
        ]
        ~f:(fun f ->
          test_map_like "filter_mapi"
            (fun t ~how ->
               (Core_kernel.Std.Map.filter_mapi t ~f,
                M.filter_mapi ~how t ~f:(fun ~key ~data -> return (f ~key ~data)))))
    ;;

    let%test_unit _ =
      let folds =
        [ "fold"      , M.fold      , Core_kernel.Std.Map.fold
        ; "fold_right", M.fold_right, Core_kernel.Std.Map.fold_right
        ]
      in
      let fs = [ fun ~key ~data ac -> (Int.to_string key ^ data) ^ ac ] in
      List.iter folds ~f:(fun (name, m_fold, core_fold) ->
        List.iter ts ~f:(fun t ->
          List.iter fs ~f:(fun f ->
            let init = "" in
            let d = m_fold t ~init ~f:(fun ~key ~data ac -> return (f ~key ~data ac)) in
            stabilize ();
            let o1 = Deferred.peek d in
            let o2 = Some (core_fold t ~init ~f) in
            if not (Option.equal String.equal o1 o2)
            then failwiths ("Deferred.Map."^name^" failed") (t, o1, o2)
                   [%sexp_of: t * string option * string option])))
    ;;

    let%test_unit _ =
      List.iter
        [ (fun ~key:_ _ -> None);
          (fun ~key:_ _ -> Some "z");
          (fun ~key:_ -> function `Left _ -> None | _ -> Some "z");
          (fun ~key:_ -> function `Right _ -> None | _ -> Some "z");
          (fun ~key:_ -> function `Both _ -> None | _ -> Some "z");
          (fun ~key:_ -> function
             | `Left v -> Some v
             | `Right v -> Some v
             | `Both (v1, v2) -> Some (v1 ^ v2));
        ]
        ~f:(fun f ->
          List.iter ts ~f:(fun t1 ->
            List.iter ts ~f:(fun t2 ->
              let d = M.merge t1 t2 ~f:(fun ~key z -> return (f ~key z)) in
              stabilize ();
              let o1 = Deferred.peek d in
              let o2 = Some (Core_kernel.Std.Map.merge t1 t2 ~f) in
              if not (Option.equal equal o1 o2) then
                failwiths "Deferred.Map.merge failed" (t1, t2, o1, o2)
                  [%sexp_of: t * t * t option * t option])))
    ;;
  end

  (* [Deferred.{Array,List,Queue}.{init,foldi}] *)
  module F (M : Deferred1.Monad_sequence) = struct
    let%test_unit _ =
      List.iter
        [ []
        ; [ 13 ]
        ; [ 13; 15 ]
        ]
        ~f:(fun l ->
          let finish = Ivar.create () in
          let d =
            M.init (List.length l) ~f:(fun i -> return (List.nth_exn l i))
            >>= fun t ->
            M.foldi t ~init:[] ~f:(fun i ac n ->
              Ivar.read finish >>| fun () -> (i, n) :: ac)
          in
          stabilize ();
          if not (List.is_empty l) then assert (is_none (Deferred.peek d));
          Ivar.fill finish ();
          stabilize ();
          let expected = List.foldi l ~init:[] ~f:(fun i ac n -> (i, n) :: ac) in
          assert (Deferred.peek d = Some expected))
    ;;
  end

  let%test_module _ = (module F (Deferred.Array))
  let%test_module _ = (module F (Deferred.List))
  let%test_module _ = (module F (Deferred.Queue))

end)
