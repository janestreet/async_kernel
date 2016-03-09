open! Core_kernel.Std
open! Import

module Deferred = Deferred1

include (Deferred_result : Monad.S2
         with type ('a, 'b) t := ('a, 'b) Deferred_result.t
         with module Let_syntax := Deferred_result.Let_syntax)

type 'a t = 'a Or_error.t Deferred.t

include Applicative.Make (struct
    type nonrec 'a t = 'a t
    let return = return
    let apply f x =
      Deferred_result.combine f x
        ~ok:(fun f x -> f x)
        ~err:(fun e1 e2 -> Error.of_list [e1; e2])
    let map = `Custom map
  end)

module Let_syntax = struct
  module Let_syntax = struct
    let return = return
    let map    = map
    let bind   = bind
    let both   = both (* from Applicative.Make *)
    module Open_on_rhs  = struct let return = return end
    module Open_in_body = struct let return = return end
  end
end

let ignore = ignore_m

let fail error = Deferred.return (Result.fail error)

let ok_exn t = Deferred.map t ~f:Or_error.ok_exn

let of_exn exn = Deferred.return (Or_error.of_exn exn)

let of_exn_result t = Deferred.map t ~f:Or_error.of_exn_result

let error msg v sexp_of = Deferred.return (Or_error.error msg v sexp_of)

let error_string msg = Deferred.return (Or_error.error_string msg)

let errorf format = ksprintf error_string format

let tag t message = Deferred.map t ~f:(fun t -> Or_error.tag t message)

let tag_arg t message a sexp_of_a =
  Deferred.map t ~f:(fun t -> Or_error.tag_arg t message a sexp_of_a)
;;

let unimplemented msg = Deferred.return (Or_error.unimplemented msg)

let combine_errors l =
  Deferred.map (Deferred_list.all l) ~f:Or_error.combine_errors
;;

let combine_errors_unit l =
  Deferred.map (Deferred_list.all l) ~f:Or_error.combine_errors_unit
;;

let ok_unit = return ()

let never = Deferred.never

let default_name = "Async.Std.Deferred.Or_error.try_with"

let try_with ?extract_exn ?(name = default_name) f =
  Deferred.map (Monitor.try_with ?extract_exn ~name f) ~f:(function
    | Error exn -> Error (Error.of_exn exn)
    | Ok _ as ok -> ok)
;;

let try_with_join ?extract_exn ?(name = default_name) f =
  Deferred.map (try_with ?extract_exn ~name f) ~f:Or_error.join
;;

module List = struct

  let foldi list ~init:acc ~f =
    let rec loop i acc = function
      | [] -> return acc
      | hd :: tl -> f i acc hd >>= fun acc -> loop (i + 1) acc tl
    in
    loop 0 acc list
  ;;

  let fold t ~init ~f = foldi t ~init ~f:(fun _ a -> f a)

  let seqmap t ~f =
    fold t ~init:[] ~f:(fun bs a -> f a >>| fun b -> b :: bs)
    >>| List.rev
  ;;

  let all list = seqmap list ~f:Fn.id

  let all_unit list = fold list ~init:() ~f:(fun () d -> d)

  let iteri ?(how = `Sequential) t ~f =
    match how with
    | `Parallel | `Max_concurrent_jobs _ as how ->
      all_unit (List.mapi t ~f:(unstage (Throttle.monad_sequence_how2 ~how ~f)))
    | `Sequential ->
      foldi t ~init:() ~f:(fun i () x -> f i x)
  ;;

  let iter ?how t ~f = iteri ?how t ~f:(fun _ a -> f a)

  let map ?(how=`Sequential) t ~f =
    match how with
    | `Parallel | `Max_concurrent_jobs _ as how ->
      all (List.map t ~f:(unstage (Throttle.monad_sequence_how ~how ~f)))
    | `Sequential -> seqmap t ~f
  ;;

  let init ?how n ~f = map ?how (List.init n ~f:Fn.id) ~f

  let filter_map ?how t ~f = map t ?how ~f >>| List.filter_opt

  let concat_map ?how t ~f = map t ?how ~f >>| List.concat

  let filter ?how t ~f =
    filter_map ?how t ~f:(fun x ->
      f x
      >>| fun b ->
      if b then Some x else None)
  ;;

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

let%test_module _ = (module struct

  (* Ounit generates code using [List.rev], but we rebound [List] above, so we need to
     [open Core_kernel.Std] to get [List].  But that shadows a couple other things we need, so we
     bind them first. *)
  module Seqlist = List

  module List = Core_kernel.Std.List

  let tasks = Queue.create ()

  let flush () =
    let rec aux () =
      match Queue.dequeue tasks with
      | None -> ()
      | Some task -> task (); aux ()
    in
    aux ()
  ;;

  let return a =
    let ivar = Ivar.create () in
    let task () = Ivar.fill ivar (Ok a) in
    Queue.enqueue tasks task;
    Ivar.read ivar;
  ;;

  let rec stabilize () =
    flush ();
    Scheduler.run_cycles_until_no_jobs_remain ();
    if not (Queue.is_empty tasks) then stabilize ();
  ;;

  let determined def value =
    match Deferred.peek def with
    | Some (Ok v) -> value = v
    | Some (Error _)
    | None -> false
  ;;

  let%test_unit _ =
    let def = return 123 in
    stabilize ();
    assert (determined def 123)
  ;;

  let%test_unit _ =
    let def = never () in
    stabilize ();
    assert (Deferred.peek def = None)
  ;;

  let%test_unit _ =
    let def =
      Seqlist.fold [ 0 ; 1 ; 2 ] ~init:"" ~f:(fun acc value ->
        return (acc ^ Int.to_string value))
    in
    stabilize ();
    assert (determined def "012")
  ;;

  let%test_unit _ =
    let def = Seqlist.init 3 ~f:(fun value -> return (string_of_int value)) in
    stabilize ();
    assert (determined def [ "0" ; "1" ; "2" ])
  ;;

  let%test_unit _ =
    let r = ref 0 in
    let n = 3 in
    let def =
      Seqlist.iter (List.init ~f:ident n)
        ~f:(fun value -> r := !r + value; return ())
    in
    stabilize ();
    assert (determined def ());
    assert (!r = n * (n - 1) / 2)
  ;;

  let%test_unit _ =
    let def =
      Seqlist.map [ 0 ; 1 ; 2 ]
        ~f:(fun value -> return (succ value))
    in
    stabilize ();
    assert (determined def [ 1 ; 2 ; 3 ])
  ;;

  let%test_unit _ =
    let def =
      Seqlist.filter [ 0 ; 1 ; 2 ; 3 ; 4 ]
        ~f:(fun value -> return (value mod 2 = 0))
    in
    stabilize ();
    assert (determined def [ 0 ; 2 ; 4 ])
  ;;

  let%test_unit _ =
    let def =
      Seqlist.filter_map [ 0 ; 1 ; 2 ; 3 ; 4 ]
        ~f:(fun value ->
          return (
            if value mod 2 = 0 then Some (succ value) else None))
    in
    stabilize ();
    assert (determined def [ 1 ; 3 ; 5 ])
  ;;

  let%test_unit _ =
    let list = List.init 3 ~f:(fun i -> return i) in
    let def = Seqlist.all list in
    stabilize ();
    assert (determined def [ 0 ; 1 ; 2 ])
  ;;

  let%test _ =
    let f _ = Deferred.return (Error (Error.of_string "error")) in
    let def = try_with (fun () -> Seqlist.iter ~f [0]) in
    stabilize ();
    match Deferred.peek def with
    | Some (Ok (Error _)) -> true
    | _ -> false
  ;;

  let%test _ =
    let f _ = raise Not_found in
    let def = try_with (fun () -> Seqlist.iter ~f [0]) in
    stabilize ();
    match Deferred.peek def with
    | Some (Error _) -> true
    | _ -> false
  ;;

  (* tests for non-list functions *)

  let err = Error.of_string "foo"

  let eq' deferred expected =
    stabilize ();
    Option.value_map (Deferred.peek deferred) ~default:false ~f:(fun got -> got = expected)
  ;;

  let eq deferred expected =
    stabilize ();
    match Deferred.peek deferred, expected with
    | Some (Error err), Error expected ->
      let expected, got = Error.to_string_hum expected, Error.to_string_hum err in
      String.(=) expected got
      || begin
        eprintf "expected %s, got %s\n%!" expected got;
        false
      end
    | Some (Ok x), Ok x' -> x = x'
    | Some (Error _), _ -> true
    | _ -> false
  ;;

  let%test _ =
    eq (fail err) (Error err)
  ;;

  let%test_unit _ =
    assert (eq' (ok_exn (return 1)) 1);
    assert begin
      let rv = Monitor.try_with (fun () -> ok_exn (fail err)) in
      stabilize ();
      match Deferred.peek rv with
      | Some (Error _) -> true
      | _ -> false
    end
  ;;

  let%test _ =
    eq (of_exn (Failure "foo")) (Or_error.of_exn (Failure "foo"))
  ;;

  let%test_unit _ =
    assert (eq (of_exn_result (return 1)) (Ok 1));
    let exn_result = Error (Failure "foo") in
    assert (eq (of_exn_result (Deferred.return exn_result))
              (Or_error.of_exn_result exn_result))
  ;;

  let%test _ =
    eq (error "foo" "bar" String.sexp_of_t) (Or_error.error "foo" "bar" String.sexp_of_t)
  ;;

  let%test _ =
    eq (error_string "foo") (Or_error.error_string "foo")
  ;;

  let%test _ =
    eq (unimplemented "foo") (Or_error.unimplemented "foo")
  ;;

  let check deferred_f immediate_f =
    let check l =
      let deferred_l = List.map l ~f:(function true -> return () | false -> fail err) in
      let immediate_l = List.map l ~f:(function true -> Ok () | false -> Error err) in
      assert (eq (deferred_f deferred_l) (immediate_f immediate_l))
    in
    check [ true; true ];
    check [];
    check [ true; false ];
    check [ true; false; false];
  ;;

  let%test_unit _ =
    check combine_errors Or_error.combine_errors
  ;;

  let%test_unit _ =
    check combine_errors_unit Or_error.combine_errors_unit
  ;;

  let%test _ =
    eq ok_unit (Ok ())
  ;;

  let%test _ =
    let rv = never () in
    stabilize ();
    Option.is_none (Deferred.peek rv)
  ;;

  let expect_failure_with_prefix ~prefix deferred =
    stabilize ();
    match Deferred.peek deferred with
    | Some (Error err) ->
      let s = Error.to_string_hum err in
      if String.is_prefix ~prefix s
      then true
      else begin
        eprintf "expected %s, got %s\n%!" prefix s;
        false
      end
    | _ -> false
  ;;

  let%test_unit _ =
    assert (eq (try_with (fun () -> Deferred.return 1)) (Ok 1));
    assert (expect_failure_with_prefix (try_with (fun () -> failwith "foo"))
              ~prefix:"\
(monitor.ml.Error_
 ((exn (Failure foo))")
  ;;

  let%test_unit _ =
    assert (eq (try_with_join (fun () -> return 1)) (Ok 1));
    assert (eq (try_with_join (fun () -> fail err)) (Error err));
    assert (expect_failure_with_prefix (try_with (fun () -> failwith "foo"))
              ~prefix:"\
(monitor.ml.Error_
 ((exn (Failure foo))")
  ;;

end)
