open Core.Std

include (Deferred.Result : Monad.S2 with type ('a, 'b) t := ('a, 'b) Deferred.Result.t)

type 'a t = 'a Or_error.t Deferred.t

let fail error = Deferred.return (Result.fail error)

let of_exn exn = fail (Error.of_exn exn)

let failwith s = of_exn (Failure s)

let ok_unit = return ()

let never = Deferred.never

let default_name = "Async.Std.Deferred.Or_error.try_with"

let try_with ?(name = default_name) ~f arg =
  Deferred.map (Monitor.try_with ~name (fun () -> f arg)) ~f:(function
  | Error exn -> Error (Error.of_exn exn)
  | Ok _ as ok -> ok)
;;

let try_with_join ?(name = default_name) ~f arg =
  Deferred.map (try_with ~name ~f arg) ~f:Or_error.join
;;

module List = struct
  let fold list ~init:acc ~f =
    let rec loop acc = function
      | [] -> return acc
      | hd :: tl -> f acc hd >>= fun acc -> loop acc tl
    in
    loop acc list
  ;;

  let seqmap t ~f =
    fold t ~init:[] ~f:(fun bs a -> f a >>| fun b -> b :: bs)
    >>| List.rev
  ;;

  let all list = seqmap list ~f:Fn.id

  let all_unit list = fold list ~init:() ~f:(fun () d -> d)

  let iter ?(how=`Sequential) t ~f =
    match how with
    | `Parallel -> all_unit (List.map t ~f)
    | `Sequential -> fold t ~init:() ~f:(fun () a -> f a)
  ;;

  let map ?(how=`Sequential) t ~f =
    match how with
    | `Parallel -> all (List.map t ~f)
    | `Sequential -> seqmap t ~f
  ;;

  let init ?how n ~f = map ?how (List.init n ~f:Fn.id) ~f

  let filter_map ?how t ~f = map t ?how ~f >>| List.filter_opt

  let filter ?how t ~f =
    filter_map ?how t ~f:(fun x ->
      f x
      >>| fun b ->
      if b then Some x else None)
  ;;
end

(* ounit generates code using [List.rev] *)
module Seqlist = List
open Core.Std

TEST_MODULE = struct

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

  TEST_UNIT =
    let def = return 123 in
    stabilize ();
    assert(determined def 123);
  ;;

  TEST_UNIT =
    let def = never () in
    stabilize ();
    assert(Deferred.peek def = None);
  ;;

  TEST_UNIT =
    let def = Seqlist.fold [ 0 ; 1 ; 2 ] ~init:"" ~f:(fun acc value ->
      return (acc^(string_of_int value))) in
    stabilize ();
    assert(determined def "012");
  ;;

  TEST_UNIT =
    let def = Seqlist.init 3 ~f:(fun value -> return (string_of_int value)) in
    stabilize ();
    assert(determined def [ "0" ; "1" ; "2" ]);
  ;;

  TEST_UNIT =
    let r = ref 0 in
    let n = 3 in
    let def = Seqlist.iter (List.init ~f:ident n)
      ~f:(fun value -> r := !r + value; return ()) in
    stabilize ();
    assert(determined def ());
    assert(!r = n * (n - 1) / 2);
  ;;

  TEST_UNIT =
    let def = Seqlist.map [ 0 ; 1 ; 2 ]
      ~f:(fun value -> return (succ value))
    in
    stabilize ();
    assert(determined def [ 1 ; 2 ; 3 ]);
  ;;

  TEST_UNIT =
    let def = Seqlist.filter [ 0 ; 1 ; 2 ; 3 ; 4 ]
      ~f:(fun value -> return (value mod 2 = 0))
    in
    stabilize ();
    assert(determined def [ 0 ; 2 ; 4 ]);
  ;;

  TEST_UNIT =
    let def = Seqlist.filter_map [ 0 ; 1 ; 2 ; 3 ; 4 ]
      ~f:(fun value ->
        return (
          if value mod 2 = 0 then Some (succ value)
          else None))
    in
    stabilize ();
    assert(determined def [ 1 ; 3 ; 5 ]);
  ;;

  TEST_UNIT =
    let list = List.init 3 ~f:(fun i -> return i) in
    let def = Seqlist.all list in
    stabilize ();
    assert(determined def [ 0 ; 1 ; 2 ]);
  ;;
end
