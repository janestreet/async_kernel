open Core.Std
open Deferred_std

module T = struct

  type 'a t =
    { start  : unit Ivar.t
    ; result : ('a, exn) Result.t Deferred.t
    }

  let create f =
    let start = Ivar.create () in
    { start
    ; result = Ivar.read start >>= (fun () -> Monitor.try_with f);
    }
  ;;

  let wait t = t.result

  let wait_exn t = wait t >>| Result.ok_exn

  let start t = Ivar.fill_if_empty t.start ()

  let force t = start t; wait t

  let force_exn t = force t >>| Result.ok_exn

  let return a = create (fun () -> return a)

  let bind t f = create (fun () -> force_exn t >>= fun a -> force_exn (f a))

  let map t ~f = create (fun () -> force_exn t >>| f)
  let map = `Custom map

end

include T

include Monad.Make (T)

let bind' t f = bind t (fun a -> create (fun () -> f a))

let follow t f =
  let ret = bind' t f in
  upon (wait t) (fun _ -> start ret);
  ret
;;

let is_forced t = Ivar.is_full t.start

let is_determined t = Deferred.is_determined t.result

let peek t = Deferred.peek t.result

let peek_exn t = Option.map (peek t) ~f:Result.ok_exn

TEST_MODULE = struct

  let stabilize = Scheduler.run_cycles_until_no_jobs_remain

  TEST_UNIT =
    let def = create Deferred.return in
    stabilize();
    let consumer = wait def in
    stabilize();
    assert (not (Deferred.is_determined consumer));
    assert (not (is_forced def));
    assert (not (is_determined def));
  ;;

  TEST_UNIT =
    let def = create Deferred.return in
    stabilize();
    let opt = peek def in
    stabilize();
    assert (opt = None);
    assert (not (is_forced def));
    assert (not (is_determined def));
  ;;

  TEST_UNIT =
    let def = create Deferred.return in
    stabilize();
    let consumer = force def in
    stabilize();
    assert (Deferred.is_determined consumer);
    assert (is_determined def);
    assert (is_forced def);
  ;;

  TEST_UNIT =
    let def = create Deferred.return in
    stabilize();
    let consumer = wait def in
    stabilize();
    assert (not (Deferred.is_determined consumer));
    assert (peek def = None);
    assert (not (is_forced def));
    let consumer2 = force def in
    stabilize();
    assert (peek def <> None);
    assert (Deferred.is_determined consumer);
    assert (Deferred.is_determined consumer2);
    assert (is_forced def);
  ;;

  let determined def value =
    match Deferred.peek def with
    | Some v -> value = v
    | None -> false
  ;;

  let make_bind_test ?(follow=false) make final =
    let def1 = create Deferred.return in
    let def2 = make def1 in
    stabilize();
    assert (not (is_forced def1));
    assert (not (is_forced def2));
    let consumer1 = wait_exn def1 in
    let consumer2 = wait_exn def2 in
    stabilize();
    assert (not (Deferred.is_determined consumer1));
    assert (not (Deferred.is_determined consumer2));
    let force1 = force_exn def1 in
    stabilize();
    assert (determined consumer1 ());
    assert (determined force1 ());
    if follow
    then assert (determined consumer2 final)
    else assert (not (Deferred.is_determined consumer2))
    ;
    let force2 = force_exn def2 in
    stabilize();
    assert (determined force2 final);
  ;;

  (* bind' *)
  TEST_UNIT =
    let final = "foo" in
    let make def1 =
      bind' def1 (fun () -> Deferred.return final)
    in
    make_bind_test make final;
  ;;

  TEST_UNIT =
    let final = "foo" in
    let make def1 =
      bind def1 (fun () -> create (fun () -> Deferred.return "foo"))
    in
    make_bind_test make final;
  ;;

  TEST_UNIT =
    let final = "foo" in
    let make def1 = follow def1 (fun () -> Deferred.return "foo") in
    make_bind_test ~follow:true make final;
  ;;

  let determined def value =
    match Deferred.peek def with
    | Some (Ok _ as v) -> value = v
    | Some (Error exn) -> value = (Error (Monitor.extract_exn exn))
    | None -> false
  ;;

  exception E_for_test

  TEST_UNIT =
    let def = create (fun _ -> raise E_for_test) in
    stabilize();
    assert (not (is_determined def));
    let def = force def in
    stabilize();
    assert (determined def (Error E_for_test));
  ;;

  TEST_UNIT =
    let def = create (fun () -> Deferred.return "foo") in
    let def = bind def (fun _ -> raise E_for_test) in
    stabilize();
    assert (not (is_determined def));
    let def = force def in
    stabilize();
    assert (determined def (Error E_for_test));
  ;;

  TEST_UNIT =
    let def = create (fun _ -> raise E_for_test) in
    stabilize();
    assert (not (is_determined def));
    let def = Monitor.try_with (fun () -> force_exn def) in
    stabilize();
    assert (determined def (Error E_for_test));
  ;;

  TEST_UNIT =
    let def = create (fun () -> Deferred.return "foo") in
    let def = bind def (fun _ -> raise E_for_test) in
    stabilize();
    assert (not (is_determined def));
    let def = Monitor.try_with (fun () -> force_exn def) in
    stabilize();
    assert (determined def (Error E_for_test));
  ;;

end
