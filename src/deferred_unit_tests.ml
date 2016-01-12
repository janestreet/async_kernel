open Core_kernel.Std
open Std

let%test_module _ = (module struct

  open Deferred

  let test f =
    let t = f () in
    Scheduler.run_cycles_until_no_jobs_remain ();
    if not (Deferred.is_determined t) then failwith "unit test didn't finish";
  ;;

  let%test_unit _ = (* [enabled] returns choices in order *)
    test (fun () ->
      enabled [ choice (return 13) Fn.id
              ; choice (return 14) Fn.id
              ]
      >>| fun f ->
      match f () with
      | [ 13; 14 ] -> ()
      | _ -> assert false)
  ;;

  let%test_unit _ =
    (* [choose] returns the first (in the list) choice that was set with no intervening
       asynchrony. *)
    test (fun () ->
      let t1 = Ivar.create () in
      let t2 = Ivar.create () in
      let t =
        choose [ choice (Ivar.read t1) Fn.id
               ; choice (Ivar.read t2) Fn.id
               ]
        >>| fun i ->
        [%test_result: int] i ~expect:13
      in
      Ivar.fill t2 14;
      Ivar.fill t1 13;
      t)
  ;;
end)
