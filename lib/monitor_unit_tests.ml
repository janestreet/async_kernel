open Core.Std
open Std
open Monitor

TEST_MODULE = struct

  let stabilize = Scheduler.run_cycles_until_no_jobs_remain

  TEST_UNIT =
    let t1_got_error = ref false in
    let t1 = create () in
    detach_and_iter_errors t1 ~f:(fun _ -> t1_got_error := true);
    let d =
      within' ~monitor:t1 (fun () ->
        let t2 = create () in
        let d = get_next_error t2 in
        within ~monitor:t2 (fun () -> failwith "");
        d)
    in
    stabilize ();
    assert (Deferred.is_determined d);
    assert !t1_got_error;
  ;;

  TEST_UNIT =
    List.iter
      [ `Raise , 0, 0
      ; `Raise , 1, 1
      ; `Raise , 2, 1
      ; `Ignore, 0, 0
      ; `Ignore, 1, 1
      ; `Ignore, 2, 2
      ]
      ~f:(fun (handler, num_exns_to_send, expect_error_count) ->
        try
          let t = create () in
          let error_count = ref 0 in
          let thunk () =
            detach_and_iter_errors t ~f:(fun e ->
              incr error_count;
              match handler with
              | `Raise -> raise e
              | `Ignore -> ());
            for _i = 1 to num_exns_to_send do
              send_exn t Not_found
            done;
            Deferred.unit
          in
          don't_wait_for (handle_errors thunk ignore);
          stabilize ();
          <:test_result< int >> (!error_count) ~expect:expect_error_count;
        with exn ->
          failwiths "failure" (exn, handler, `num_exns_to_send num_exns_to_send)
            <:sexp_of< exn * [ `Raise | `Ignore ] * [ `num_exns_to_send of int ] >>)
  ;;
end
