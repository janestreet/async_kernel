open Core_kernel.Std
open Std
open Monitor

let%test_module _ = (module struct

  let stabilize = Scheduler.run_cycles_until_no_jobs_remain

  let%test_unit _ =
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
    assert !t1_got_error
  ;;

  let%test_unit _ =
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
            for _ = 1 to num_exns_to_send do
              send_exn t Not_found
            done;
            Deferred.unit
          in
          don't_wait_for (handle_errors thunk ignore);
          stabilize ();
          [%test_result: int] (!error_count) ~expect:expect_error_count;
        with exn ->
          failwiths "failure" (exn, handler, `num_exns_to_send num_exns_to_send)
            [%sexp_of: exn * [ `Raise | `Ignore ] * [ `num_exns_to_send of int ]])
  ;;

  (* Test the lifetime of Async monitors:
     (1) [try_with] does not hold on to its enclosing monitor
     (2) Monitors are freed when the last job refering to them has been freed. *)
  let%test_unit _ =
    let outer_monitor_finalized = ref false in
    let inner_monitor_finalized = ref false in
    let inner_job_ran           = ref false in
    let outer_monitor = Monitor.create () in
    Gc.add_finalizer_exn outer_monitor (fun _ -> outer_monitor_finalized := true);
    let delayed_run = Ivar.create () in
    let d =
      within' ~monitor:outer_monitor (fun () ->
        Monitor.try_with (fun () ->
          let inner_monitor = Monitor.current () in
          within ~monitor:Monitor.main (fun () ->
            (* An Async job refers to the current monitor.  So we replace the current
               monitor with main so that inner_monitor can be GCed once the [upon] below
               has completed.  We attach a finalizer to the inner monitor so that we know
               once it has been collected. *)
            Gc.add_finalizer_exn inner_monitor
              (fun _ -> inner_monitor_finalized := true));
          (* We use this [upon] to force Async to keep [inner_monitor].  Once completed,
             [inner_monitor] should be not longer be live. *)
          upon (Ivar.read delayed_run) (fun () -> inner_job_ran := true);
          return ()))
    in
    Scheduler.run_cycles_until_no_jobs_remain ();
    assert (is_some (Deferred.peek d));
    Gc.full_major ();
    (* [outer_monitor] has been freed, but its Async finalizer job has not run. *)
    assert (not !outer_monitor_finalized);
    Scheduler.run_cycles_until_no_jobs_remain ();
    (* We check to see that [outer_monitor] is free to be collected even though
       [inner_monitor] is alive.  This shows that monitors are not chained. *)
    assert !outer_monitor_finalized;
    assert (not !inner_monitor_finalized);
    (* Now we fill [delayed_run], thus enabling the the [upon] to run.  After the [upon]
       has completed there should be no more references to [inner_monitor]. *)
    Ivar.fill delayed_run ();
    Scheduler.run_cycles_until_no_jobs_remain ();
    (* Check that [upon] ran. *)
    assert !inner_job_ran;
    Gc.full_major ();
    assert (not !inner_monitor_finalized);
    Scheduler.run_cycles_until_no_jobs_remain ();
    (* check that the inner monitor has been collected. *)
    assert !inner_monitor_finalized
  ;;
end)
