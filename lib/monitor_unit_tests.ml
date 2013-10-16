open Core.Std  let _ = _squelch_unused_module_warning_
open Std
open Monitor

TEST_MODULE = struct

  let stabilize = Scheduler.run_cycles_until_no_jobs_remain

  TEST_UNIT =
    let t1_got_error = ref false in
    let t1 = create () in
    Stream.iter (errors t1) ~f:(fun _ -> t1_got_error := true);
    let d =
      within' ~monitor:t1 (fun () ->
        let t2 = create () in
        let d = error t2 in
        within ~monitor:t2 (fun () -> failwith "");
        d)
    in
    stabilize ();
    assert (Deferred.is_determined d);
    assert !t1_got_error;
  ;;
end
