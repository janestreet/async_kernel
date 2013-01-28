open Std

TEST_MODULE = struct
  let stabilize () = Scheduler.run_cycles_until_no_jobs_remain ()

  TEST_UNIT =
    let i1 = Ivar.create () in
    let i2 = Ivar.create () in
    let c = choose_ident [ Ivar.read i1; Ivar.read i2 ] in
    stabilize ();
    assert (Deferred.peek c = None);
    Ivar.fill i1 13;
    stabilize ();
    assert (Deferred.peek c = Some 13);
    Ivar.fill i2 14;
    stabilize ();
    assert (Deferred.peek c = Some 13);
  ;;
end
