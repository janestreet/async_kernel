open Core.Std

include Gc

(** [add_finalizer f x] is like [Gc.finalise f x], except that the finalizer is guaranteed
    to run as an Async job (i.e. without interrupting other Async jobs).  Unprotected use
    of [Caml.Gc.finalise] or [Core.Gc.add_finalizer] in Async programs is wrong, because
    the finalizers won't hold the async lock, and thus could interleave arbitrarily with
    async jobs. *)
let add_finalizer     heap_block f = Scheduler.(add_finalizer     (t ())) heap_block f
let add_finalizer_exn heap_block f = Scheduler.(add_finalizer_exn (t ())) heap_block f

TEST_MODULE = struct

  let stabilize () =
    Gc.full_major ();
    Scheduler.run_cycles_until_no_jobs_remain ();
  ;;

  TEST_UNIT =
    let x = ref 0 in
    let r = ref 13 in
    add_finalizer_exn x (fun z -> r := !z);
    stabilize ();
    assert (!r = 13);
    x := 17;
    stabilize ();
    assert (!r = 17);
  ;;

end
