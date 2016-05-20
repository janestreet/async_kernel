open! Core_kernel.Std
open! Import
open! Deferred_std

module Deferred = Deferred1

module Make
    (M : sig type 'a t [@@deriving compare, sexp_of] end)
    (S : Deferred.Monad_sequence with type 'a t := 'a M.t)
  : sig
  end = struct

  let deferred_result (d : unit Deferred.t) =
    let deferred_result = ref None in
    upon d (fun v -> deferred_result := Some v);
    Scheduler.run_cycles_until_no_jobs_remain ();
    assert (is_some !deferred_result);
  ;;

  let%test_unit _ =
    for max_concurrent_jobs = 1 to 10 do
      for length = 0 to 10 do
        deferred_result (
          let parallel_cur = ref 0 in
          let parallel_max = ref 0 in
          let%bind numbers = S.init length ~f:return in
          let%map res =
            S.map numbers
              ~how:(`Max_concurrent_jobs max_concurrent_jobs)
              ~f:(fun (x : int) ->
                incr parallel_cur;
                parallel_max := max !parallel_max !parallel_cur;
                let%bind () = Scheduler.yield (Scheduler.t ()) in
                decr parallel_cur;
                return x)
          in
          [%test_result: int M.t] res ~expect:numbers;
          [%test_result: int] !parallel_max ~expect:(min max_concurrent_jobs length))
      done
    done
  ;;
end
