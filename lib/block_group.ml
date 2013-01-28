open Core.Std

(* Each block group reserves a certain number of threads that it may concurrently use.
   Async guarantees the threads will be available by keeping the sum of the
   "num_reserved_threads" over all block groups less than some maximum, which is a small
   constant (like 100) for which we are confident that we can simultaneously have that
   many threads. *)

module Work = struct
  type t =
    { (* When this work starts running, the name of the thread will be set
         (via Linux_ext.pr_set_name) to this name if provided. *)
      set_thread_name_to : string option;
      doit : unit -> [ `Stop | `Continue ];
    }
  with sexp_of
end

type t =
  { mutable num_blocked_threads : int;
    min_reserved_threads : int;
    mutable num_reserved_threads : int;
    max_reserved_threads : int;
    work : Work.t Queue.t;
  }
with sexp_of

let create ~min_reserved_threads ~max_reserved_threads =
  { num_blocked_threads = 0;
    min_reserved_threads;
    num_reserved_threads = min_reserved_threads;
    max_reserved_threads;
    work = Queue.create ();
  }
;;

let bogus = create ~min_reserved_threads:0 ~max_reserved_threads:max_int

let invariant t =
  assert (0 <= t.num_blocked_threads);
  assert (t.num_blocked_threads <= t.num_reserved_threads);
  assert (0 <= t.min_reserved_threads);
  assert (t.min_reserved_threads <= t.num_reserved_threads);
  assert (t.num_reserved_threads <= t.max_reserved_threads);
  assert (Queue.is_empty t.work
           || t.num_blocked_threads = t.num_reserved_threads);
;;
