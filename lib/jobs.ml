open Core.Std
open Import

module Q = Dequeue

module Jobs_at_priority : sig
  type t with sexp_of

  include Invariant.S with type t := t

  val create : unit -> t
  val add : t -> Job.t -> unit
  val clear : t -> unit
  val set_jobs_left_this_cycle : t -> int -> unit
  val is_empty : t -> bool
  val can_run_a_job : t -> bool
  val length : t -> int
  val run_all : t -> (Job.t -> unit) -> (unit, exn) Result.t
end = struct
  type t =
    { jobs : Job.t Q.t;
      mutable jobs_left_this_cycle : int;
    }
  with sexp_of

  let invariant t =
    assert (t.jobs_left_this_cycle >= 0);
    Q.iter t.jobs ~f:Job.invariant;
  ;;

  let create () =
    { jobs = Q.create ~never_shrink:true ();
      jobs_left_this_cycle = 0;
    }
  ;;

  let add t job = Q.enqueue_back t.jobs job

  let length t = Q.length t.jobs

  let is_empty t = Q.is_empty t.jobs

  let clear t = Q.clear t.jobs

  let set_jobs_left_this_cycle t n =
    if n < 0 then
      failwiths "Jobs.set_jobs_left_this_cycle got negative number" (n, t)
        (<:sexp_of< int * t >>);
    t.jobs_left_this_cycle <- n;
  ;;

  let can_run_a_job t = Q.length t.jobs > 0 && t.jobs_left_this_cycle > 0

  let run_all t f =
    (* We do the [try-with] outside of the [while] because it is cheaper than doing
       a [try-with] for each job. *)
    try
      while can_run_a_job t do
        let job = Q.dequeue_front_exn t.jobs in
        t.jobs_left_this_cycle <- t.jobs_left_this_cycle - 1;
        (* [f] may raise, but this is OK because the only side effects we have done are to
           take [job] out of the queue and decrement [jobs_left_this_cycle].

           [f] may side effect [t], either by adding new jobs, or by clearing [t]. *)
        f job;
      done;
      Result.ok_unit
    with exn -> Error exn
  ;;
end

type t =
  { normal : Jobs_at_priority.t;
    low    : Jobs_at_priority.t;
  }
with fields, sexp_of

let invariant t : unit =
  let check invariant field = invariant (Field.get field t) in
  Fields.iter
    ~normal:(check Jobs_at_priority.invariant)
    ~low:   (check Jobs_at_priority.invariant)
;;

let create () =
  { normal = Jobs_at_priority.create ();
    low    = Jobs_at_priority.create ();
  }
;;

let length t = Jobs_at_priority.length t.normal + Jobs_at_priority.length t.low

let add t priority job =
  let module P = Priority in
  let jap =
    match priority with
    | P.Normal -> t.normal
    | P.Low -> t.low
  in
  Jobs_at_priority.add jap job
;;

let clear t = List.iter [ t.normal; t.low ] ~f:Jobs_at_priority.clear

let start_cycle t ~max_num_jobs_per_priority =
  let n = Max_num_jobs_per_priority_per_cycle.raw max_num_jobs_per_priority in
  Jobs_at_priority.set_jobs_left_this_cycle t.normal n;
  Jobs_at_priority.set_jobs_left_this_cycle t.low    n;
;;

let force_current_cycle_to_end t = Jobs_at_priority.set_jobs_left_this_cycle t.normal 0

let is_empty t =
  Jobs_at_priority.is_empty t.normal
  && Jobs_at_priority.is_empty t.low
;;

let rec run_all t f =
  match Jobs_at_priority.run_all t.normal f with
  | Error _ as e -> e
  | Ok () ->
    match Jobs_at_priority.run_all t.low f with
    | Error _ as e -> e
    | Ok () ->
      if   Jobs_at_priority.can_run_a_job t.normal
        || Jobs_at_priority.can_run_a_job t.low
      then
        run_all t f
      else
        Result.ok_unit
;;
