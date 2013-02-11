open Core.Std

module Q = Dequeue

module Priority = struct
  type t = Normal | Low with sexp_of

  let normal = Normal
  let low = Low

  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Jobs_at_priority : sig
  type 'job t with sexp_of

  val invariant : _ t -> unit

  val create : dummy:'a -> 'a t
  val add : 'job t -> 'job -> unit
  val clear : _ t -> unit
  val set_jobs_left_this_cycle : _ t -> int -> unit
  val is_empty : _ t -> bool
  val can_run_a_job : _ t -> bool
  val length : _ t -> int
  val run_all : 'job t -> ('job -> unit) -> (unit, exn) Result.t
end = struct
  type 'job t =
    { jobs : 'job Q.t;
      mutable jobs_left_this_cycle : int;
    }
  with sexp_of

  let invariant t =
    assert (t.jobs_left_this_cycle >= 0);
  ;;

  let create ~dummy =
    { jobs = Q.create ~dummy ~never_shrink:true ();
      jobs_left_this_cycle = 0;
    }
  ;;

  let add t job = Q.push_back t.jobs job

  let length t = Q.length t.jobs

  let is_empty t = Q.is_empty t.jobs

  let clear t = Q.clear t.jobs

  let set_jobs_left_this_cycle t n =
    if t.jobs_left_this_cycle < 0 then
      failwiths "Jobs.set_jobs_left_this_cycle got negative number" (n, t)
        (<:sexp_of< int * _ t >>);
    t.jobs_left_this_cycle <- n
  ;;

  let can_run_a_job t = Q.length t.jobs > 0 && t.jobs_left_this_cycle > 0

  let run_all t f =
    (* We do the [try-with] outside of the [while] because it is cheaper than doing
       a [try-with] for each job. *)
    try
      while can_run_a_job t do
        let job = Q.take_front_exn t.jobs in
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

type 'job t =
  { normal : 'job Jobs_at_priority.t;
    low : 'job Jobs_at_priority.t;
  }
with fields, sexp_of

let invariant t : unit =
  let check invariant field = invariant (Field.get field t) in
  Fields.iter
    ~normal:(check Jobs_at_priority.invariant)
    ~low:   (check Jobs_at_priority.invariant)
;;

let create ~dummy =
  { normal = Jobs_at_priority.create ~dummy;
    low    = Jobs_at_priority.create ~dummy;
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
  let doit jobs_at_priority =
    Jobs_at_priority.set_jobs_left_this_cycle jobs_at_priority max_num_jobs_per_priority
  in
  doit t.normal;
  doit t.low;
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
