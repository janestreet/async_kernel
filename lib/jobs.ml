open Core.Std

module Priority = struct
  type t = Normal | Low with sexp_of

  let normal = Normal
  let low = Low

  let to_string t = Sexp.to_string (sexp_of_t t)
end

module Jobs_at_priority : sig
  type 'job t with sexp_of

  val create : unit -> _ t
  val add : 'job t -> 'job -> unit
  val start_cycle : _ t -> max_num_jobs:int -> unit
  val is_empty : _ t -> bool
  val get : 'job t -> 'job list
  val length : _ t -> int
end = struct
  type 'job t =
    { jobs : 'job Queue.t;
      mutable jobs_left_this_cycle : int;
    }
  with sexp_of

  let invariant t =
    assert (t.jobs_left_this_cycle >= 0);
  ;;

  let create () =
    { jobs = Queue.create ();
      jobs_left_this_cycle = 0;
    }
  ;;

  let add t job = Queue.enqueue t.jobs job

  let length t = Queue.length t.jobs

  let is_empty t = Queue.is_empty t.jobs

  let start_cycle t ~max_num_jobs = t.jobs_left_this_cycle <- max_num_jobs

  let get t =
    let rec loop ac =
      if t.jobs_left_this_cycle = 0 || Queue.is_empty t.jobs then
        ac
      else begin
        t.jobs_left_this_cycle <- t.jobs_left_this_cycle - 1;
        loop (Queue.dequeue_exn t.jobs :: ac);
      end;
    in
    loop []
  ;;
end

type 'job t =
  { normal : 'job Jobs_at_priority.t;
    low : 'job Jobs_at_priority.t;
  }
with sexp_of

let create () =
  { normal = Jobs_at_priority.create ();
    low = Jobs_at_priority.create ();
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

let start_cycle t ~max_num_jobs_per_priority =
  let doit jobs_at_priority =
    Jobs_at_priority.start_cycle jobs_at_priority ~max_num_jobs:max_num_jobs_per_priority
  in
  doit t.normal;
  doit t.low;
;;

let is_empty t =
  Jobs_at_priority.is_empty t.normal
  && Jobs_at_priority.is_empty t.low
;;

let get t =
  match Jobs_at_priority.get t.normal with
  | [] -> Jobs_at_priority.get t.low
  | jobs -> jobs
;;
