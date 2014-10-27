open Core.Std
open Import

let debug = Debug.scheduler

let dummy_e = Execution_context.main
let dummy_f : Obj.t -> unit = ignore
let dummy_a : Obj.t = Obj.repr ()

module State = struct
  type t =
    { mutable current_execution_context : Execution_context.t
    ; mutable global_kill_index         : Kill_index.t
    (* The scheduler calls [Jobs.got_uncaught_exn] when an exception bubbles to the top of
       the monitor tree without being handled.  This function guarantees to never run
       another job after this by calling [clear] and because [Jobs.enqueue] will never
       add another job. *)
    ; mutable uncaught_exn              : Error.t option;
    }
  with fields, sexp_of

  let create () =
    { current_execution_context = Execution_context.main
    ; global_kill_index         = Kill_index.initial
    ; uncaught_exn              = None;
    }
  ;;

  let invariant t : unit =
    Invariant.invariant _here_ t <:sexp_of< t >> (fun () ->
      let check f field = f (Field.get field t) in
      Fields.iter
        ~current_execution_context:(check Execution_context.invariant)
        ~global_kill_index:(check (fun kill_index ->
          Kill_index.invariant kill_index;
          assert (not (Kill_index.equal kill_index Kill_index.dead))))
        ~uncaught_exn:ignore)
  ;;

  let set_execution_context t execution_context =
    (* Avoid a caml_modify in most cases. *)
    if not (phys_equal t.current_execution_context execution_context)
    then t.current_execution_context <- execution_context;
  ;;

  let inc_global_kill_index t =
    t.global_kill_index <- Kill_index.next t.global_kill_index;
  ;;
end

module Job_queue : sig
  type t with sexp_of

  include Invariant.S with type t := t

  val create : unit -> t
  val enqueue : t -> Execution_context.t -> ('a -> unit) -> 'a -> unit
  val clear : t -> unit
  val set_jobs_left_this_cycle : t -> int -> unit
  val can_run_a_job : t -> bool
  val length : t -> int
  val run_all
    :  t
    -> State.t
    -> external_actions:(unit -> unit) Thread_safe_queue.t
    -> (unit, exn) Result.t
  val num_jobs_run : t -> int
end = struct
  module A = Core_kernel.Obj_array

  let slots_per_elt = 3

  (* This is essentially a specialized [Flat_queue], done for reasons of speed. *)
  type t =
    { mutable num_jobs_run         : int
    ; mutable jobs_left_this_cycle : int
    (* [jobs] is an array of length [capacity t * slots_per_elt], where each elt has the
       three components of a job ([execution_context], [f], [a]) in consecutive spots in
       [jobs].  [enqueue] doubles the length of [jobs] if [jobs] is full.  [jobs] never
       shrinks. *)
    ; mutable jobs                 : A.t
    (* [mask] is [capacity t - 1], and is used for quickly computing [i mod (capacity
       t)] *)
    ; mutable mask                 : int
    (* [front] is the index of the first job in the queue.  The array index of that job's
       execution context is [front * slots_per_elt]. *)
    ; mutable front                : int
    ; mutable length               : int
    }
  with fields, sexp_of

  let offset t i = ((t.front + i) land t.mask) * slots_per_elt

  let capacity t = t.mask + 1

  let invariant t : unit =
    Invariant.invariant _here_ t <:sexp_of< t >> (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~num_jobs_run:(check (fun num_jobs_run ->
          assert (num_jobs_run >= 0)))
        ~jobs_left_this_cycle:(check (fun jobs_left_this_cycle ->
          assert (jobs_left_this_cycle >= 0)))
        ~jobs:(check (fun jobs ->
          for i = 0 to t.length - 1 do
            Execution_context.invariant (Obj.obj (A.get jobs (offset t i))
                                         : Execution_context.t);
          done))
        ~mask:(check (fun mask ->
          let capacity = mask + 1 in
          assert (Int.is_pow2 capacity);
          assert (capacity * slots_per_elt = A.length t.jobs)))
        ~front:(check (fun front ->
          assert (front >= 0);
          assert (front < capacity t)))
        ~length:(check (fun length ->
          assert (length >= 0);
          assert (length <= capacity t))))
  ;;

  let create_array ~capacity = A.create ~len:(capacity * slots_per_elt)

  let create () =
    let capacity = 1 in
    { num_jobs_run         = 0
    ; jobs_left_this_cycle = 0
    ; jobs                 = create_array ~capacity
    ; mask                 = capacity - 1
    ; front                = 0
    ; length               = 0
    }
  ;;

  let clear t = t.front <- 0; t.length <- 0; t.jobs_left_this_cycle <- 0

  let grow t =
    let old_capacity = capacity t in
    let new_capacity = old_capacity * 2 in
    let old_jobs = t.jobs in
    let old_front = t.front in
    let len1 = (Int.min t.length (old_capacity - old_front)) * slots_per_elt in
    let len2 = t.length * slots_per_elt - len1 in
    let new_jobs = create_array ~capacity:new_capacity in
    A.blit ~len:len1
      ~src:old_jobs ~src_pos:(old_front * slots_per_elt)
      ~dst:new_jobs ~dst_pos:0;
    A.blit ~len:len2
      ~src:old_jobs ~src_pos:0
      ~dst:new_jobs ~dst_pos:len1;
    t.mask <- new_capacity - 1;
    t.jobs <- new_jobs;
    t.front <- 0;
  ;;

  let set (type a) t i execution_context f a =
    let offset = offset t i in
    A.unsafe_set t.jobs  offset      (Obj.repr (execution_context : Execution_context.t));
    A.unsafe_set t.jobs (offset + 1) (Obj.repr (f : a -> unit));
    A.unsafe_set t.jobs (offset + 2) (Obj.repr (a : a));
  ;;

  let enqueue t execution_context f a =
    if t.length = capacity t then grow t;
    set t t.length execution_context f a;
    t.length <- t.length + 1;
  ;;

  let set_jobs_left_this_cycle t n =
    if n < 0
    then failwiths "Jobs.set_jobs_left_this_cycle got negative number" (n, t)
           <:sexp_of< int * t >>;
    t.jobs_left_this_cycle <- n;
  ;;

  let can_run_a_job t = t.length > 0 && t.jobs_left_this_cycle > 0

  let run_external_actions external_actions =
    while Thread_safe_queue.length external_actions > 0 do
      let f = Thread_safe_queue.dequeue_exn external_actions in
      f ()
    done;
  ;;

  let run_all (type a) t (state : State.t) ~external_actions =
    (* [run_external_actions] before entering the loop, since it might enqueue a job,
       changing [t.length]. *)
    run_external_actions external_actions;
    (* We do the [try-with] outside of the [while] because it is cheaper than doing
       a [try-with] for each job. *)
    try
      while can_run_a_job t do
        let this_job = offset t 0 in
        let execution_context =
          (Obj.obj (A.unsafe_get t.jobs this_job) : Execution_context.t)
        in
        let f = (Obj.obj (A.unsafe_get t.jobs (this_job + 1)) : a -> unit) in
        let a = (Obj.obj (A.unsafe_get t.jobs (this_job + 2)) : a        ) in
        (* We clear out the job right now so that it isn't live at the next minor
           collection.  We tried not doing this and saw significant (15% or so)
           performance hits due to spurious promotion. *)
        set t 0 dummy_e dummy_f dummy_a;
        t.front <- (t.front + 1) land t.mask;
        t.length <- t.length - 1;
        t.jobs_left_this_cycle <- t.jobs_left_this_cycle - 1;
        if Execution_context.is_alive execution_context
             ~global_kill_index:state.global_kill_index
        then begin
          t.num_jobs_run <- t.num_jobs_run + 1;
          State.set_execution_context state execution_context;
          (* [f a] may raise, in which case the exn is handled by the outer try-with.  It
             is OK if [f a] raises because the only side effects we have done are to take
             the job out of the queue and decrement [jobs_left_this_cycle].  [f a] may
             side effect [t], either by enqueueing jobs, or by clearing [t]. *)
          f a;
        end;
        (* [run_external_actions] at each iteration for fairness.  This can enqueue
           jobs. *)
        run_external_actions external_actions;
      done;
      Result.ok_unit
    with exn -> Error exn
  ;;
end

module Job = struct

  module Pool = struct

    open Pool

    module Pointer = Pointer
    module Slot = Slot

    let get = get
    let new3 = new3
    let is_full = is_full
    let grow = grow
    let free = free

    let invariant _invariant_a t = invariant ignore t

    type slots = (Execution_context.t, Obj.t -> unit, Obj.t sexp_opaque) Slots.t3
    with sexp_of

    type nonrec t = slots t with sexp_of

    let create () = create Slots.t3 ~capacity:1 ~dummy:(dummy_e, dummy_f, dummy_a)
  end

  type t = Pool.slots Pool.Pointer.t with sexp_of

  let invariant _ = ()

  let create (type a) pool execution_context f a =
    Pool.new3 pool execution_context
      (Obj.magic (f : a -> unit) : Obj.t -> unit)
      (Obj.repr (a : a))
  ;;
end

type t =
  { state            : State.t
  ; mutable job_pool : Job.Pool.t
  ; normal           : Job_queue.t
  ; low              : Job_queue.t
  }
with fields, sexp_of

let length t = Job_queue.length t.normal + Job_queue.length t.low

let is_empty t = length t = 0

let invariant t =
  Invariant.invariant _here_ t <:sexp_of< t >> (fun () ->
    let check invariant field = invariant (Field.get field t) in
    Fields.iter
      ~state:(check (fun state ->
        State.invariant state;
        if is_some state.uncaught_exn then assert (is_empty t)))
      ~job_pool:(check (Job.Pool.invariant ignore))
      ~normal:(check Job_queue.invariant)
      ~low:   (check Job_queue.invariant))
;;

let create () =
  { state    = State.create ()
  ; job_pool = Job.Pool.create ()
  ; normal   = Job_queue.create ()
  ; low      = Job_queue.create ()
  }
;;

let enqueue t (execution_context : Execution_context.t) f a =
  (* If there's been an uncaught exn, we don't add the job, since we don't want
     any jobs to run once there's been an uncaught exn. *)
  if is_none t.state.uncaught_exn then begin
    let job_queue =
      match execution_context.priority with
      | Normal -> t.normal
      | Low    -> t.low
    in
    Job_queue.enqueue job_queue execution_context f a
  end;
;;

let enqueue_job t job ~free_job =
  let job_pool = t.job_pool in
  let module P = Job.Pool in
  enqueue t
    (P.get job_pool job P.Slot.t0)
    (P.get job_pool job P.Slot.t1)
    (P.get job_pool job P.Slot.t2);
  if free_job then P.free t.job_pool job;
;;

let create_job t execution_context f a =
  if Job.Pool.is_full t.job_pool then t.job_pool <- Job.Pool.grow t.job_pool;
  Job.create t.job_pool execution_context f a;
;;

let clear t = List.iter [ t.normal; t.low ] ~f:Job_queue.clear

let start_cycle t ~max_num_jobs_per_priority =
  let n = Max_num_jobs_per_priority_per_cycle.raw max_num_jobs_per_priority in
  Job_queue.set_jobs_left_this_cycle t.normal n;
  Job_queue.set_jobs_left_this_cycle t.low    n;
;;

let force_current_cycle_to_end t = Job_queue.set_jobs_left_this_cycle t.normal 0

let rec run_all t ~external_actions =
  match Job_queue.run_all t.normal t.state ~external_actions with
  | Error _ as e -> e
  | Ok () ->
    match Job_queue.run_all t.low t.state ~external_actions with
    | Error _ as e -> e
    | Ok () ->
      if Job_queue.can_run_a_job t.normal || Job_queue.can_run_a_job t.low
      then run_all t ~external_actions
      else Result.ok_unit
;;

let current_execution_context t = t.state.current_execution_context

let set_execution_context t e = State.set_execution_context t.state e

let uncaught_exn t = t.state.uncaught_exn

let got_uncaught_exn t error =
  if debug then Debug.log "got_uncaught_exn" error <:sexp_of< Error.t >>;
  clear t;
  t.state.uncaught_exn <- Some error;
;;

let global_kill_index t = t.state.global_kill_index

let inc_global_kill_index t = State.inc_global_kill_index t.state

let num_jobs_run t = Job_queue.num_jobs_run t.normal + Job_queue.num_jobs_run t.low
