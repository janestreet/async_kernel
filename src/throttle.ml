open! Core
open! Import
open! Deferred_std
module Deferred = Deferred1

type abort_reason =
  [ `Killed of Source_code_position.t
  | `Other_job_raised of exn
  ]
[@@deriving sexp_of]

type 'a internal_outcome =
  [ `Ok of 'a
  | `Aborted of abort_reason
  | `Raised of exn
  ]
[@@deriving sexp_of]

type 'a outcome =
  [ `Ok of 'a
  | `Aborted
  | `Raised of exn
  ]
[@@deriving sexp_of]

let abort_reason_to_exn (#abort_reason as reason) =
  Error.to_exn
    (match reason with
     | `Killed killed_from ->
       Error.create_s
         [%message
           "throttle aborted job because it was [kill]ed"
             (killed_from : Source_code_position.t)]
     | `Other_job_raised exn ->
       Error.create_s
         [%message
           "throttle aborted job because it was closed by another job raising" (exn : exn)])
;;

let downgrade_outcome (outcome : 'a internal_outcome) : 'a outcome =
  match outcome with
  | `Aborted _ -> `Aborted
  | (`Raised _ | `Ok _) as x -> x
;;

(** [Job_continuation] tells the throttle what should be done when the job is finished.

    This is de-functionalized for two reasons:
    - make it easier to maintain the property that [handle_outcome] doesn't raise
    - make it easier to keep track of what the memory layout of this stuff is *)
module Job_continuation = struct
  type 'b t =
    | Get_outcome_internal of 'b internal_outcome Ivar.t
    | Get_outcome of 'b outcome Ivar.t
    | Get_result_or_raise of Monitor.t * 'b Ivar.t
    | Delay of 'b t
  [@@deriving sexp_of]

  let get_result_or_raise ivar =
    let monitor = Monitor.current () in
    Get_result_or_raise (monitor, ivar)
  ;;

  let rec handle_outcome t (outcome : _ internal_outcome) =
    match t with
    | Get_outcome_internal ivar -> Ivar.fill_exn ivar outcome
    | Get_outcome ivar -> Ivar.fill_exn ivar (downgrade_outcome outcome)
    | Get_result_or_raise (monitor, ivar) ->
      (match outcome with
       | `Ok a -> Ivar.fill_exn ivar a
       | `Aborted (#abort_reason as reason) ->
         Monitor.send_exn monitor (abort_reason_to_exn reason)
       | `Raised exn -> Monitor.send_exn monitor exn)
    | Delay t -> upon (return ()) (fun () -> handle_outcome t outcome)
  ;;
end

module Job : sig
  type ('a, 'b) t = private
    { work : 'a -> 'b Deferred.t
    ; run : [ `Now | `Schedule ]
    ; k : 'b Job_continuation.t
    ; context : Execution_context.t
    }
  [@@deriving sexp_of]

  val create
    :  run:[ `Now | `Schedule ]
    -> ('a -> 'b Deferred.t)
    -> k:'b Job_continuation.t
    -> ('a, 'b) t

  (* Every internal job will eventually be either [run] or [abort]ed, but not both. *)

  val run
    :  ('a, _) t
    -> rest:[ `Log | `Raise | `Call of exn -> unit ]
    -> 'a
    -> [ `Ok | `Raised of exn ] Deferred.t

  (* We take [`Aborted of abort_reason] to avoid needing to reallocate the same value many
      times. *)
  val abort : _ t -> [ `Aborted of abort_reason ] -> unit
end = struct
  type ('a, 'b) t =
    { work : 'a -> 'b Deferred.t
    ; run : [ `Now | `Schedule ]
    ; k : 'b Job_continuation.t
    ; context : Execution_context.t
    }
  [@@deriving sexp_of]

  let create ~run work ~k =
    (* Preserve the execution context so that we don't accidentally inherit the context of
       wherever [Internal_job.run] gets called from. *)
    let context = Scheduler.(current_execution_context (t ())) in
    let t = { work; run; context; k } in
    t
  ;;

  let run =
    let really_run t ~rest a =
      Monitor.try_with ~rest ~run:t.run (fun () -> t.work a)
      |> Eager_deferred0.map ~f:(function
        | Ok b ->
          Job_continuation.handle_outcome t.k (`Ok b);
          `Ok
        | Error exn ->
          let this_outcome = `Raised exn in
          Job_continuation.handle_outcome t.k this_outcome;
          this_outcome)
    in
    fun t ~rest a ->
      Scheduler.with_execution_context (Scheduler.t ()) t.context ~f:(fun () ->
        match t.run with
        | `Now -> really_run t ~rest a
        | `Schedule ->
          (* A previous version of [Internal_job] had a number of extra [Deferred.map]
               invocations. Refactoring it to make those unnecessary causes externally
               observable changes in how async jobs get scheduled, which breaks many tests
               and has the potential to break programs that were unintentionally relying
               on the previous ordering. *)
          let%bind () = return () in
          really_run t ~rest a >>| Fn.id >>| Fn.id)
  ;;

  let abort t reason = Job_continuation.handle_outcome t.k (reason :> _ internal_outcome)
end

module Job_packed = struct
  type 'a t = Job : ('a, 'b) Job.t -> 'a t [@@unboxed]

  let sexp_of_t sexp_of_a (Job job) =
    Job.sexp_of_t sexp_of_a (fun _ -> Sexp.Atom "<opaque>") job
  ;;

  let run (Job job) x = Job.run job x
  let abort (Job job) reason = Job.abort job reason
end

type 'a t =
  { on_error : [ `Continue | `Abort of [ `Raise | `Never_return ] ]
  ; rest : [ `Log | `Raise | `Call of exn -> unit ]
  ; max_concurrent_jobs : int
  ; (* [job_resources_not_in_use] holds resources that are not currently in use by a
       running job. *)
    job_resources_not_in_use : 'a Stack_or_counter.t
  ; (* [jobs_waiting_to_start] is the queue of jobs that haven't yet started. *)
    jobs_waiting_to_start : 'a Job_packed.t Queue.t
  ; (* [0 <= num_jobs_running <= max_concurrent_jobs]. *)
    mutable num_jobs_running : int
  ; (* [capacity_available] is [Some ivar] if user code has called [capacity_available t]
       and is waiting to be notified when capacity is available in the throttle.
       [maybe_start_job] will fill [ivar] when capacity becomes available, i.e. when
       [jobs_waiting_to_start] is empty and [num_jobs_running < max_concurrent_jobs]. *)
    mutable capacity_available : unit Ivar.t option
  ; (* [is_dead] is Some if [t] was killed due to a job raising an exception or [kill t]
       being called. *)
    mutable is_dead : [ `Aborted of abort_reason | `Never_return ] option
  ; (* [cleans] holds functions that will be called to clean each resource when [t] is
       killed. *)
    mutable cleans : ('a -> unit Deferred.t) list
  ; (* [num_resources_not_cleaned] is the number of resources whose clean functions have
       not yet completed.  While [t] is alive, [num_resources_not_cleaned =
       max_concurrent_jobs].  Once [t] is killed, [num_resources_not_cleaned] decreases to
       zero over time as the clean functions complete. *)
    mutable num_resources_not_cleaned : int
  ; (* [cleaned] becomes determined when [num_resources_not_cleaned] reaches zero,
       i.e. after [t] is killed and all its clean functions complete. *)
    cleaned : unit Ivar.t
  }
[@@deriving fields ~getters ~iterators:iter, sexp_of]

let invariant invariant_a t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~on_error:ignore
      ~rest:ignore
      ~max_concurrent_jobs:
        (check (fun max_concurrent_jobs -> assert (max_concurrent_jobs > 0)))
      ~job_resources_not_in_use:
        (check (fun job_resources_not_in_use ->
           Stack_or_counter.iter job_resources_not_in_use ~f:invariant_a;
           assert (
             Stack_or_counter.length job_resources_not_in_use
             =
             if Option.is_some t.is_dead
             then 0
             else t.max_concurrent_jobs - t.num_jobs_running)))
      ~jobs_waiting_to_start:
        (check (function jobs_waiting_to_start ->
           if Option.is_some t.is_dead then assert (Queue.is_empty jobs_waiting_to_start)))
      ~num_jobs_running:
        (check (fun num_jobs_running ->
           assert (num_jobs_running >= 0);
           assert (num_jobs_running <= t.max_concurrent_jobs);
           if num_jobs_running < t.max_concurrent_jobs
           then assert (Queue.is_empty t.jobs_waiting_to_start)))
      ~capacity_available:
        (check (function
          | None -> ()
          | Some ivar -> assert (Ivar.is_empty ivar)))
      ~is_dead:ignore
      ~cleans:ignore
      ~num_resources_not_cleaned:
        (check (fun num_resources_not_cleaned ->
           assert (num_resources_not_cleaned >= 0);
           assert (num_resources_not_cleaned <= t.max_concurrent_jobs);
           if num_resources_not_cleaned < t.max_concurrent_jobs
           then assert (Option.is_some t.is_dead)))
      ~cleaned:
        (check (fun cleaned ->
           if Ivar.is_full cleaned then assert (t.num_resources_not_cleaned = 0)))
  with
  | exn -> raise_s [%message "Throttle.invariant failed" (exn : exn) (t : _ t)]
;;

module T2 = struct
  type nonrec ('a, 'kind) t = 'a t [@@deriving sexp_of]

  let invariant invariant_a _ t = invariant invariant_a t
end

let num_jobs_waiting_to_start t = Queue.length t.jobs_waiting_to_start

let resources_just_cleaned t n =
  t.num_resources_not_cleaned <- t.num_resources_not_cleaned - n;
  assert (t.num_resources_not_cleaned >= 0);
  if t.num_resources_not_cleaned = 0 then Ivar.fill_exn t.cleaned ()
;;

let clean_resource t a =
  Deferred.all_unit (List.map t.cleans ~f:(fun f -> f a))
  >>> fun () -> resources_just_cleaned t 1
;;

let clean_resources_not_in_use t =
  match t.cleans with
  | [] ->
    (* This special case helps a lot if resources are "units", so [job_resources_not_in_use]
       is a potentially very large counter. *)
    resources_just_cleaned t (Stack_or_counter.length t.job_resources_not_in_use);
    Stack_or_counter.clear t.job_resources_not_in_use
  | _ :: _ ->
    Stack_or_counter.iter t.job_resources_not_in_use ~f:(fun a -> clean_resource t a);
    Stack_or_counter.clear t.job_resources_not_in_use
;;

let kill_internal t how =
  if Option.is_none t.is_dead
  then (
    t.is_dead <- Some how;
    (match how with
     | `Never_return -> ()
     | `Aborted _ as abort_reason ->
       Queue.iter t.jobs_waiting_to_start ~f:(fun job ->
         Job_packed.abort job abort_reason));
    Queue.clear t.jobs_waiting_to_start;
    clean_resources_not_in_use t)
;;

let kill ~(here : [%call_pos]) t = kill_internal t (`Aborted (`Killed here))

let at_kill t f =
  (* We preserve the execution context so that exceptions raised by [f] go to the monitor
     in effect when [at_kill] was called. *)
  let f = unstage (Monitor.Exported_for_scheduler.preserve_execution_context' f) in
  t.cleans <- f :: t.cleans
;;

let cleaned t = Ivar.read t.cleaned

let rec start_job t =
  assert (Option.is_none t.is_dead);
  assert (t.num_jobs_running < t.max_concurrent_jobs);
  assert (not (Queue.is_empty t.jobs_waiting_to_start));
  let job = Queue.dequeue_exn t.jobs_waiting_to_start in
  t.num_jobs_running <- t.num_jobs_running + 1;
  let job_resource = Stack_or_counter.pop_exn t.job_resources_not_in_use in
  let run_result = Job_packed.run job ~rest:t.rest job_resource in
  Eager_deferred0.upon run_result (fun res ->
    t.num_jobs_running <- t.num_jobs_running - 1;
    (match res with
     | `Ok -> ()
     | `Raised exn ->
       (match t.on_error with
        | `Continue -> ()
        | `Abort `Never_return -> kill_internal t `Never_return
        | `Abort `Raise -> kill_internal t (`Aborted (`Other_job_raised exn))));
    if Option.is_some t.is_dead
    then clean_resource t job_resource
    else (
      Stack_or_counter.push t.job_resources_not_in_use job_resource;
      if not (Queue.is_empty t.jobs_waiting_to_start)
      then start_job t
      else (
        match t.capacity_available with
        | None -> ()
        | Some ivar ->
          Ivar.fill_exn ivar ();
          t.capacity_available <- None)))
;;

let create_internal ~on_error ~rest job_resources =
  let max_concurrent_jobs = Stack_or_counter.length job_resources in
  { on_error
  ; rest
  ; max_concurrent_jobs
  ; job_resources_not_in_use = job_resources
  ; jobs_waiting_to_start = Queue.create ()
  ; num_jobs_running = 0
  ; capacity_available = None
  ; is_dead = None
  ; cleans = []
  ; num_resources_not_cleaned = max_concurrent_jobs
  ; cleaned = Ivar.create ()
  }
;;

let on_error_old ~continue_on_error =
  if continue_on_error then `Continue else `Abort `Raise
;;

let create_with' ~rest ~continue_on_error job_resources =
  let on_error = on_error_old ~continue_on_error in
  create_internal ~rest ~on_error (Stack_or_counter.of_list job_resources)
;;

let create_with ~continue_on_error job_resources =
  create_with' ~rest:`Log ~continue_on_error job_resources
;;

module Sequencer = struct
  type nonrec 'a t = 'a t [@@deriving sexp_of]

  let create ?(rest = `Log) ?(continue_on_error = false) a =
    create_with' ~rest ~continue_on_error [ a ]
  ;;
end

let create' ~rest ~continue_on_error ~max_concurrent_jobs =
  if max_concurrent_jobs <= 0
  then
    raise_s
      [%message
        "Throttle.create requires positive max_concurrent_jobs, but got"
          (max_concurrent_jobs : int)];
  let on_error = on_error_old ~continue_on_error in
  create_internal
    ~rest
    ~on_error
    (Stack_or_counter.create_counter ~length:max_concurrent_jobs)
;;

let create'' ~rest ~on_error ~max_concurrent_jobs =
  if max_concurrent_jobs <= 0
  then
    raise_s
      [%message
        "Throttle.create requires positive max_concurrent_jobs, but got"
          (max_concurrent_jobs : int)];
  create_internal
    ~rest
    ~on_error
    (Stack_or_counter.create_counter ~length:max_concurrent_jobs)
;;

let create ~continue_on_error ~max_concurrent_jobs =
  create' ~rest:`Log ~continue_on_error ~max_concurrent_jobs
;;

let enqueue_job_or_abort t enqueue job =
  match t.is_dead with
  | Some `Never_return -> ()
  | Some (`Aborted _ as reason) -> Job.abort job reason
  | None ->
    enqueue t.jobs_waiting_to_start (Job_packed.Job job);
    if t.num_jobs_running < t.max_concurrent_jobs then start_job t
;;

let create_and_enqueue_job t enqueue ~run f ~k =
  let job = Job.create ~run f ~k in
  enqueue_job_or_abort t enqueue job
;;

let enqueue_ivar t enqueue ~run f k =
  let ivar = Ivar.create () in
  let k = k ivar in
  create_and_enqueue_job t enqueue ~run f ~k;
  Ivar.read ivar
;;

let handle_enqueue_result result =
  match result with
  | `Ok a -> a
  | `Aborted (#abort_reason as reason) -> raise (abort_reason_to_exn reason)
  | `Raised exn -> raise exn
;;

let enqueue' t f =
  enqueue_ivar t Queue.enqueue ~run:`Schedule f (fun ivar -> Get_outcome ivar)
;;

let enqueue t f =
  enqueue_ivar t Queue.enqueue ~run:`Schedule f (fun ivar ->
    Delay (Job_continuation.get_result_or_raise ivar))
;;

let enqueue_front' t f =
  enqueue_ivar t Queue.enqueue_front ~run:`Schedule f (fun ivar -> Get_outcome ivar)
;;

let enqueue_front t f =
  enqueue_ivar t Queue.enqueue_front ~run:`Schedule f (fun ivar ->
    Delay (Job_continuation.get_result_or_raise ivar))
;;

let enqueue_eager' t f =
  enqueue_ivar t Queue.enqueue ~run:`Now f (fun ivar -> Get_outcome ivar)
;;

let enqueue_eager t f =
  enqueue_ivar t Queue.enqueue ~run:`Now f (fun ivar -> Get_outcome_internal ivar)
  |> Eager_deferred0.map ~f:handle_enqueue_result
;;

let enqueue_exclusive t f =
  let n = t.max_concurrent_jobs in
  if Int.( >= ) n 1_000_000
  then
    raise_s
      [%sexp
        "[enqueue_exclusive] was called with a very large value of \
         [max_concurrent_jobs]. This doesn't work."];
  let done_ = Ivar.create () in
  assert (n > 0);
  let f_placeholder _slot = Ivar.read done_ in
  for _ = 1 to n - 1 do
    don't_wait_for (enqueue t f_placeholder)
  done;
  let%map result =
    enqueue_ivar
      t
      Queue.enqueue
      ~run:`Schedule
      (fun _slot -> f ())
      (fun ivar -> Get_outcome_internal ivar)
  in
  Ivar.fill_exn done_ ();
  handle_enqueue_result result
;;

let monad_sequence_how ~how ~on_error ~f =
  stage
    (match how with
     | `Parallel -> f
     | (`Sequential | `Max_concurrent_jobs _) as how ->
       let max_concurrent_jobs =
         match how with
         | `Sequential -> 1
         | `Max_concurrent_jobs max_concurrent_jobs -> max_concurrent_jobs
       in
       let t = create'' ~rest:`Log ~on_error ~max_concurrent_jobs in
       fun a -> enqueue t (fun () -> f a))
;;

let monad_sequence_how2 ~how ~on_error ~f =
  stage
    (match how with
     | `Parallel -> f
     | (`Sequential | `Max_concurrent_jobs _) as how ->
       let max_concurrent_jobs =
         match how with
         | `Sequential -> 1
         | `Max_concurrent_jobs max_concurrent_jobs -> max_concurrent_jobs
       in
       let t = create'' ~rest:`Log ~on_error ~max_concurrent_jobs in
       fun a1 a2 -> enqueue t (fun () -> f a1 a2))
;;

let prior_jobs_done t =
  (* We queue [t.max_concurrent_jobs] dummy jobs and when they are all started we know
     that all prior jobs finished.  We make sure that all dummy jobs wait for the last one
     to get started before finishing. *)
  Deferred.create (fun all_dummy_jobs_running ->
    let dummy_jobs_running = ref 0 in
    for _ = 1 to t.max_concurrent_jobs do
      don't_wait_for
        (enqueue t (fun _ ->
           incr dummy_jobs_running;
           if !dummy_jobs_running = t.max_concurrent_jobs
           then Ivar.fill_exn all_dummy_jobs_running ();
           Ivar.read all_dummy_jobs_running))
    done)
;;

let capacity_available t =
  if num_jobs_running t < max_concurrent_jobs t
  then return ()
  else (
    match t.capacity_available with
    | Some ivar -> Ivar.read ivar
    | None -> Deferred.create (fun ivar -> t.capacity_available <- Some ivar))
;;

let is_dead t = Option.is_some t.is_dead
