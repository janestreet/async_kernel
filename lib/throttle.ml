open Core.Std
open Deferred_std

type 'a outcome = [ `Ok of 'a | `Aborted | `Raised of exn ]

module Internal_job : sig
  type 'a t with sexp_of

  val create : ('a -> 'b Deferred.t) -> 'a t * 'b outcome Deferred.t

  (* Every internal job will eventually be either [run] or [abort]ed, but not both. *)
  val run : 'a t -> 'a -> [ `Ok | `Raised ] Deferred.t
  val abort : _ t -> unit
end  = struct
  type 'a t =
    { start : [ `Abort | `Start of 'a ] Ivar.t;
      outcome : [ `Ok | `Aborted | `Raised ] Deferred.t;
    }
  with sexp_of

  let create work =
    let start = Ivar.create () in
    let result =
      Ivar.read start
      >>= function
      | `Abort -> return `Aborted
      | `Start a ->
        Monitor.try_with (fun () -> work a)
        >>| function
        | Ok a -> `Ok a
        | Error exn -> `Raised exn
    in
    let outcome =
      result
      >>| function
      | `Ok _ -> `Ok
      | `Aborted -> `Aborted
      | `Raised _ -> `Raised
    in
    let t = { start; outcome } in
    (t, result)
  ;;

  let run t a =
    Ivar.fill t.start (`Start a);
    t.outcome
    >>| function
    | `Aborted -> assert false
    | `Ok | `Raised as x -> x
  ;;

  let abort t = Ivar.fill t.start `Abort
end

type 'a t =
  { continue_on_error : bool;
    max_concurrent_jobs : int;
    (* [job_resources] holds resources that are not currently in use by a running
       job. *)
    job_resources : 'a Stack.t;
    (* [jobs_waiting_to_start] is the queue of jobs that haven't yet started. *)
    jobs_waiting_to_start : 'a Internal_job.t Queue.t;
    (* [0 <= num_jobs_running <= max_concurrent_jobs]. *)
    mutable num_jobs_running : int;
    (* [capacity_available] is [Some ivar] if user code has called [capacity_available
       t] and is waiting to be notified when capacity is available in the throttle.
       [maybe_start_job] will fill [ivar] when capacity becomes available, i.e. when
       [jobs_waiting_to_start] is empty and [num_jobs_running < max_concurrent_jobs]. *)
    mutable capacity_available : unit Ivar.t option;
    mutable is_dead : bool;
  }
with fields, sexp_of

type ('a, 'kind) t_ = 'a t with sexp_of

let invariant invariant_a t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~continue_on_error:ignore
      ~max_concurrent_jobs:(check (fun max_concurrent_jobs ->
        assert (max_concurrent_jobs > 0)))
      ~job_resources:(check (fun job_resources ->
        Stack.iter job_resources ~f:invariant_a;
        assert (Stack.length job_resources = t.max_concurrent_jobs - t.num_jobs_running)))
      ~jobs_waiting_to_start:ignore
      ~num_jobs_running:(check (fun num_jobs_running ->
        assert (num_jobs_running >= 0);
        assert (num_jobs_running <= t.max_concurrent_jobs);
        if num_jobs_running < t.max_concurrent_jobs then
          assert (Queue.is_empty t.jobs_waiting_to_start)))
      ~capacity_available:(check (function
        | None -> ()
        | Some ivar -> assert (Ivar.is_empty ivar)))
      ~is_dead:(check (function is_dead ->
        if is_dead then assert (Queue.is_empty t.jobs_waiting_to_start)))
  with exn ->
    failwiths "Throttle.invariant failed" (exn, t) <:sexp_of< exn * _ t >>
;;

let num_jobs_waiting_to_start t = Queue.length t.jobs_waiting_to_start

let kill t =
  t.is_dead <- true;
  Queue.iter t.jobs_waiting_to_start ~f:Internal_job.abort;
  Queue.clear t.jobs_waiting_to_start;
;;

let rec start_job t =
  assert (not t.is_dead);
  assert (t.num_jobs_running < t.max_concurrent_jobs);
  assert (not (Queue.is_empty t.jobs_waiting_to_start));
  let job = Queue.dequeue_exn t.jobs_waiting_to_start in
  t.num_jobs_running <- t.num_jobs_running + 1;
  let job_resource = Stack.pop_exn t.job_resources in
  Internal_job.run job job_resource
  >>> fun res ->
  t.num_jobs_running <- t.num_jobs_running - 1;
  Stack.push t.job_resources job_resource;
  begin match res with
  | `Ok -> ()
  | `Raised -> if not t.continue_on_error then kill t
  end;
  if not t.is_dead then begin
    if not (Queue.is_empty t.jobs_waiting_to_start) then
      start_job t
    else
      match t.capacity_available with
      | None -> ()
      | Some ivar -> Ivar.fill ivar (); t.capacity_available <- None;
  end;
;;

let create_with ~continue_on_error job_resources =
  { continue_on_error;
    max_concurrent_jobs = List.length job_resources;
    job_resources = Stack.of_list job_resources;
    jobs_waiting_to_start = Queue.create ();
    num_jobs_running = 0;
    capacity_available = None;
    is_dead = false;
  }
;;

module Sequencer = struct
  type nonrec 'a t = 'a t with sexp_of

  let create ?(continue_on_error = false) a = create_with ~continue_on_error [a]
end

let create ~continue_on_error ~max_concurrent_jobs =
  if max_concurrent_jobs <= 0 then
    failwiths "Throttle.create requires positive max_concurrent_jobs, but got"
      max_concurrent_jobs <:sexp_of< int >>;
  create_with ~continue_on_error (List.init max_concurrent_jobs ~f:ignore)
;;

module Job = struct
  type ('a, 'b) t =
    { internal_job : 'a Internal_job.t;
      result : [ `Ok of 'b | `Aborted | `Raised of exn ] Deferred.t;
    }

  let result t = t.result

  let create f =
    let (internal_job, result) = Internal_job.create f in
    { internal_job; result }
  ;;
end

let enqueue' t f =
  if t.is_dead then failwith "cannot enqueue a job in dead throttle";
  let job = Job.create f in
  Queue.enqueue t.jobs_waiting_to_start job.Job.internal_job;
  if t.num_jobs_running < t.max_concurrent_jobs then start_job t;
  Job.result job;
;;

let enqueue t f =
  enqueue' t f
  >>| function
  | `Ok a -> a
  | `Aborted -> failwith "throttle aborted job"
  | `Raised exn -> raise exn
;;

let prior_jobs_done t =
  (* We queue [t.max_concurrent_jobs] dummy jobs and when they are all started we know
     that all prior jobs finished.  We make sure that all dummy jobs wait for the last one
     to get started before finishing. *)
  Deferred.create (fun all_dummy_jobs_running ->
    let dummy_jobs_running = ref 0 in
    for _i = 1 to t.max_concurrent_jobs do
      don't_wait_for (enqueue t (fun _ ->
        incr dummy_jobs_running;
        if !dummy_jobs_running = t.max_concurrent_jobs then
          Ivar.fill all_dummy_jobs_running ();
        Ivar.read all_dummy_jobs_running))
    done)
;;

let capacity_available t =
  if num_jobs_running t < max_concurrent_jobs t
  then return ()
  else
    match t.capacity_available with
    | Some ivar -> Ivar.read ivar
    | None -> Deferred.create (fun ivar -> t.capacity_available <- Some ivar)
;;

TEST_MODULE = struct
  let stabilize = Scheduler.run_cycles_until_no_jobs_remain

  TEST =
    try
      ignore (create ~continue_on_error:false ~max_concurrent_jobs:0);
      false
    with _ -> true
  ;;

  TEST_UNIT =
    (* Check [~continue_on_error:false]. *)
    let t = create ~continue_on_error:false ~max_concurrent_jobs:1 in
    let d1 = enqueue' t (fun () -> failwith "") in
    let d2 = enqueue' t (fun () -> assert false) in
    stabilize ();
    assert (num_jobs_waiting_to_start t = 0);
    assert (match Deferred.peek d1 with Some (`Raised _) -> true | _ -> false);
    assert (match Deferred.peek d2 with Some `Aborted -> true | _ -> false);
  ;;

  TEST_UNIT =
    (* Check [~continue_on_error:true]. *)
    let t = create ~continue_on_error:true ~max_concurrent_jobs:1 in
    let d1 = enqueue' t (fun () -> failwith "") in
    let d2 = enqueue' t (fun () -> return 13) in
    stabilize ();
    assert (match Deferred.peek d1 with Some (`Raised _) -> true | _ -> false);
    assert (match Deferred.peek d2 with Some (`Ok 13) -> true | _ -> false);
  ;;

  TEST_UNIT =
    (* Check that jobs are started in the order they are enqueued. *)
    let t = create ~continue_on_error:false ~max_concurrent_jobs:2 in
    let r = ref [] in
    for i = 0 to 99 do
      don't_wait_for (enqueue t (fun () -> r := i :: !r; Deferred.unit));
    done;
    stabilize ();
    assert (!r = List.rev (List.init 100 ~f:Fn.id));
  ;;

  (* [num_jobs_waiting_to_start], [num_jobs_running] *)
  TEST_UNIT =
    let t = create ~continue_on_error:false ~max_concurrent_jobs:2 in
    assert (num_jobs_waiting_to_start t = 0);
    let add_job () =
      let ivar = Ivar.create () in
      don't_wait_for (enqueue t (fun () -> Ivar.read ivar));
      ivar;
    in
    let i1 = add_job () in
    assert (num_jobs_waiting_to_start t + num_jobs_running t = 1);
    stabilize ();
    assert (num_jobs_waiting_to_start t = 0);
    assert (num_jobs_running t = 1);
    let _i2 = add_job () in
    assert (num_jobs_waiting_to_start t + num_jobs_running t = 2);
    stabilize ();
    assert (num_jobs_waiting_to_start t = 0);
    assert (num_jobs_running t = 2);
    let _i3 = add_job () in
    assert (num_jobs_waiting_to_start t = 1);
    assert (num_jobs_running t = 2);
    stabilize ();
    assert (num_jobs_waiting_to_start t = 1);
    assert (num_jobs_running t = 2);
    Ivar.fill i1 ();
    stabilize ();
    assert (num_jobs_waiting_to_start t = 0);
    assert (num_jobs_running t = 2);
  ;;

  TEST_UNIT =
    (* Check [capacity_available] *)
    let t = create ~continue_on_error:false ~max_concurrent_jobs:2 in
    let r = capacity_available t in
    stabilize ();
    assert (Option.is_some (Deferred.peek r));
    let i1 = Ivar.create () in
    don't_wait_for (enqueue t (fun () -> Ivar.read i1));
    let r = capacity_available t in
    stabilize ();
    assert (Option.is_some (Deferred.peek r));
    let i2 = Ivar.create () in
    don't_wait_for (enqueue t (fun () -> Ivar.read i2));
    let r = capacity_available t in
    stabilize ();
    assert (Option.is_none (Deferred.peek r));
    Ivar.fill i1 ();
    stabilize ();
    assert (Option.is_some (Deferred.peek r));
  ;;

  TEST_UNIT =
    (* Check that [max_concurrent_jobs] limit works, and is fully utilized. *)
    List.iter [ 1; 10; 100; 1000 ] ~f:(fun num_jobs ->
      List.iter [ 1; 10; 100; 1000 ] ~f:(fun max_concurrent_jobs ->
        let resources = List.init max_concurrent_jobs ~f:(fun _ -> ref false) in
        let max_observed_concurrent_jobs = ref 0 in
        let num_concurrent_jobs = ref 0 in
        let job_starts = ref [] in
        let t = create_with ~continue_on_error:false resources in
        let d = prior_jobs_done t in
        stabilize ();
        assert (Deferred.is_determined d);
        let continue = Ivar.create () in
        let jobs = ref [] in
        for i = 0 to num_jobs - 1; do
          let job =
            enqueue t (fun r ->
              assert (not !r); (* ensure no one else is accessing the resource *)
              r := true;
              job_starts := i :: !job_starts;
              incr num_concurrent_jobs;
              max_observed_concurrent_jobs :=
                max !max_observed_concurrent_jobs !num_concurrent_jobs;
              assert (!num_concurrent_jobs <= max_concurrent_jobs);
              Ivar.read continue
              >>| fun () ->
              decr num_concurrent_jobs;
              r := false)
          in
          jobs := job :: !jobs
        done;
        let all_done = prior_jobs_done t in
        let jobs = !jobs in
        let jobs_finished = Deferred.all_unit jobs in
        stabilize ();
        assert (not (Deferred.is_determined all_done));
        let num_initial_jobs = min num_jobs max_concurrent_jobs in
        assert (!num_concurrent_jobs = num_initial_jobs);
        assert (List.length !job_starts = num_initial_jobs);
        if max_concurrent_jobs = 1 then
          assert (!job_starts = List.init num_initial_jobs ~f:Fn.id);
        Ivar.fill continue ();
        stabilize ();
        assert (Deferred.is_determined all_done);
        assert (!max_observed_concurrent_jobs = min num_jobs max_concurrent_jobs);
        assert (Deferred.is_determined jobs_finished);
        if max_concurrent_jobs = 1 then
          assert (List.rev !job_starts = List.init num_jobs ~f:Fn.id);
      ))
  ;;

end
