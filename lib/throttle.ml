open Core.Std
open Deferred_std

type 'a outcome = [ `Ok of 'a | `Aborted | `Raised of exn ]

module Internal_job : sig
  type t with sexp_of

  val create : (unit -> 'a Deferred.t) -> t * 'a outcome Deferred.t

  (* Every internal job will eventually be either [run] or [abort]ed, but not both. *)
  val run : t -> [ `Ok | `Raised ] Deferred.t
  val abort : t -> unit
end  = struct
  type t =
    { start : [ `Abort | `Start ] Ivar.t;
      outcome : [ `Ok | `Aborted | `Raised ] Deferred.t;
    }
  with sexp_of

  let create work =
    let start = Ivar.create () in
    let result =
      Ivar.read start
      >>= function
      | `Abort -> return `Aborted
      | `Start ->
        Monitor.try_with work
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

  let run t =
    Ivar.fill t.start `Start;
    t.outcome
    >>| function
    | `Aborted -> assert false
    | `Ok | `Raised as x -> x
  ;;

  let abort t = Ivar.fill t.start `Abort
end

type t =
  { continue_on_error : bool;
    max_concurrent_jobs : int;
    (* [0 <= num_jobs_running <= max_concurrent_jobs]. *)
    mutable num_jobs_running : int;
    (* [job_reader] and [job_writer] are the two ends of the pipe of jobs that the
       throttle hasn't yet started. *)
    job_reader : Internal_job.t Pipe.Reader.t;
    job_writer : Internal_job.t Pipe.Writer.t;
    (* The throttle has background uthreads running [job_runner], which reads jobs from
       [job_reader] and runs them.  [am_reading_job] reflects whether [job_runner] is
       currently in the middle of a [Pipe.read] call.  This is used to ensure that at most
       one [job_runner] has an active [Pipe.read]. *)
    mutable am_reading_job : bool;
    mutable is_dead : bool;
  }
with fields, sexp_of

let num_jobs_waiting_to_start t = Pipe.length t.job_reader

let invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~continue_on_error:ignore
      ~max_concurrent_jobs:(check (fun max_concurrent_jobs ->
        assert (max_concurrent_jobs > 0)))
      ~num_jobs_running:(check (fun num_jobs_running ->
        assert (num_jobs_running >= 0);
        assert (num_jobs_running <= t.max_concurrent_jobs)))
      ~am_reading_job:(check (fun am_reading_job ->
        if t.is_dead then assert (not am_reading_job)))
      ~is_dead:ignore
      ~job_reader:(check Pipe.Reader.invariant)
      ~job_writer:(check Pipe.Writer.invariant)
  with exn ->
    failwiths "Throttle.invariant failed" (exn, t) <:sexp_of< exn * t >>
;;

let kill t =
  t.is_dead <- true;
  begin match Pipe.read_now' t.job_reader with
  | `Eof | `Nothing_available -> ()
  | `Ok q -> Queue.iter q ~f:Internal_job.abort
  end;
  Pipe.close_read t.job_reader;
;;

let rec job_runner t =
  if not t.is_dead
    && not t.am_reading_job
    && t.num_jobs_running < t.max_concurrent_jobs
  then begin
    t.am_reading_job <- true;
    Pipe.read t.job_reader
    >>> fun res ->
    t.am_reading_job <- false;
    match res with
     | `Eof ->
       (* There is nothing in the API allowing [t.job_writer] to be closed.  However, the
          the throttle may, via [kill], close [t.job_reader], which would cause a
          [job_runner] that reads from the pipe to end up here. *)
       ()
     | `Ok job ->
       t.num_jobs_running <- t.num_jobs_running + 1;
       job_runner t;
       Internal_job.run job
       >>> fun res ->
       t.num_jobs_running <- t.num_jobs_running - 1;
       begin match res with
       | `Ok -> ()
       | `Raised -> if not t.continue_on_error then kill t
       end;
       job_runner t;
  end;
;;

let create ~continue_on_error ~max_concurrent_jobs =
  if max_concurrent_jobs <= 0 then
    failwiths "Throttle.create requires positive max_concurrent_jobs, but got"
      max_concurrent_jobs <:sexp_of< int >>;
  let (job_reader, job_writer) = Pipe.create () in
  let t =
    { max_concurrent_jobs;
      continue_on_error;
      am_reading_job = false;
      num_jobs_running = 0;
      is_dead = false;
      job_reader;
      job_writer;
    }
  in
  job_runner t;
  t
;;

module Job = struct
  type 'a t =
    { internal_job : Internal_job.t;
      result : [ `Ok of 'a | `Aborted | `Raised of exn ] Deferred.t;
    }

  let result t = t.result

  let create f =
    let (internal_job, result) = Internal_job.create f in
    { internal_job; result }
  ;;
end

let enqueue_job t job =
  if t.is_dead then failwith "cannot enqueue a job in dead throttle";
  don't_wait_for (Pipe.write t.job_writer job.Job.internal_job);
;;

let enqueue' t f =
  let job = Job.create f in
  enqueue_job t job;
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
      don't_wait_for (enqueue t (fun () ->
        incr dummy_jobs_running;
        if !dummy_jobs_running = t.max_concurrent_jobs then
          Ivar.fill all_dummy_jobs_running ();
        Ivar.read all_dummy_jobs_running))
    done)
;;

module Sequencer = struct
  type throttle = t
  type 'a t =
    { state : 'a;
      throttle : throttle;
    }

  let create ?(continue_on_error = false) a =
    { state = a;
      throttle = create ~continue_on_error ~max_concurrent_jobs:1;
    }
  ;;

  let enqueue t f = enqueue t.throttle (fun () -> f t.state)

  let num_jobs_waiting_to_start t = num_jobs_waiting_to_start t.throttle
end

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

  TEST_UNIT =
    (* Check that [max_concurrent_jobs] limit works, and is fully utilized. *)
    List.iter [ 1; 10; 100; 1000] ~f:(fun num_jobs ->
      List.iter [ 1; 10; 100; 1000] ~f:(fun max_concurrent_jobs ->
        let max_observed_concurrent_jobs = ref 0 in
        let num_concurrent_jobs = ref 0 in
        let job_starts = ref [] in
        let t = create ~continue_on_error:false ~max_concurrent_jobs in
        let d = prior_jobs_done t in
        stabilize ();
        assert (Deferred.is_determined d);
        let continue = Ivar.create () in
        let jobs = ref [] in
        for i = 0 to num_jobs - 1; do
          let job =
            enqueue t (fun () ->
              job_starts := i :: !job_starts;
              incr num_concurrent_jobs;
              max_observed_concurrent_jobs :=
                max !max_observed_concurrent_jobs !num_concurrent_jobs;
              assert (!num_concurrent_jobs <= max_concurrent_jobs);
              Ivar.read continue
              >>| fun () ->
              decr num_concurrent_jobs)
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
