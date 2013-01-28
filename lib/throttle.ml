open Core.Std
open Import
open Deferred_std

type 'a outcome = [ `Ok of 'a | `Aborted | `Raised of exn ]

module Internal_job : sig
  type t with sexp_of

  val create : (unit -> 'a Deferred.t) -> t * 'a outcome Deferred.t
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
  { max_concurrent_jobs : int;
    continue_on_error : bool;
    job_reader : Internal_job.t Pipe.Reader.t;
    job_writer : Internal_job.t Pipe.Writer.t;
  }
with fields, sexp_of

let invariant t =
  assert (t.max_concurrent_jobs > 0);
  Pipe.Reader.invariant t.job_reader;
  Pipe.Writer.invariant t.job_writer;
;;

let kill t =
  begin match Pipe.read_now t.job_reader with
  | `Eof | `Nothing_available -> ()
  | `Ok q -> Queue.iter q ~f:Internal_job.abort
  end;
  Pipe.close_read t.job_reader;
;;

let one_job_runner t =
  whenever (Deferred.repeat_until_finished () (fun () ->
    Pipe.read t.job_reader
    >>= function
    | `Eof -> return (`Finished ())
    | `Ok job ->
      Internal_job.run job
      >>| function
      | `Ok -> `Repeat ()
      | `Raised ->
        if t.continue_on_error
        then `Repeat ()
        else (kill t; `Finished ())))
;;

let create ~continue_on_error ~max_concurrent_jobs =
  if max_concurrent_jobs <= 0 then
    failwiths "Throttle.create requires positive max_concurrent_jobs, but got"
      max_concurrent_jobs <:sexp_of< int >>;
  let (job_reader, job_writer) = Pipe.create () in
  let t =
    { max_concurrent_jobs;
      continue_on_error;
      job_reader;
      job_writer;
    }
  in
  for i = 1 to max_concurrent_jobs; do
    one_job_runner t;
  done;
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

let enqueue_job t job = whenever (Pipe.write t.job_writer job.Job.internal_job)

let enqueue' t f =
  if Pipe.is_closed t.job_writer then failwith "cannot enqueue to dead throttle";
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
      whenever (enqueue t (fun () ->
        incr dummy_jobs_running;
        if !dummy_jobs_running = t.max_concurrent_jobs then
          Ivar.fill all_dummy_jobs_running ();
        Ivar.read all_dummy_jobs_running))
    done)
;;

module Sequencer = struct
  type throttle = t
  type 'a t = { state : 'a; throttle : throttle }

  let create ?(continue_on_error=false) a = {
    state = a;
    throttle = create ~continue_on_error ~max_concurrent_jobs:1;
  }

  let enqueue t f = enqueue t.throttle (fun () -> f t.state)
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
