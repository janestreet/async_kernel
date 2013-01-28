open Core.Std
open Raw_scheduler.T

let debug = Debug.scheduler

type t = Raw_scheduler.t with sexp_of

let t = Raw_scheduler.t

include struct
  open Raw_scheduler
  let current_execution_context = current_execution_context
  let set_execution_context     = set_execution_context
  let with_execution_context    = with_execution_context
  let set_check_access          = set_check_access
  let check_access              = check_access
end

include Monitor.Exported_for_scheduler

let invariant = Raw_scheduler.invariant

let add_job execution_context f a =
  Raw_scheduler.(add_job (t ())) (Job.create execution_context f a)
;;

let uncaught_exn t = t.uncaught_exn

let main_execution_context = (t ()).main_execution_context

let num_pending_jobs t = Jobs.length t.jobs

let next_upcoming_event t =
  match Events.next_upcoming t.events with
  | None -> None
  | Some events_event -> Some (Events.Event.at events_event)
;;

let cycle_start t = t.cycle_start

let cycle_times t = Tail.collect (Tail.of_raw t.cycle_times)

let cycle_num_jobs t = Tail.collect (Tail.of_raw t.cycle_num_jobs)

let cycle_count t = t.cycle_count

let num_jobs_run t = t.num_jobs_run

let set_max_num_jobs_per_priority_per_cycle t int =
  if int <= 0 then
    failwiths "max_num_jobs_per_priority_per_cycle must be > 0" int <:sexp_of< int >>;
  t.max_num_jobs_per_priority_per_cycle <- int;
;;

let debug_run_job = debug || Debug.run_job

let run_cycle t =
  let do_one job =
    let execution_context = Job.execution_context job in
    if debug_run_job then
      Debug.log "running job"
        execution_context.Execution_context.backtrace_history
        (<:sexp_of< Backtrace.t list >>);
    t.num_jobs_run <- t.num_jobs_run + 1;
    set_execution_context t execution_context;
    (* [Job.run] may raise, in which case the exn is handled by [Jobs.run_all]. *)
    Job.run job;
    if debug_run_job then
      Debug.log "finished running job"
        execution_context.Execution_context.backtrace_history
        (<:sexp_of< Backtrace.t list >>);
  in
  if debug then Debug.log "run_cycle starting" t <:sexp_of< t >>;
  let now = Time.now () in
  t.cycle_count <- t.cycle_count + 1;
  t.cycle_start <- now;
  let num_jobs_run_at_start_of_cycle = t.num_jobs_run in
  Tail.extend (Tail.of_raw t.cycle_times) t.last_cycle_time;
  Tail.extend (Tail.of_raw t.cycle_num_jobs) t.last_cycle_num_jobs;
  begin match Events.advance_clock t.events ~to_:now with
  | `Not_in_the_future ->
    (* This could conceivably happen with NTP tweaking the clock.  There's no reason
       to do anything other than press on. *)
    ()
  | `Ok events -> List.iter events ~f:Clock_event.fire
  end;
  Jobs.start_cycle t.jobs
    ~max_num_jobs_per_priority:t.max_num_jobs_per_priority_per_cycle;
  let rec run_jobs () =
    match Jobs.run_all t.jobs do_one with
    | Ok () -> ()
    | Error exn ->
      Monitor.send_exn (Monitor.current ()) exn ~backtrace:`Get;
      (* [run_all] stopped due to an exn.  There may still be jobs that could be run
         this cycle, so [run_jobs] again. *)
      run_jobs ()
  in
  run_jobs ();
  t.last_cycle_time <- Time.diff (Time.now ()) t.cycle_start;
  t.last_cycle_num_jobs <- t.num_jobs_run - num_jobs_run_at_start_of_cycle;
  if debug then
    Debug.log "run_cycle finished"
      (t.uncaught_exn, is_some (Events.next_upcoming t.events))
      (<:sexp_of< Error.t option * bool >>);
;;

let run_cycles_until_no_jobs_remain () =
  if debug then Debug.log_string "run_cycles_until_no_jobs_remain starting";
  let t = t () in
  let rec loop () =
    run_cycle t;
    if not (Jobs.is_empty t.jobs) then loop ();
  in
  loop ();
  if debug then Debug.log_string "run_cycles_until_no_jobs_remain finished";
;;

let reset_in_forked_process () =
  if debug then Debug.log_string "reset_in_forked_process";
  (* There is no need to empty [main_monitor_hole]. *)
  Backpatched.Hole.empty Execution_context.main_work_group_hole;
  Raw_scheduler.(t_ref := create ());
;;
