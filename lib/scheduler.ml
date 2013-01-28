open Core.Std
open Import
open Raw_scheduler.T

let debug = Debug.debug

let t = Raw_scheduler.t

include struct
  open Raw_scheduler
  let current_execution_context = current_execution_context
  let set_execution_context     = set_execution_context
  let with_execution_context    = with_execution_context
  let invariant                 = invariant
end

include Monitor.Exported_for_scheduler

let add_job execution_context f a =
  Raw_scheduler.add_job (Job.create execution_context f a)
;;

let uncaught_exception () = t.uncaught_exception

let main_execution_context () = t.main_execution_context

let set_main_execution_context execution_context =
  t.main_execution_context <- execution_context;
;;

let set_block_group block_group =
  set_execution_context
    { current_execution_context () with Execution_context. block_group };
;;

let initialize_execution_context block_group =
  set_execution_context
    { Execution_context.
      block_group;
      monitor = Monitor.main;
      priority = Priority.normal;
      backtrace_history = [];
    };
;;

let num_pending_jobs () = Jobs.length t.jobs

let next_upcoming_event () =
  match Events.next_upcoming t.events with
  | None -> None
  | Some events_event -> Some (Events.Event.at events_event)
;;

let cycle_start () = t.cycle_start

let cycle_times () = Tail.collect t.cycle_times

let cycle_num_jobs () = Tail.collect t.cycle_num_jobs

let cycle_count () = t.cycle_count

let num_jobs_run () = t.num_jobs_run

let set_max_num_jobs_per_priority_per_cycle int =
  if int <= 0 then
    failwiths "max_num_jobs_per_priority_per_cycle must be > 0" int <:sexp_of< int >>;
  t.max_num_jobs_per_priority_per_cycle <- int;
;;

let run_cycle =
  let rec do_batch jobs =
    if Option.is_none t.uncaught_exception then begin
      match jobs with
      | [] -> ()
      | job::jobs ->
        let execution_context = Job.execution_context job in
        if debug then
          Debug.log "running job" execution_context.Execution_context.backtrace_history
            (<:sexp_of< Backtrace.t list >>);
        t.num_jobs_run <- t.num_jobs_run + 1;
        set_execution_context execution_context;
        begin
          try
            Job.run job;
          with exn -> Monitor.send_exn (Monitor.current ()) exn ~backtrace:`Get;
        end;
        do_batch jobs
    end
  in
  fun () ->
    if debug then Debug.log "run_cycle starting" (Jobs.length t.jobs) <:sexp_of< int >>;
    let now = Time.now () in
    t.cycle_count <- t.cycle_count + 1;
    t.cycle_start <- now;
    let num_jobs_run_at_start_of_cycle = t.num_jobs_run in
    begin match Events.advance_clock t.events ~to_:now with
    | `Not_in_the_future ->
      (* This could conceivably happen with NTP tweaking the clock.  There's no reason
         to do anything other than press on. *)
      ()
    | `Ok events -> List.iter events ~f:Clock_event.fire
    end;
    Jobs.start_cycle t.jobs
      ~max_num_jobs_per_priority:t.max_num_jobs_per_priority_per_cycle;
    let rec run_jobs t =
      match Jobs.get t.jobs with
      | [] -> ()
      | jobs -> do_batch jobs; run_jobs t
    in
    run_jobs t;
    let result = if Jobs.is_empty t.jobs then `No_jobs_remain else `Jobs_remain in
    (* Extending streams can potentially add more jobs if somebody is listening to them,
       so we have to check [Jobs.is_empty] before extending. *)
    Tail.extend t.cycle_times (Time.diff (Time.now ()) t.cycle_start);
    Tail.extend t.cycle_num_jobs (t.num_jobs_run - num_jobs_run_at_start_of_cycle);
    if debug then
      Debug.log "run_cycle finished"
        (t.uncaught_exception, result, is_some (Events.next_upcoming t.events))
        (<:sexp_of< Error.t option * [ `No_jobs_remain | `Jobs_remain ] * bool >>);
    result
;;

let run_cycles_until_no_jobs_remain () =
  if debug then Debug.log_string "run_cycles_until_no_jobs_remain starting";
  let rec loop () =
    match run_cycle () with
    | `No_jobs_remain -> ()
    | `Jobs_remain -> loop ()
  in
  loop ();
  if debug then Debug.log_string "run_cycles_until_no_jobs_remain finished";

;;
