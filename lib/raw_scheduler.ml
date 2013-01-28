open Core.Std
open Import

module Execution_context = Execution_context
module Clock_event = Raw_clock_event
module Ivar = Raw_ivar
module Tail = Raw_tail

let debug = Debug.debug

type 'a tail = ('a, Execution_context.t) Tail.t with sexp_of

module T = struct
  type t =
    { jobs : Execution_context.t Job.t Jobs.t sexp_opaque;
      mutable current_execution_context : Execution_context.t sexp_opaque;
      mutable main_execution_context : Execution_context.t sexp_opaque;
      mutable max_num_jobs_per_priority_per_cycle : int;
      mutable uncaught_exception : Error.t option;
      mutable num_jobs_run : int;
      mutable cycle_count : int;
      mutable cycle_start : Time.t;
      cycle_times : Time.Span.t tail;
      cycle_num_jobs : int tail;
      events : Execution_context.t Clock_event.t Events.t;
    }
  with sexp_of
end

include T

let t =
  let now = Time.epoch in
  { jobs = Jobs.create ();
    current_execution_context = Execution_context.bogus;
    main_execution_context = Execution_context.bogus;
    max_num_jobs_per_priority_per_cycle = 500;
    uncaught_exception = None;
    num_jobs_run = 0;
    cycle_start = now;
    cycle_count = 0;
    cycle_times = Tail.create ();
    cycle_num_jobs = Tail.create ();
    events = Events.create ~now;
  }
;;

let invariant () =
  try
    assert (t.num_jobs_run >= 0);
    assert (t.cycle_count >= 0);
    Events.invariant t.events;
    try
      Events.iter t.events ~f:(fun events_event ->
        let event = Events.Event.value events_event in
        let module E = Clock_event in
        match event.E.state with
        | E.Waiting { E. event = events_event'; ready } ->
          assert (phys_equal events_event events_event');
          assert (Ivar.is_empty ready);
        | _ -> assert false);
    with exn ->
      failwiths "events problem" (exn, t.events)
        (<:sexp_of< exn * Execution_context.t Clock_event.t Events.t >>);
  with exn ->
    failwiths "Scheduler.invariant failed" (exn, t) <:sexp_of< exn * t >>
;;

let current_execution_context () =
  let execution_context = t.current_execution_context in
  if Debug.record_backtraces
  then Execution_context.record_backtrace execution_context
  else execution_context
;;

let set_execution_context execution_context =
  (* Avoid a caml_modify in most cases. *)
  if not (phys_equal execution_context t.current_execution_context) then
    t.current_execution_context <- execution_context;
;;

let with_execution_context tmp_context ~f =
  let old_context = current_execution_context () in
  set_execution_context tmp_context;
  protect ~f ~finally:(fun () -> set_execution_context old_context);
;;

let add_job job =
  let priority = (Job.execution_context job).Execution_context.priority in
  if debug then Debug.log "enqueing job" priority <:sexp_of< Priority.t >>;
  Jobs.add t.jobs priority job;
;;

