(* This file defines the mutually recursive types at the heart of Async.  The functions
   associated with the types are defined in the corresponding file(s) for each module.
   This file should define only types, not functions, since functions defined inside the
   recursive modules are not inlined.

   If you need to add functionality to a module but doing so would create a dependency
   cycle, split the file into pieces as needed to break the cycle, e.g. scheduler0.ml,
   scheduler1.ml, scheduler.ml. *)

open! Core
open! Import

module rec Cell : sig
  type any =
    [ `Empty
    | `Empty_one_handler
    | `Empty_one_or_more_handlers
    | `Full
    | `Indir
    ]

  type ('a, 'b) t =
    | Empty_one_or_more_handlers :
        { mutable run : 'a -> unit
        ; execution_context : Execution_context.t
        ; mutable prev : 'a Handler.t
        ; mutable next : 'a Handler.t
        }
        -> ('a, [> `Empty_one_or_more_handlers ]) t
    | Empty_one_handler :
        ('a -> unit) * Execution_context.t
        -> ('a, [> `Empty_one_handler ]) t
    | Empty : ('a, [> `Empty ]) t
    | Full : 'a -> ('a, [> `Full ]) t
    | Indir : 'a Ivar.t -> ('a, [> `Indir ]) t
end =
  Cell

and Handler : sig
  type 'a t = ('a, [ `Empty_one_or_more_handlers ]) Cell.t
end =
  Handler

and Ivar : sig
  type 'a t = { mutable cell : ('a, Cell.any) Cell.t }

  module Immutable : sig
    type 'a t = { cell : ('a, Cell.any) Cell.t }
  end
end =
  Ivar

and Deferred : sig
  type +!'a t
end =
  Deferred

and Execution_context : sig
  type t =
    { monitor : Monitor.t
    ; priority : Priority.t
    ; local_storage : Univ_map.t
    ; backtrace_history : Backtrace.t list
    }
end =
  Execution_context

and Forwarding : sig
  type t =
    | Detached
    | Parent of Monitor.t
    | Report_uncaught_exn
end =
  Forwarding

and Monitor : sig
  type t =
    { name : Info.t
    ; here : Source_code_position.t
    ; id : int
    ; mutable next_error : exn Ivar.t
    ; mutable handlers_for_all_errors : (Execution_context.t * (exn -> unit)) Bag.t
    ; mutable tails_for_all_errors : exn Tail.t list
    ; mutable has_seen_error : bool
    ; mutable forwarding : Forwarding.t
    }
end =
  Monitor

and Tail : sig
  type 'a t = { mutable next : 'a Stream.next Ivar.t }
end =
  Tail

and Stream : sig
  type 'a t = 'a next Deferred.t

  and 'a next =
    | Nil
    | Cons of 'a * 'a t
end =
  Stream

(* We avoid using [module rec] to define [Bvar], so that [to_repr] and [of_repr] are
   inlined. *)
module Bvar : sig
  type ('a, -'permission) t

  (** [repr] exists so that we may hide the implementation of a [Bvar.t], and then add a
      phantom type to it upstream. Without this, the phantom type variable would allow for
      anything to be coerced in and out, since it is unused. *)
  type 'a repr =
    { mutable has_any_waiters : bool
    ; mutable ivar : 'a Ivar.t
    }

  val of_repr : 'a repr -> ('a, 'permission) t
  val to_repr : ('a, 'permission) t -> 'a repr
end = struct
  type 'a repr =
    { mutable has_any_waiters : bool
    ; mutable ivar : 'a Ivar.t
    }

  type ('a, 'permission) t = 'a repr

  let to_repr t = t
  let of_repr t = t
end

module rec Event : sig
  module Status : sig
    type t =
      | Fired
      | Happening_periodic_event
      | Scheduled
      | Unscheduled
  end

  module Option : sig
    type t
  end

  type t =
    { mutable alarm : Job_or_event.t Timing_wheel.Alarm.t
    ; mutable at : Time_ns.t
    ; callback : unit -> unit
    ; execution_context : Execution_context.t
    ; mutable interval : Time_ns.Span.t option
    ; mutable next_fired : Option.t
    ; mutable prev_fired : Option.t
    ; mutable status : Status.t
    }
end =
  Event

and External_job : sig
  type 'a inner =
    { execution_context : Execution_context.t
    ; f : 'a -> unit
    ; a : 'a
    }

  type t' = T : 'a inner -> t' [@@unboxed]
  type t = (t', Capsule.Expert.initial) Capsule.Data.t
end =
  External_job

and Job : sig
  type slots = (Execution_context.t, Obj.t -> unit, Obj.t) Pool.Slots.t3
  type t = slots Pool.Pointer.t
end =
  Job

and Job_or_event : sig
  type t
end =
  Job_or_event

and Job_pool : sig
  type t = Job.slots Pool.t
end =
  Job_pool

and Job_queue : sig
  type t =
    { mutable num_jobs_run : int
    ; mutable jobs_left_this_cycle : int
    ; mutable jobs : Obj.t Uniform_array.t
    ; mutable mask : int
    ; mutable front : int
    ; mutable length : int
    ; mutable backtrace_of_first_enqueue : Backtrace.t option
    }
end =
  Job_queue

and Jobs : sig
  type t =
    { scheduler : Scheduler.t
    ; mutable job_pool : Job_pool.t
    ; normal : Job_queue.t
    ; low : Job_queue.t
    }
end =
  Jobs

and Scheduler : sig
  type t =
    { mutable check_access : (unit -> unit) option
    ; mutable job_pool : Job_pool.t
    ; normal_priority_jobs : Job_queue.t
    ; low_priority_jobs : Job_queue.t
    ; very_low_priority_workers : Very_low_priority_worker.t Deque.t
    ; main_execution_context : Execution_context.t
    ; mutable current_execution_context : Execution_context.t
    ; mutable uncaught_exn : (Exn.t * Sexp.t) option
    ; mutable cycle_count : int
    ; mutable cycle_start : Time_ns.t
    ; mutable in_cycle : bool
    ; mutable run_every_cycle_start : Cycle_hook.t array
    ; run_every_cycle_start_state : (Cycle_hook_handle.t, Cycle_hook.t) Hashtbl.t
    ; mutable run_every_cycle_end : Cycle_hook.t array
    ; run_every_cycle_end_state : (Cycle_hook_handle.t, Cycle_hook.t) Hashtbl.t
    ; mutable last_cycle_time : Time_ns.Span.t
    ; mutable last_cycle_num_jobs : int
    ; job_infos_for_cycle : Job_infos_for_cycle.t
    ; mutable total_cycle_time : Time_ns.Span.t
    ; mutable time_source : read_write Time_source.t1
    ; external_jobs : External_job.t Mpsc_queue.t
    ; thread_safe_external_job_hook : (unit -> unit) Atomic.t
    ; mutable job_queued_hook : (Priority.t -> unit) option
    ; mutable event_added_hook : (Time_ns.t -> unit) option
    ; mutable yield : (unit, read_write) Bvar.t
    ; mutable yield_until_no_jobs_remain : (unit, read_write) Bvar.t
    ; mutable check_invariants : bool
    ; mutable max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t
    ; mutable record_backtraces : bool
    }
end =
  Scheduler

and Cycle_hook : sig
  type t = unit -> unit
end =
  Cycle_hook

and Cycle_hook_handle : Unique_id.Id = Unique_id.Int63 ()
and Time_source_id : Unique_id.Id = Unique_id.Int63 ()

and Time_source : sig
  type -'rw t1 =
    { id : Time_source_id.t
    ; mutable advance_errors : Error.t list
    ; mutable am_advancing : bool
    ; events : Job_or_event.t Timing_wheel.t
    ; mutable fired_events : Event.Option.t
    ; mutable most_recently_fired : Event.Option.t
    ; handle_fired : Job_or_event.t Timing_wheel.Alarm.t -> unit
    ; is_wall_clock : bool
    ; scheduler : Scheduler.t
    }
end =
  Time_source

and Very_low_priority_worker : sig
  module Exec_result : sig
    type t =
      | Finished
      | Not_finished
  end

  type t =
    { execution_context : Execution_context.t
    ; exec : unit -> Exec_result.t
    }
end =
  Very_low_priority_worker
