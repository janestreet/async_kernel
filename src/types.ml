(* This file defines the mutually recursive types at the heart of Async.  The functions
   associated with the types are defined in the corresponding file(s) for each module.
   This file should define onlye types, not functions, since functions defined inside the
   recursive modules are not inlined.

   If you need to add functionality to a module but doing so would create a dependency
   cycle, split the file into pieces as needed to break the cycle, e.g. scheduler0.ml,
   scheduler1.ml, scheduler.ml.
*)

open! Core_kernel.Std
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
    | Empty_one_or_more_handlers
      :  ('a -> unit) * Execution_context.t * 'a Handler.t * 'a Handler.t
      ->                                       ('a, [> `Empty_one_or_more_handlers ]) t
    | Empty_one_handler
      :  ('a -> unit) * Execution_context.t -> ('a, [> `Empty_one_handler          ]) t
    | Empty                                  : ('a, [> `Empty                      ]) t
    | Full                             : 'a -> ('a, [> `Full                       ]) t
    | Indir                     : 'a Ivar.t -> ('a, [> `Indir                      ]) t
end = Cell

and Deferred : sig
  type +'a t
end = Deferred

and Execution_context : sig
  type t =
    { monitor            : Monitor.t
    ; priority           : Priority.t
    ; local_storage      : Univ_map.t
    ; backtrace_history  : Backtrace.t list
    }
end = Execution_context

and External_job : sig
  type t = T : Execution_context.t * ('a -> unit) * 'a -> t
end = External_job

and Handler : sig
  type 'a t =
    { mutable run       : 'a -> unit
    ; execution_context : Execution_context.t
    ; mutable prev      : 'a t
    ; mutable next      : 'a t
    }
end = Handler

and Ivar : sig
  type 'a t =
    { mutable cell : ('a, Cell.any) Cell.t
    }
end = Ivar

and Job : sig
  type slots = (Execution_context.t, Obj.t -> unit, Obj.t) Pool.Slots.t3
  type t = slots Pool.Pointer.t
end = Job

and Job_pool : sig
  type t = Job.slots Pool.t
end = Job_pool

and Job_queue : sig
  type t =
    { mutable num_jobs_run         : int
    ; mutable jobs_left_this_cycle : int
    ; mutable jobs                 : Core_kernel.Obj_array.t
    ; mutable mask                 : int
    ; mutable front                : int
    ; mutable length               : int
    }
end = Job_queue

and Jobs : sig
  type t =
    { scheduler        : Scheduler.t
    ; mutable job_pool : Job_pool.t
    ; normal           : Job_queue.t
    ; low              : Job_queue.t
    }
end = Jobs

and Monitor : sig
  type t =
    { name                            : Info.t
    ; here                            : Source_code_position.t option
    ; id                              : int
    ; parent                          : t option
    ; mutable next_error              : exn Ivar.t
    ; mutable handlers_for_all_errors : (Execution_context.t * (exn -> unit)) Bag.t
    ; mutable tails_for_all_errors    : exn Tail.t list
    ; mutable has_seen_error          : bool
    ; mutable is_detached             : bool
    }
end = Monitor

and Scheduler : sig
  type t =
    { mutable check_access                        : (unit -> unit) option
    ; mutable job_pool                            : Job_pool.t
    ; normal_priority_jobs                        : Job_queue.t
    ; low_priority_jobs                           : Job_queue.t
    ; mutable main_execution_context              : Execution_context.t
    ; mutable current_execution_context           : Execution_context.t
    ; mutable uncaught_exn                        : (Exn.t * Sexp.t) option
    ; mutable cycle_count                         : int
    ; mutable cycle_start                         : Time_ns.t
    ; mutable run_every_cycle_start               : (unit -> unit) list
    ; mutable last_cycle_time                     : Time_ns.Span.t
    ; mutable last_cycle_num_jobs                 : int
    ; mutable time_source                         : read_write Time_source.t1
    ; external_jobs                               : External_job.t Thread_safe_queue.t
    ; mutable thread_safe_external_job_hook       : unit -> unit
    ; mutable job_queued_hook                     : (Priority.t -> unit) option
    ; mutable event_added_hook                    : (Time_ns.t  -> unit) option
    ; mutable yield_ivar                          : unit Ivar.t option
    ; mutable check_invariants                    : bool
    ; mutable max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t
    ; mutable record_backtraces                   : bool
    }
end = Scheduler

and Stream : sig
  type 'a t = 'a next Deferred.t
  and 'a next = Nil | Cons of 'a * 'a t
end = Stream

and Tail : sig
  type 'a t =
    { mutable next: 'a Stream.next Ivar.t
    }
end = Tail

and Time_source : sig
  type -'rw t1 =
    { events        : Job.t Timing_wheel_ns.t
    ; handle_fired  : Job.t Timing_wheel_ns.Alarm.t -> unit
    ; is_wall_clock : bool
    ; scheduler     : Scheduler.t
    }
end = Time_source
