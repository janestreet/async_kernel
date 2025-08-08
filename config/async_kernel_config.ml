open Core
module Time_ns = Core_private.Time_ns_alternate_sexp
open Poly

let sec = Time_ns.Span.of_sec
let concat = String.concat

module Epoll_max_ready_events = Validated.Make (struct
    include Int

    let here = [%here]
    let validate = Int.validate_positive
  end)

module Io_uring_max_submission_entries = Validated.Make (struct
    include Int

    let here = [%here]
    let validate = Int.validate_positive
  end)

module Max_inter_cycle_timeout = Validated.Make (struct
    include Time_ns.Span

    let here = [%here]
    let validate = Time_ns.Span.validate_non_negative
  end)

module Min_inter_cycle_timeout = Validated.Make (struct
    include Time_ns.Span

    let here = [%here]
    let validate = Time_ns.Span.validate_non_negative
  end)

module Max_num_open_file_descrs = struct
  include Validated.Make (struct
      include Int

      let here = [%here]
      let validate = Int.validate_positive
    end)

  let default = create_exn (1 lsl 17)
  let equal (t1 : t) t2 = t1 = t2
end

module Max_num_threads = Validated.Make (struct
    include Int

    let here = [%here]
    let validate = Int.validate_positive
  end)

module Max_num_jobs_per_priority_per_cycle = Validated.Make (struct
    include Int

    let here = [%here]
    let validate = Int.validate_positive
  end)

module Dump_core_on_job_delay = struct
  module How_to_dump = struct
    type t =
      | Default
      | Call_abort
      | Call_gcore
    [@@deriving sexp]
  end

  type watch =
    { dump_if_delayed_by : Time_ns.Span.t
    ; how_to_dump : How_to_dump.t
    }
  [@@deriving sexp]

  type t =
    | Watch of watch
    | Do_not_watch
  [@@deriving sexp]
end

module Debug_tag = struct
  module T = struct
    type t =
      | All
      | Clock
      | Fd
      | File_descr_watcher
      | Finalizers
      | Interruptor
      | Monitor
      | Monitor_send_exn
      | Parallel
      | Reader
      | Scheduler
      | Shutdown
      | Thread_pool
      | Thread_safe
      | Writer
    [@@deriving compare ~localize, sexp]

    let equal = [%compare.equal: t]
  end

  include T
  include Sexpable.To_stringable (T)

  let list =
    [ All
    ; Clock
    ; Fd
    ; File_descr_watcher
    ; Finalizers
    ; Interruptor
    ; Monitor
    ; Monitor_send_exn
    ; Parallel
    ; Reader
    ; Scheduler
    ; Shutdown
    ; Thread_pool
    ; Thread_safe
    ; Writer
    ]
  ;;
end

module File_descr_watcher = struct
  module T = struct
    type t =
      | Epoll_if_timerfd
      | Epoll
      | Select
      | Io_uring
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  let list = [ Epoll_if_timerfd; Epoll; Select; Io_uring ]
end

module Io_uring_mode = struct
  module T = struct
    type t =
      | Disabled
      | Eventfd
      | If_available_eventfd
      | From_scheduler
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  let list = [ Disabled; Eventfd; If_available_eventfd; From_scheduler ]
end

type t =
  { abort_after_thread_pool_stuck_for : Time_ns.Span.t option [@sexp.option]
  ; check_invariants : bool option [@sexp.option]
  ; detect_invalid_access_from_thread : bool option [@sexp.option]
  ; dump_core_on_job_delay : Dump_core_on_job_delay.t option [@sexp.option]
  ; epoll_max_ready_events : Epoll_max_ready_events.t option [@sexp.option]
  ; io_uring_max_submission_entries : Io_uring_max_submission_entries.t option
       [@sexp.option]
  ; io_uring_mode : Io_uring_mode.t option [@sexp.option]
  ; file_descr_watcher : File_descr_watcher.t option [@sexp.option]
  ; max_inter_cycle_timeout : Max_inter_cycle_timeout.t option [@sexp.option]
  ; max_num_open_file_descrs : Max_num_open_file_descrs.t option [@sexp.option]
  ; max_num_threads : Max_num_threads.t option [@sexp.option]
  ; max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t option
       [@sexp.option]
  ; min_inter_cycle_timeout : Min_inter_cycle_timeout.t option [@sexp.option]
  ; print_debug_messages_for : Debug_tag.t list option [@sexp.option]
  ; record_backtraces : bool option [@sexp.option]
  ; report_thread_pool_stuck_for : Time_ns.Span.t option [@sexp.option]
  ; thread_pool_cpu_affinity : Thread_pool_cpu_affinity.t option [@sexp.option]
  ; timing_wheel_config : Timing_wheel.Config.t option [@sexp.option]
  }
[@@deriving fields ~fields ~iterators:(map, fold), sexp]

module Allow_extra_fields = struct
  type nonrec t = t =
    { abort_after_thread_pool_stuck_for : Time_ns.Span.t option [@sexp.option]
    ; check_invariants : bool option [@sexp.option]
    ; detect_invalid_access_from_thread : bool option [@sexp.option]
    ; dump_core_on_job_delay : Dump_core_on_job_delay.t option [@sexp.option]
    ; epoll_max_ready_events : Epoll_max_ready_events.t option [@sexp.option]
    ; io_uring_max_submission_entries : Io_uring_max_submission_entries.t option
         [@sexp.option]
    ; io_uring_mode : Io_uring_mode.t option [@sexp.option]
    ; file_descr_watcher : File_descr_watcher.t option [@sexp.option]
    ; max_inter_cycle_timeout : Max_inter_cycle_timeout.t option [@sexp.option]
    ; max_num_open_file_descrs : Max_num_open_file_descrs.t option [@sexp.option]
    ; max_num_threads : Max_num_threads.t option [@sexp.option]
    ; max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t option
         [@sexp.option]
    ; min_inter_cycle_timeout : Min_inter_cycle_timeout.t option [@sexp.option]
    ; print_debug_messages_for : Debug_tag.t list option [@sexp.option]
    ; record_backtraces : bool option [@sexp.option]
    ; report_thread_pool_stuck_for : Time_ns.Span.t option [@sexp.option]
    ; thread_pool_cpu_affinity : Thread_pool_cpu_affinity.t option [@sexp.option]
    ; timing_wheel_config : Timing_wheel.Config.t option [@sexp.option]
    }
  [@@sexp.allow_extra_fields] [@@deriving sexp]
end

let t_of_sexp ~allow_extra_fields sexp =
  match allow_extra_fields with
  | true -> Allow_extra_fields.t_of_sexp sexp
  | false -> t_of_sexp sexp
;;

let empty =
  { abort_after_thread_pool_stuck_for = None
  ; check_invariants = None
  ; detect_invalid_access_from_thread = None
  ; dump_core_on_job_delay = None
  ; epoll_max_ready_events = None
  ; io_uring_max_submission_entries = None
  ; io_uring_mode = None
  ; file_descr_watcher = None
  ; max_inter_cycle_timeout = None
  ; max_num_open_file_descrs = None
  ; max_num_threads = None
  ; max_num_jobs_per_priority_per_cycle = None
  ; min_inter_cycle_timeout = None
  ; print_debug_messages_for = None
  ; record_backtraces = None
  ; report_thread_pool_stuck_for = None
  ; thread_pool_cpu_affinity = None
  ; timing_wheel_config = None
  }
;;

let create
  ?abort_after_thread_pool_stuck_for
  ?check_invariants
  ?detect_invalid_access_from_thread
  ?dump_core_on_job_delay
  ?epoll_max_ready_events
  ?io_uring_max_submission_entries
  ?io_uring_mode
  ?file_descr_watcher
  ?max_inter_cycle_timeout
  ?max_num_open_file_descrs
  ?max_num_threads
  ?max_num_jobs_per_priority_per_cycle
  ?min_inter_cycle_timeout
  ?print_debug_messages_for
  ?record_backtraces
  ?report_thread_pool_stuck_for
  ?thread_pool_cpu_affinity
  ?timing_wheel_config
  ()
  =
  { abort_after_thread_pool_stuck_for
  ; check_invariants
  ; detect_invalid_access_from_thread
  ; dump_core_on_job_delay
  ; epoll_max_ready_events
  ; io_uring_max_submission_entries
  ; io_uring_mode
  ; file_descr_watcher
  ; max_inter_cycle_timeout
  ; max_num_open_file_descrs
  ; max_num_threads
  ; max_num_jobs_per_priority_per_cycle
  ; min_inter_cycle_timeout
  ; print_debug_messages_for
  ; record_backtraces
  ; report_thread_pool_stuck_for
  ; thread_pool_cpu_affinity
  ; timing_wheel_config
  }
;;

let default_timing_wheel_config_for_word_size (word_size : Word_size.t) =
  let module Alarm_precision = Timing_wheel.Alarm_precision in
  let alarm_precision, level_bits =
    match word_size with
    | W32 -> Alarm_precision.about_one_millisecond, [ 10; 10; 9 ]
    | W64 -> Alarm_precision.(div about_one_millisecond ~pow2:3), [ 14; 15; 9; 6 ]
  in
  Timing_wheel.Config.create
    ~alarm_precision
    ~level_bits:(Timing_wheel.Level_bits.create_exn level_bits)
    ()
;;

let default_timing_wheel_config =
  default_timing_wheel_config_for_word_size Word_size.word_size
;;

let hardcoded_default =
  (* For [file_descr_watcher] and [max_num_open_file_descrs] we choose the default for the
     common case that [epoll] is available.  There is some additional code in
     [Async_unix.Config] that checks whether [epoll] is actually available, and if not,
     uses [select] and a smaller number of file descriptors. *)
  { abort_after_thread_pool_stuck_for = Some (sec 60.)
  ; check_invariants = Some false
  ; detect_invalid_access_from_thread = Some false
  ; dump_core_on_job_delay = Some Do_not_watch
  ; epoll_max_ready_events = Some (Epoll_max_ready_events.create_exn 256)
  ; io_uring_max_submission_entries =
      Some
        (Io_uring_max_submission_entries.create_exn
           (* The default of 512 is motivated by a kernel bug/limitation where
              the io_uring queue needed to be contiguous in physical memory. When using
              1024 as the queue size, in conditions of fragmented free memory the
              allocation would crash with OOM sometimes. The smaller queue size makes the
              issue pretty much disappear. This is thought to be fixed in kernels >= 6.10
           *)
           512)
  ; io_uring_mode = Some Disabled
  ; file_descr_watcher = Some Epoll_if_timerfd
  ; max_inter_cycle_timeout = Some (Max_inter_cycle_timeout.create_exn (sec 0.05))
  ; max_num_open_file_descrs = Some Max_num_open_file_descrs.default
  ; max_num_threads = Some (Max_num_threads.create_exn 50)
  ; max_num_jobs_per_priority_per_cycle =
      Some (Max_num_jobs_per_priority_per_cycle.create_exn 500)
  ; min_inter_cycle_timeout = Some (Min_inter_cycle_timeout.create_exn (sec 0.))
  ; print_debug_messages_for = Some []
  ; record_backtraces = Some false
  ; report_thread_pool_stuck_for = Some (sec 1.)
  ; thread_pool_cpu_affinity = Some Inherit
  ; timing_wheel_config = Some default_timing_wheel_config
  }
;;

let example =
  { hardcoded_default with
    print_debug_messages_for = Some Debug_tag.[ Fd; Scheduler ]
  ; thread_pool_cpu_affinity =
      Some
        (Cpuset
           ([ 0; 1; 2 ] |> Int.Set.of_list |> Thread_pool_cpu_affinity.Cpuset.create_exn))
  }
;;

let merge t1 t2 =
  let override field = Option.first_some (Field.get field t1) (Field.get field t2) in
  Fields.map
    ~abort_after_thread_pool_stuck_for:override
    ~check_invariants:override
    ~detect_invalid_access_from_thread:override
    ~dump_core_on_job_delay:override
    ~epoll_max_ready_events:override
    ~io_uring_max_submission_entries:override
    ~io_uring_mode:override
    ~file_descr_watcher:override
    ~max_inter_cycle_timeout:override
    ~max_num_open_file_descrs:override
    ~max_num_threads:override
    ~max_num_jobs_per_priority_per_cycle:override
    ~min_inter_cycle_timeout:override
    ~print_debug_messages_for:override
    ~record_backtraces:override
    ~report_thread_pool_stuck_for:override
    ~thread_pool_cpu_affinity:override
    ~timing_wheel_config:override
;;

let reduce ts = List.fold_right ~init:empty ~f:merge ts
let environment_variable = "ASYNC_CONFIG"
let environment_variable_allow_extra_fields = "ASYNC_CONFIG_ALLOW_EXTRA_FIELDS"

let build_time_config =
  let config_str = Build_time_default_config.config in
  match
    Parsexp.Conv_many.parse_string config_str (t_of_sexp ~allow_extra_fields:false)
  with
  | Ok ts -> reduce ts
  | Error error ->
    raise_s
      [%sexp
        "invalid value for build-time DEFAULT_ASYNC_CONFIG variable"
        , (config_str : string)
        , (error : Parsexp.Conv_error.t)]
;;

let build_time_default = lazy (reduce [ build_time_config; hardcoded_default ])

let field_descriptions () : string =
  let field to_sexp description ac field =
    ( Field.name field
    , to_sexp (Option.value_exn (Field.get field (force build_time_default)))
    , description )
    :: ac
  in
  let fields =
    Fields.fold
      ~init:[]
      ~abort_after_thread_pool_stuck_for:
        (field
           [%sexp_of: Time_ns.Span.t]
           [ {|
  By default, Async will send an exception to the toplevel monitor
  if it detects that the thread pool is stuck for longer than this.
|}
           ])
      ~check_invariants:
        (field
           [%sexp_of: bool]
           [ {|
  If true, causes Async to regularly check invariants of its internal
  data structures.  This can substantially slow down your program.
|}
           ])
      ~detect_invalid_access_from_thread:
        (field
           [%sexp_of: bool]
           [ {|
  If true, causes Async routines to check if they are being accessed
  from some thread other than the thread currently holding the Async
  lock, which is not allowed and can lead to very confusing behavior.
|}
           ])
      ~dump_core_on_job_delay:
        (field
           [%sexp_of: Dump_core_on_job_delay.t]
           [ {|
  Can be set to [Do_not_watch] or:

    (Watch ((dump_if_delayed_by SPAN) (how_to_dump HOW)))

  If set to [Watch], then on program start this will start a regular
  Async job that increments a counter, and a C thread that will
  detect if that job is delayed by [dump_if_delayed_by], and if so,
  will core dump the program.  If available, [/usr/bin/gcore] is
  used by default to dump the core, which should allow the program
  to continue running.  Otherwise, [abort] will be called from C,
  which will kill the program while causing a core dump.  One can
  force [abort] or [gcore] via [how_to_dump], which should be one of:
  [Call_abort], [Call_gcore], or [Default].
|}
           ])
      ~epoll_max_ready_events:
        (field
           [%sexp_of: Epoll_max_ready_events.t]
           [ {|
  The maximum number of ready events that Async's call to [Epoll.wait]
  will handle.
|}
           ])
      ~io_uring_max_submission_entries:
        (field
           [%sexp_of: Io_uring_max_submission_entries.t]
           [ {|
  The maximum number of submission queue entries that can be stored within
  the [io_uring] submission queue at a given time. Will be rounded up to
  the nearest power of two internally within [io_uring].
|}
           ])
      ~io_uring_mode:
        (field
           [%sexp_of: Io_uring_mode.t]
           [ {|
  This determines the driver for using the io_uring. The default is not to use
  [io_uring] at all.
Allowed values are:|}
           ; concat ~sep:", " (List.map Io_uring_mode.list ~f:Io_uring_mode.to_string)
           ; {|.
|}
           ])
      ~file_descr_watcher:
        (field
           [%sexp_of: File_descr_watcher.t]
           [ {|
  This determines what OS subsystem Async uses to watch file descriptors for being ready.
  The default is to use [epoll] if timerfd's are supported and if not, use [select].

  Allowed values are:|}
           ; concat
               ~sep:", "
               (List.map File_descr_watcher.list ~f:File_descr_watcher.to_string)
           ; {|.
|}
           ])
      ~max_num_open_file_descrs:
        (field
           (fun default ->
             [%message
               ""
                 ~_:
                   (concat
                      [ "min "
                      ; default |> Max_num_open_file_descrs.raw |> Int.to_string_hum
                      ; " [ulimit -n -H]"
                      ]
                    : string)])
           [ {|
  The maximum number of open file descriptors allowed at any one time.|}
           ])
      ~max_num_threads:
        (field
           [%sexp_of: Max_num_threads.t]
           [ {|
  The maximum number of threads that Async will create to do blocking
  system calls and handle calls to [In_thread.run].
|}
           ])
      ~max_inter_cycle_timeout:
        (field
           [%sexp_of: Max_inter_cycle_timeout.t]
           [ {|
  The maximum amount of time the scheduler will pause between cycles
  when it has no jobs and is going to wait for I/O.  In principle one
  doesn't need this, and we could use an infinite timeout.  We instead
  use a small timeout (by default), to be more robust to bugs that
  could prevent Async from waking up and servicing events.  For
  example, as of 2013-01, the OCaml runtime has a bug that causes it
  to not necessarily run an OCaml signal handler in a timely manner.
  This in turn can cause a simple Async program that is waiting on a
  signal to hang, when in fact it should handle the signal.

  We use 50ms as the default timeout, because it is infrequent enough
  to have a negligible performance impact, and frequent enough that
  the latency would typically be not noticeable.  Also, 50ms is what
  the OCaml ticker thread uses.
|}
           ])
      ~max_num_jobs_per_priority_per_cycle:
        (field
           [%sexp_of: Max_num_jobs_per_priority_per_cycle.t]
           [ {|
  The maximum number of jobs that will be done at each priority within
  each Async cycle.  This limits how many jobs the scheduler will run
  before pausing to check for I/O.
|}
           ])
      ~min_inter_cycle_timeout:
        (field
           [%sexp_of: Min_inter_cycle_timeout.t]
           [ {|
  The minimum timeout the scheduler will pass to the OS when it checks
  for I/O between cycles.  This is zero by default.  Setting it to a
  nonzero value is used to increase thread fairness between the scheduler
  and other threads.  A plausible setting is 1us.  This is also
  configurable in OCaml via [Scheduler.set_min_inter_cycle_timeout].
|}
           ])
      ~print_debug_messages_for:
        (field
           [%sexp_of: Debug_tag.t list]
           [ {|
  A list of tags specifying which Async functions should print debug
  messages to stderr.  Each tag identifies a group of related Async
  functions.  The tag [all] means to print debug messages for all
  functions.  Allowed values are:

|}
           ; concat
               (List.map Debug_tag.list ~f:(fun d ->
                  concat [ "    "; Debug_tag.to_string d; "\n" ]))
           ; {|
  Turning on debug messages will substantially slow down most programs.
|}
           ])
      ~record_backtraces:
        (field
           [%sexp_of: bool]
           [ {|
  If true, this will cause Async to keep in the execution context the
  history of stack backtraces (obtained via [Backtrace.get]) that led
  to the current job.  If an Async job has an unhandled exception,
  this backtrace history will be recorded in the exception.  In
  particular the history will appear in an unhandled exception that
  reaches the main monitor.  This can have a substantial performance
  impact, both in running time and space usage.
|}
           ])
      ~report_thread_pool_stuck_for:
        (field
           [%sexp_of: Time_ns.Span.t]
           [ {|
  By default, Async will print a message to stderr every second if
  the thread pool is stuck for longer than this.
|}
           ])
      ~thread_pool_cpu_affinity:
        (field
           [%sexp_of: Thread_pool_cpu_affinity.t]
           [ {|
   Whether and how threads in the thread pool should be affinitized to CPUs.
|}
           ])
      ~timing_wheel_config:
        (field
           [%sexp_of: Timing_wheel.Config.t]
           [ {|
  This is used to adjust the time/space tradeoff in the timing wheel
  used to implement Async's clock.  Time is split into intervals of
  size [alarm_precision], and alarms with times in the same interval
  fire in the same cycle.  Level [i] in the timing wheel has an
  array of size [2^b], where [b] is the [i]'th entry in [level_bits].
|}
           ])
  in
  concat
    (List.map
       (List.sort fields ~compare:(fun (name1, _, _) (name2, _, _) ->
          String.compare name1 name2))
       ~f:(fun (name, default, description) ->
         concat
           ("\n" :: name :: " (default " :: Sexp.to_string default :: ")" :: description)))
;;

let help_message () =
  concat
    [ "The "
    ; environment_variable
    ; {| environment variable affects Async
in various ways.  Its value should be a sexp of the following form,
where all fields are optional:

|}
    ; Sexp.to_string_hum (sexp_of_t example)
    ; {|

Here is an explanation of each field.
|}
    ; field_descriptions ()
    ]
;;

let usage () =
  eprintf "%s%!" (help_message ());
  exit 1
;;

let parse_variable env_var ~allow_extra_fields =
  match Sys.getenv env_var with
  | None -> empty
  | Some "" -> usage ()
  | Some string ->
    (match Parsexp.Conv_many.parse_string string (t_of_sexp ~allow_extra_fields) with
     | Ok ts -> reduce ts
     | Error error ->
       eprintf
         "%s\n\n"
         (Sexp.to_string_hum
            [%sexp
              (sprintf "invalid value for %s environment variable" env_var : string)
              , (error : Parsexp.Conv_error.t)]);
       usage ())
;;

let t =
  reduce
    [ build_time_config
    ; parse_variable environment_variable ~allow_extra_fields:false
    ; parse_variable environment_variable_allow_extra_fields ~allow_extra_fields:true
    ]
;;

module Print_debug_messages_for = struct
  let print_debug_messages_for tag =
    match t.print_debug_messages_for with
    | None -> false
    | Some l -> List.mem l tag ~equal:Debug_tag.equal
  ;;

  let all = print_debug_messages_for All
  let debug tag = all || print_debug_messages_for tag
  let clock = debug Clock
  let fd = debug Fd
  let file_descr_watcher = debug File_descr_watcher
  let finalizers = debug Finalizers
  let interruptor = debug Interruptor
  let monitor = debug Monitor
  let monitor_send_exn = debug Monitor_send_exn
  let parallel = debug Parallel
  let reader = debug Reader
  let scheduler = debug Scheduler
  let shutdown = debug Shutdown
  let thread_pool = debug Thread_pool
  let thread_safe = debug Thread_safe
  let writer = debug Writer
end

let ( !! ) field =
  Option.value
    (Field.get field t)
    ~default:(Option.value_exn (Field.get field (force build_time_default)))
;;

let abort_after_thread_pool_stuck_for = !!Fields.abort_after_thread_pool_stuck_for
let check_invariants = !!Fields.check_invariants
let detect_invalid_access_from_thread = !!Fields.detect_invalid_access_from_thread
let epoll_max_ready_events = !!Fields.epoll_max_ready_events
let io_uring_max_submission_entries = !!Fields.io_uring_max_submission_entries
let io_uring_mode = !!Fields.io_uring_mode
let thread_pool_cpu_affinity = !!Fields.thread_pool_cpu_affinity
let file_descr_watcher = !!Fields.file_descr_watcher
let max_inter_cycle_timeout = !!Fields.max_inter_cycle_timeout
let max_num_open_file_descrs = !!Fields.max_num_open_file_descrs
let max_num_threads = !!Fields.max_num_threads
let max_num_jobs_per_priority_per_cycle = !!Fields.max_num_jobs_per_priority_per_cycle
let min_inter_cycle_timeout = !!Fields.min_inter_cycle_timeout
let record_backtraces = !!Fields.record_backtraces
let report_thread_pool_stuck_for = !!Fields.report_thread_pool_stuck_for
let timing_wheel_config = !!Fields.timing_wheel_config
let dump_core_on_job_delay = !!Fields.dump_core_on_job_delay

let t =
  { abort_after_thread_pool_stuck_for = Some abort_after_thread_pool_stuck_for
  ; check_invariants = Some check_invariants
  ; detect_invalid_access_from_thread = Some detect_invalid_access_from_thread
  ; dump_core_on_job_delay = Some dump_core_on_job_delay
  ; thread_pool_cpu_affinity = Some thread_pool_cpu_affinity
  ; epoll_max_ready_events = Some epoll_max_ready_events
  ; io_uring_max_submission_entries = Some io_uring_max_submission_entries
  ; io_uring_mode = Some io_uring_mode
  ; file_descr_watcher = Some file_descr_watcher
  ; max_inter_cycle_timeout = Some max_inter_cycle_timeout
  ; max_num_open_file_descrs = Some max_num_open_file_descrs
  ; max_num_threads = Some max_num_threads
  ; max_num_jobs_per_priority_per_cycle = Some max_num_jobs_per_priority_per_cycle
  ; min_inter_cycle_timeout = Some min_inter_cycle_timeout
  ; print_debug_messages_for = t.print_debug_messages_for
  ; record_backtraces = Some record_backtraces
  ; report_thread_pool_stuck_for = Some report_thread_pool_stuck_for
  ; timing_wheel_config = Some timing_wheel_config
  }
;;

let task_id = ref (fun () -> Sexp.Atom "<no task id>")
