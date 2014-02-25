open Core.Std

let concat = String.concat

module Epoll_max_ready_events =
  Validated.Make (struct
    include Int
    let here = _here_
    let validate = Int.validate_positive
  end)

module Max_inter_cycle_timeout =
  Validated.Make (struct
    include Time.Span
    let here = _here_
    let validate = Time.Span.validate_positive
  end)

module Max_num_open_file_descrs =
  Validated.Make (struct
    include Int
    let here = _here_
    let validate = Int.validate_positive
  end)

module Max_num_threads =
  Validated.Make (struct
    include Int
    let here = _here_
    let validate = Int.validate_positive
  end)

module Max_num_jobs_per_priority_per_cycle =
  Validated.Make (struct
    include Int
    let here = _here_
    let validate = Int.validate_positive
  end)

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
    with sexp
  end

  include T

  include Sexpable.To_stringable (T)

  let list =
    [ All;
      Clock;
      Fd;
      File_descr_watcher;
      Finalizers;
      Interruptor;
      Monitor;
      Monitor_send_exn;
      Parallel;
      Reader;
      Scheduler;
      Shutdown;
      Thread_pool;
      Thread_safe;
      Writer
    ]
  ;;

end

module File_descr_watcher = struct

  module T = struct
    type t = Epoll | Select with sexp
  end

  include T

  include Sexpable.To_stringable (T)

  let list = [ Epoll; Select ]

end

type t =
  { abort_after_thread_pool_stuck_for   : Time.Span.t                           sexp_option
  ; check_invariants                    : bool                                  sexp_option
  ; detect_invalid_access_from_thread   : bool                                  sexp_option
  ; epoll_max_ready_events              : Epoll_max_ready_events.t              sexp_option
  ; file_descr_watcher                  : File_descr_watcher.t                  sexp_option
  ; max_inter_cycle_timeout             : Max_inter_cycle_timeout.t             sexp_option
  ; max_num_open_file_descrs            : Max_num_open_file_descrs.t            sexp_option
  ; max_num_threads                     : Max_num_threads.t                     sexp_option
  ; max_num_jobs_per_priority_per_cycle : Max_num_jobs_per_priority_per_cycle.t sexp_option
  ; print_debug_messages_for            : Debug_tag.t list                      sexp_option
  ; record_backtraces                   : bool                                  sexp_option
  ; report_thread_pool_stuck_for        : Time.Span.t                           sexp_option
  ; timing_wheel_config                 : Timing_wheel.Config.t                 sexp_option
  }
with fields, sexp

let empty =
  { abort_after_thread_pool_stuck_for   = None
  ; check_invariants                    = None
  ; detect_invalid_access_from_thread   = None
  ; epoll_max_ready_events              = None
  ; file_descr_watcher                  = None
  ; max_inter_cycle_timeout             = None
  ; max_num_open_file_descrs            = None
  ; max_num_threads                     = None
  ; max_num_jobs_per_priority_per_cycle = None
  ; print_debug_messages_for            = None
  ; record_backtraces                   = None
  ; report_thread_pool_stuck_for        = None
  ; timing_wheel_config                 = None
  }
;;

let default_file_descr_watcher, default_max_num_open_file_descrs =
  (* Without timerfd, epoll_wait(2) timeouts would have only millisecond precision. *)
  if Result.is_ok Linux_ext.Timerfd.create
  then File_descr_watcher.Epoll , Max_num_open_file_descrs.create_exn 8192
  else File_descr_watcher.Select, Max_num_open_file_descrs.create_exn 1024
;;

let default_timing_wheel_config word_size =
  let module W = Word_size in
  let alarm_precision, level_bits =
    match word_size with
    | W.W32 -> Time.Span.of_ms 1. , [ 10; 10; 9;   ]
    | W.W64 -> Time.Span.of_ms 0.1, [ 15; 15; 9; 6 ]
  in
  Timing_wheel.Config.create
    ~alarm_precision
    ~level_bits:(Timing_wheel.Level_bits.create_exn level_bits)
    ()
;;

TEST_UNIT =
  let module L = Timing_wheel.Level_bits in
  let module W = Word_size in
  List.iter [ W.W32; W.W64 ] ~f:(fun word_size ->
    let config = default_timing_wheel_config word_size in
    let actual_durations = Timing_wheel.Config.durations config in
    let year = Time.Span.(scale day) 365. in
    let lower_bounds =
      match word_size with
      | W.W32 -> Time.Span.([ 1., second;
                              17., minute;
                              6.2, day;
                            ])
      | W.W64 -> Time.Span.([ 3.2, second;
                              1.2, day;
                              1.7, year;
                              111., year;
                            ])
    in
    let lower_bounds =
      List.map lower_bounds ~f:(fun (scale, span) -> Time.Span.scale span scale)
    in
    try
      List.iter2_exn actual_durations lower_bounds ~f:(fun actual bound ->
        assert (Time.Span.(>=) actual bound))
    with exn ->
      failwiths "lower bound violated" (exn, actual_durations, lower_bounds)
        <:sexp_of< exn * Time.Span.t list * Time.Span.t list >>)
;;

let default_timing_wheel_config = default_timing_wheel_config Word_size.word_size

let default =
  { abort_after_thread_pool_stuck_for   = Some (sec 60.)
  ; check_invariants                    = Some false
  ; detect_invalid_access_from_thread   = Some false
  ; epoll_max_ready_events              = Some (Epoll_max_ready_events.create_exn 256)
  ; file_descr_watcher                  = Some default_file_descr_watcher
  ; max_inter_cycle_timeout             =
      Some (Max_inter_cycle_timeout.create_exn (sec 0.05))
  ; max_num_open_file_descrs            = Some default_max_num_open_file_descrs
  ; max_num_threads                     = Some (Max_num_threads.create_exn 50)
  ; max_num_jobs_per_priority_per_cycle =
      Some (Max_num_jobs_per_priority_per_cycle.create_exn 500)
  ; print_debug_messages_for            = Some []
  ; record_backtraces                   = Some false
  ; report_thread_pool_stuck_for        = Some (sec 1.)
  ; timing_wheel_config                 = Some default_timing_wheel_config
  }
;;

let example =
  { default with
    print_debug_messages_for = Some Debug_tag.([ Fd; Scheduler ]);
  }
;;

let environment_variable = "ASYNC_CONFIG"

let field_descriptions () : string =
  let field to_sexp description ac field =
    (Field.name field,
     to_sexp (Option.value_exn (Field.get field default)),
     description
    ) :: ac
  in
  let fields =
    Fields.fold ~init:[]
      ~abort_after_thread_pool_stuck_for:(field <:sexp_of< Time.Span.t >>
                                            ["
  By default, async will send an exception to the toplevel monitor if it detects that the
  thread pool is stuck for longer than this.
"
                                            ])
      ~check_invariants:(field <:sexp_of< bool >>
                           ["
  If true, causes async to regularly check invariants of its internal
  data structures.  This can substantially slow down your program.
"
                           ])
      ~detect_invalid_access_from_thread:(field <:sexp_of< bool >>
                                            ["
  If true, causes async routines to check if they are being accessed
  from some thread other than the thread currently holding the async
  lock, which is not allowed and can lead to very confusing behavior.
"
                                            ])

      ~epoll_max_ready_events:(field <:sexp_of< Epoll_max_ready_events.t >>
                                 ["
  The maximum number of ready events that async's call to [Epoll.wait]
  will handle.
"
                                 ])
      ~file_descr_watcher:(field <:sexp_of< File_descr_watcher.t >>
                             ["
  This determines what OS subsystem async uses to watch file
  descriptors for being ready.  Allowed values are:";
                              concat ~sep:", "
                                (List.map File_descr_watcher.list
                                   ~f:File_descr_watcher.to_string);
                              ".
"
                             ])
      ~max_num_open_file_descrs:(field <:sexp_of< Max_num_open_file_descrs.t >>
                                   ["
  The maximum number of open file descriptors allowed at any one time.
"
                                   ])
      ~max_num_threads:(field <:sexp_of< Max_num_threads.t >>
                          ["
  The maximum number of threads that async will create to do blocking
  system calls and handle calls to [In_thread.run].
"
                          ])
      ~max_inter_cycle_timeout:(field <:sexp_of< Max_inter_cycle_timeout.t >>
                                  ["
  The maximum amount of time the scheduler will pause between cycles
  when it has no jobs and is going to wait for I/O.  In principle one
  doesn't need this, and we could use an infinite timeout.  We instead
  use a small timeout (by default), to be more robust to bugs that
  could prevent async from waking up and servicing events.  For
  example, as of 2013-01, the OCaml runtime has a bug that causes it
  to not necessarily run an OCaml signal handler in a timely manner.
  This in turn can cause a simple async program that is waiting on a
  signal to hang, when in fact it should handle the signal.

  We use 50ms as the default timeout, because it is infrequent enough
  to have a negligible performance impact, and frequent enough that
  the latency would typically be not noticeable.  Also, 50ms is what
  the OCaml ticker thread uses.
"
                                  ])
      ~max_num_jobs_per_priority_per_cycle:
        (field <:sexp_of< Max_num_jobs_per_priority_per_cycle.t >>
           ["
  The maximum number of jobs that will be done at each priority within
  each async cycle.  This limits how many jobs the scheduler will run
  before pausing to check for I/O.
"
           ])
      ~print_debug_messages_for:
        (field <:sexp_of< Debug_tag.t list >>
           ["
  A list of tags specifying which async functions should print debug
  messages to stderr.  Each tag identifies a group of related async
  functions.  The tag 'all' means to print debug messages for all
  functions.  Allowed values are:

";
            concat (List.map Debug_tag.list
                      ~f:(fun d ->
                        concat ["    "; Debug_tag.to_string d; "\n"]));
            "
  Turning on debug messages will substantially slow down most programs.
"
           ])
      ~record_backtraces:(field <:sexp_of< bool >>
                            ["
  If true, this will cause async to keep in the execution context the
  history of stack backtraces (obtained via [Backtrace.get]) that led
  to the current job.  If an async job has an unhandled exception,
  this backtrace history will be recorded in the exception.  In
  particular the history will appean in an unhandled exception that
  reaches the main monitor.  This can have a substantial performance
  impact, both in running time and space usage.
"
                            ])
      ~report_thread_pool_stuck_for:(field <:sexp_of< Time.Span.t >>
                                       ["
  By default, async will print a message to stderr every second if the thread pool is
  stuck for longer than this.
"
                                       ])
      ~timing_wheel_config:(field <:sexp_of< Timing_wheel.Config.t >>
                              ["
  This is used to adjust the time/space tradeoff in the timing wheel used to implement
  async's clock.  Time is split into intervals of size [alarm_precision], and alarms with
  times in the same interval fire in the same cycle.  Level [i] in the timing wheel has
  an array of size [2^b], where [b] is the [i]'th entry in [level_bits].
"
                              ])
  in
  concat
    (List.map
       (List.sort fields
          ~cmp:(fun (name1, _, _) (name2, _, _) -> String.compare name1 name2))
       ~f:(fun (name, default, description) ->
         concat ("\n"
                 :: name :: " (default " :: Sexp.to_string default :: ")"
                 :: description)))
;;

let help_message () =
  concat [
    "\
The "; environment_variable;" environment variable affects async
in various ways.  Its value should be a sexp of the following form,
where all fields are optional:

";
    Sexp.to_string_hum (sexp_of_t example)
;"

Here is an explanation of each field.
";
    field_descriptions ();
  ]
;;

let usage () = eprintf "%s%!" (help_message ()); exit 1

let t =
  match Sys.getenv environment_variable with
  | None -> empty
  | Some "" -> usage ()
  | Some string ->
    match Result.try_with (fun () -> t_of_sexp (Sexp.of_string (String.strip string))) with
    | Ok t -> t
    | Error exn ->
      eprintf "%s\n\n"
        (Sexp.to_string_hum
           (Error.sexp_of_t
              (Error.create (sprintf "invalid value for %s environment variable"
                               environment_variable)
                 exn <:sexp_of< exn >>)));
      usage ();
;;

module Print_debug_messages_for = struct

  let print_debug_messages_for tag =
    match t.print_debug_messages_for with
    | None -> false
    | Some l -> List.mem l tag
  ;;

  let all = print_debug_messages_for Debug_tag.All

  let debug tag = all || print_debug_messages_for tag

  let clock              = debug Debug_tag.Clock
  let fd                 = debug Debug_tag.Fd
  let file_descr_watcher = debug Debug_tag.File_descr_watcher
  let finalizers         = debug Debug_tag.Finalizers
  let interruptor        = debug Debug_tag.Interruptor
  let monitor            = debug Debug_tag.Monitor
  let monitor_send_exn   = debug Debug_tag.Monitor_send_exn
  let parallel           = debug Debug_tag.Parallel
  let reader             = debug Debug_tag.Reader
  let scheduler          = debug Debug_tag.Scheduler
  let shutdown           = debug Debug_tag.Shutdown
  let thread_pool        = debug Debug_tag.Thread_pool
  let thread_safe        = debug Debug_tag.Thread_safe
  let writer             = debug Debug_tag.Writer

end

let default field =
  Option.value (Field.get field t)
    ~default:(Option.value_exn (Field.get field default))
;;

let abort_after_thread_pool_stuck_for   = default Fields.abort_after_thread_pool_stuck_for
let check_invariants                    = default Fields.check_invariants
let detect_invalid_access_from_thread   = default Fields.detect_invalid_access_from_thread
let epoll_max_ready_events              = default Fields.epoll_max_ready_events
let file_descr_watcher                  = default Fields.file_descr_watcher
let max_inter_cycle_timeout             = default Fields.max_inter_cycle_timeout
let max_num_open_file_descrs            = default Fields.max_num_open_file_descrs
let max_num_threads                     = default Fields.max_num_threads
let max_num_jobs_per_priority_per_cycle = default Fields.max_num_jobs_per_priority_per_cycle
let record_backtraces                   = default Fields.record_backtraces
let report_thread_pool_stuck_for        = default Fields.report_thread_pool_stuck_for
let timing_wheel_config                 = default Fields.timing_wheel_config

let t =
  { abort_after_thread_pool_stuck_for   = Some abort_after_thread_pool_stuck_for
  ; check_invariants                    = Some check_invariants
  ; detect_invalid_access_from_thread   = Some detect_invalid_access_from_thread
  ; epoll_max_ready_events              = Some epoll_max_ready_events
  ; file_descr_watcher                  = Some file_descr_watcher
  ; max_inter_cycle_timeout             = Some max_inter_cycle_timeout
  ; max_num_open_file_descrs            = Some max_num_open_file_descrs
  ; max_num_threads                     = Some max_num_threads
  ; max_num_jobs_per_priority_per_cycle = Some max_num_jobs_per_priority_per_cycle
  ; print_debug_messages_for            = t.print_debug_messages_for
  ; record_backtraces                   = Some record_backtraces
  ; report_thread_pool_stuck_for        = Some report_thread_pool_stuck_for
  ; timing_wheel_config             = Some timing_wheel_config
  }
;;
