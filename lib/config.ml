open Core.Std

let concat = String.concat

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
    | Run_job
    | Scheduler
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
      Run_job;
      Scheduler;
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
  { abort_after_thread_pool_stuck_for : Time.Span.t               sexp_option;
    alarm_precision                   : Time.Span.t               sexp_option;
    check_invariants                  : bool                      sexp_option;
    detect_invalid_access_from_thread : bool                      sexp_option;
    epoll_max_ready_events            : int                       sexp_option;
    file_descr_watcher                : File_descr_watcher.t      sexp_option;
    max_num_open_file_descrs          : int                       sexp_option;
    max_num_threads                   : int                       sexp_option;
    print_debug_messages_for          : Debug_tag.t list          sexp_option;
    record_backtraces                 : bool                      sexp_option;
    report_thread_pool_stuck_for      : Time.Span.t               sexp_option;
    timing_wheel_level_bits           : Timing_wheel.Level_bits.t sexp_option;
  }
with fields, sexp

let empty =
  { abort_after_thread_pool_stuck_for = None;
    alarm_precision                   = None;
    check_invariants                  = None;
    detect_invalid_access_from_thread = None;
    epoll_max_ready_events            = None;
    file_descr_watcher                = None;
    max_num_open_file_descrs          = None;
    max_num_threads                   = None;
    print_debug_messages_for          = None;
    record_backtraces                 = None;
    report_thread_pool_stuck_for      = None;
    timing_wheel_level_bits           = None;
  }
;;

let default_file_descr_watcher, default_max_num_open_file_descrs =
  if Result.is_ok Linux_ext.Timerfd.create
  then File_descr_watcher.Epoll, 8192
  else File_descr_watcher.Select, 1024
;;

let default_alarm_precision_and_level_bits word_size =
  let module W = Word_size in
  match word_size with
  | W.W32 -> Time.Span.of_ms 1.,  Timing_wheel.Level_bits.create_exn [ 10; 10; 9;   ]
  | W.W64 -> Time.Span.of_ms 0.1, Timing_wheel.Level_bits.create_exn [ 15; 15; 9; 6 ]
;;

TEST_UNIT =
  let module L = Timing_wheel.Level_bits in
  let module W = Word_size in
  List.iter [ W.W32; W.W64 ] ~f:(fun word_size ->
    let alarm_precision, level_bits = default_alarm_precision_and_level_bits word_size in
    let actual_durations =
      Timing_wheel.Level_bits.durations level_bits ~alarm_precision
    in
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

let default_alarm_precision, default_timing_wheel_level_bits =
  default_alarm_precision_and_level_bits Word_size.word_size
;;

let default =
  { abort_after_thread_pool_stuck_for = Some (sec 60.)                       ;
    alarm_precision                   = Some default_alarm_precision         ;
    check_invariants                  = Some false                           ;
    detect_invalid_access_from_thread = Some false                           ;
    epoll_max_ready_events            = Some 256                             ;
    file_descr_watcher                = Some default_file_descr_watcher      ;
    max_num_open_file_descrs          = Some default_max_num_open_file_descrs;
    max_num_threads                   = Some 50                              ;
    print_debug_messages_for          = Some []                              ;
    record_backtraces                 = Some false                           ;
    report_thread_pool_stuck_for      = Some (sec 1.)                        ;
    timing_wheel_level_bits           = Some default_timing_wheel_level_bits ;
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
      ~alarm_precision:(field <:sexp_of< Time.Span.t >>
                          ["
  The precision of alarms in Async's clock.  Time is split into intervals of this size,
  and alarms with times in the same interval fire in the same cycle.
"
                          ])
      ~check_invariants:(field <:sexp_of< bool >>
                           ["
  If true, causes async to regularly check invariants of its internal data structures.
  This can substantially slow down your program.
"
                           ])
      ~detect_invalid_access_from_thread:(field <:sexp_of< bool >>
                                            ["
  If true, causes async routines to check if they are being accessed from some thread
  other than the thread currently holding the async lock, which is not allowed and can
  lead to very confusing behavior.
"
                                            ])

      ~epoll_max_ready_events:(field <:sexp_of< int >>
                                 ["
  The maximum number of ready events that async's call to [Epoll.wait] will handle.
"
                                 ])
      ~file_descr_watcher:(field <:sexp_of< File_descr_watcher.t >>
                             ["
  This determines what OS subsystem async uses to watch file descriptors for being ready.
  Allowed values are: ";
                              concat ~sep:", "
                                (List.map File_descr_watcher.list
                                   ~f:File_descr_watcher.to_string);
                              ".
"
                             ])
      ~max_num_open_file_descrs:(field <:sexp_of< int >>
                                   ["
  The maximum number of open file descriptors allowed at any one time.
"
                                   ])
      ~max_num_threads:(field <:sexp_of< int >>
                          ["
  The maximum number of threads that async will create to do blocking system calls
  and handle calls to [In_thread.run].
"
                          ])
      ~print_debug_messages_for:(field <:sexp_of< Debug_tag.t list >>
                                   ["
  A list of tags specifying which async functions should print debug messages to stderr.
  Each tag identifies a group of related async functions.  The tag 'all' means
  to print debug messages for all functions.  Allowed values are:
";
                                    concat (List.map Debug_tag.list ~f:(fun d ->
                                      concat ["    "; Debug_tag.to_string d; "\n"]));
                                    "\
  Turning on debug messages will substantially slow down most programs.
"
                                   ])
      ~record_backtraces:(field <:sexp_of< bool >>
                            ["
  If true, this will cause async to keep in the execution context the history of stack
  backtraces (obtained via [Backtrace.get]) that led to the current job.  If an async job
  has an unhandled exception, this backtrace history will be recorded in the exception.
  In particular the history will appean in an unhandled exception that reaches the main
  monitor.  This can have a substantial performance impact, both in running time and
  space usage.
"
                            ])
      ~report_thread_pool_stuck_for:(field <:sexp_of< Time.Span.t >>
                                       ["
  By default, async will print a message to stderr every second if the thread pool is
  stuck for longer than this.
"
                                       ])
      ~timing_wheel_level_bits:(field <:sexp_of< Timing_wheel.Level_bits.t >>
                                  ["
  This is used to adjust the time/space tradeoff in the timing wheel used to implement
  async's clock.  Level [i] in the timing wheel has an array of size [2^b], where [b] is
  the [i]'th entry in [timing_wheel_level_bits].
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
The "; environment_variable;" environment variable affects async in various ways.  Its
value should be a sexp of the following form, where all fields are optional:

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
  let run_job            = debug Debug_tag.Run_job
  let scheduler          = debug Debug_tag.Scheduler
  let thread_pool        = debug Debug_tag.Thread_pool
  let thread_safe        = debug Debug_tag.Thread_safe
  let writer             = debug Debug_tag.Writer

end

let default field =
  Option.value (Field.get field t) ~default:(Option.value_exn (Field.get field default))
;;

let abort_after_thread_pool_stuck_for = default Fields.abort_after_thread_pool_stuck_for
let alarm_precision                   = default Fields.alarm_precision
let check_invariants                  = default Fields.check_invariants
let detect_invalid_access_from_thread = default Fields.detect_invalid_access_from_thread
let epoll_max_ready_events            = default Fields.epoll_max_ready_events
let file_descr_watcher                = default Fields.file_descr_watcher
let max_num_open_file_descrs          = default Fields.max_num_open_file_descrs
let max_num_threads                   = default Fields.max_num_threads
let record_backtraces                 = default Fields.record_backtraces
let report_thread_pool_stuck_for      = default Fields.report_thread_pool_stuck_for
let timing_wheel_level_bits           = default Fields.timing_wheel_level_bits

let t =
  { abort_after_thread_pool_stuck_for = Some abort_after_thread_pool_stuck_for;
    alarm_precision                   = Some alarm_precision                  ;
    check_invariants                  = Some check_invariants                 ;
    detect_invalid_access_from_thread = Some detect_invalid_access_from_thread;
    epoll_max_ready_events            = Some epoll_max_ready_events           ;
    file_descr_watcher                = Some file_descr_watcher               ;
    max_num_open_file_descrs          = Some max_num_open_file_descrs         ;
    max_num_threads                   = Some max_num_threads                  ;
    print_debug_messages_for          = t.print_debug_messages_for            ;
    record_backtraces                 = Some record_backtraces                ;
    report_thread_pool_stuck_for      = Some report_thread_pool_stuck_for     ;
    timing_wheel_level_bits           = Some timing_wheel_level_bits          ;
  }
;;
