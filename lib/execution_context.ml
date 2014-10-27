open Core.Std

module Monitor = Raw_monitor

type t =
  { monitor            : Monitor.t
  ; priority           : Priority.t
  ; local_storage      : Univ_map.t
  ; backtrace_history  : Backtrace.t list
  ; mutable kill_index : Kill_index.t
  }
with fields, sexp_of

let invariant (_ : t) = ()

let main =
  { monitor           = Monitor.main
  ; priority          = Priority.normal
  ; local_storage     = Univ_map.empty
  ; backtrace_history = []
  ; kill_index        = Kill_index.initial
  }
;;

let create_like ?monitor ?priority ?local_storage t =
  let monitor = Option.value monitor ~default:t.monitor in
  { monitor
  ; priority          = Option.value priority ~default:t.priority
  ; local_storage     = Option.value local_storage ~default:t.local_storage
  ; backtrace_history = t.backtrace_history
  ; kill_index        = Monitor.kill_index monitor
  }
;;

let find_local t key = Univ_map.find t.local_storage key

let with_local t key data =
  { t with local_storage = Univ_map.change t.local_storage key (fun _ -> data) }
;;

let record_backtrace =
  match Backtrace.get with
  | Error _ -> Fn.id
  | Ok get -> fun t -> { t with backtrace_history = get () :: t.backtrace_history }
;;

let is_alive t ~global_kill_index =
  Kill_index.equal t.kill_index global_kill_index
  || (not (Kill_index.equal t.kill_index Kill_index.dead)
      && let b = Monitor.is_alive t.monitor ~global_kill_index in
      t.kill_index <- t.monitor.kill_index;
      b)
;;
