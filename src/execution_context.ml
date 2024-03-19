open! Core
open! Import
module Monitor = Monitor0

type t = Types.Execution_context.t =
  { monitor : Monitor.t
  ; priority : Priority.t
  ; local_storage : Univ_map.t
  ; backtrace_history : Backtrace.t list
  }
[@@deriving fields ~getters, sexp_of]

let invariant (_ : t) = ()

let main =
  { monitor = Monitor.main
  ; priority = Priority.normal
  ; local_storage = Univ_map.empty
  ; backtrace_history = []
  }
;;

let create_like ?monitor ?priority ?local_storage t =
  match monitor, priority, local_storage with
  | None, None, None ->
    (* avoid allocating in the trivial case, which e.g. happens when calling
       [Async.schedule] without optional args *)
    t
  | _ ->
    let monitor = Option.value monitor ~default:t.monitor in
    { monitor
    ; priority = Option.value priority ~default:t.priority
    ; local_storage = Option.value local_storage ~default:t.local_storage
    ; backtrace_history = t.backtrace_history
    }
;;

let find_local t key = Univ_map.find t.local_storage key

let with_local t key data =
  match data with
  | Some data -> { t with local_storage = Univ_map.set t.local_storage ~key ~data }
  | None -> { t with local_storage = Univ_map.remove t.local_storage key }
;;

let record_backtrace t =
  { t with backtrace_history = Backtrace.get () :: t.backtrace_history }
;;
