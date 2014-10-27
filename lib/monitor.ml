open Core.Std
open Import  let _ = _squelch_unused_module_warning_
open Deferred_std

module Scheduler = Raw_scheduler
module Stream = Tail.Stream

module Monitor = Raw_monitor
include Monitor

type monitor = t with sexp_of

let current_execution_context () = Scheduler.(current_execution_context (t ()))

let current () = Execution_context.monitor (current_execution_context ())

let depth t =
  let rec loop t n =
    match t.parent with
    | None -> n
    | Some t -> loop t (n + 1)
  in
  loop t 0
;;

type 'a with_optional_monitor_name =
  ?here : Source_code_position.t
  -> ?info : Info.t
  -> ?name : string
  -> 'a

let detach t = t.is_detached <- true

(* After [add_handler_for_all_errors t f], [f] runs in the middle of [send_exn], in an
   arbitrary execution context.  So, [f] should not depend on the current execution
   context.  If [f] needs to do anything that does depend on the execution context, it
   must explicitly run within the desired execution context.  [f] also runs in the middle
   of [Bag.iter t.handlers_for_all_errors], so [f] should not side effect
   [t.handlers_for_all_errors]. *)
let add_handler_for_all_errors t ~f = Bag.add t.handlers_for_all_errors f

type handler_state =
  | Uninitialized
  | Running of (exn -> unit) Bag.Elt.t
  | Terminated

let detach_and_iter_errors t ~f =
  detach t;
  let scheduler = Scheduler.t () in
  let execution_context = Scheduler.current_execution_context scheduler in
  let handler_state_ref = ref Uninitialized in
  let run_f exn =
    match !handler_state_ref with
    | Uninitialized -> assert false
    | Terminated -> ()
    | Running bag_elt ->
      try f exn
      with inner_exn ->
        handler_state_ref := Terminated;
        Bag.remove t.handlers_for_all_errors bag_elt;
        (* [run_f] always runs in [execution_context].  Hence, [raise inner_exn] sends
           [inner_exn] to [execution_context]'s monitor, i.e. the monitor in effect when
           [detach_and_iter_errors] was called. *)
        raise inner_exn
  in
  handler_state_ref :=
    Running (add_handler_for_all_errors t ~f:(fun exn ->
      Scheduler.enqueue scheduler execution_context run_f exn));
;;

let detach_and_get_error_stream t =
  detach t;
  let tail = Tail.create () in
  ignore (add_handler_for_all_errors t ~f:(fun exn -> Tail.extend tail exn)
          : _ Bag.Elt.t);
  Tail.collect tail
;;

let get_next_error t =
  Deferred.create (fun ivar ->
    t.handlers_for_next_error <-
      (fun exn -> Ivar.fill ivar exn)
      :: t.handlers_for_next_error)
;;

let detach_and_get_next_error t =
  detach t;
  get_next_error t
;;

let create ?here ?info ?name () =
  let parent = current () in
  create_with_parent ?here ?info ?name (Some parent);
;;

module Exn_for_monitor = struct
  type t =
    { exn               : exn
    ; backtrace         : string sexp_list
    ; backtrace_history : Backtrace.t sexp_list
    ; monitor           : Monitor.t
    }
  with fields, sexp_of
end

exception Error_ of Exn_for_monitor.t with sexp

let extract_exn exn =
  match exn with
  | Error_ error -> error.exn
  | exn -> exn
;;

let send_exn t ?backtrace exn =
  let backtrace =
    let split backtrace = String.split backtrace ~on:'\n' in
    match backtrace with
    | None -> []
    | Some `Get -> split (Exn.backtrace ())
    | Some (`This b) -> split b
  in
  let backtrace_history = (current_execution_context ()).backtrace_history in
  let exn =
    match exn with
    | Error_ _ -> exn
    | _ -> Error_ { Exn_for_monitor. exn; backtrace; backtrace_history; monitor = t }
  in
  if Debug.monitor_send_exn
  then Debug.log "Monitor.send_exn" (t, exn) <:sexp_of< t * exn >>;
  t.has_seen_error <- true;
  let rec loop t =
    List.iter t.handlers_for_next_error ~f:(fun f -> f exn);
    t.handlers_for_next_error <- [];
    if t.is_detached then begin
      if Debug.monitor_send_exn
      then Debug.log "Monitor.send_exn found listening monitor" (t, exn)
             <:sexp_of< t * exn >>;
      Bag.iter t.handlers_for_all_errors ~f:(fun f -> f exn);
    end else
      match t.parent with
      | Some t' -> loop t'
      | None ->
        (* Ignore shutdown errors that reach the top. *)
        if exn <> Shutdown then begin
          (* Do not change this branch to print the exception or to exit.  Having the
             scheduler raise an uncaught exception is the necessary behavior for programs
             that call [Scheduler.go] and want to handle it. *)
          Scheduler.(got_uncaught_exn (t ()))
            (Error.create "unhandled exception" (exn, `Pid (Unix.getpid ()))
               <:sexp_of< exn * [ `Pid of Pid.t ] >>)
        end;
  in
  loop t
;;

module Exported_for_scheduler = struct
  let within_context context f =
    Scheduler.(with_execution_context (t ())) context
      ~f:(fun () ->
        match Result.try_with f with
        | Ok x -> Ok x
        | Error exn ->
          send_exn (Execution_context.monitor context) exn ~backtrace:`Get;
          Error ())
  ;;

  type 'a with_options =
    ?monitor:t
    -> ?priority:Priority.t
    -> 'a

  let within_gen ?monitor ?priority f =
    let tmp_context =
      Execution_context.create_like (current_execution_context ())
        ?monitor ?priority
    in
    within_context tmp_context f
  ;;

  let within'        ?monitor ?priority f =
    match within_gen ?monitor ?priority f with
    | Error () -> Deferred.never ()
    | Ok d -> d
  ;;

  let within_v       ?monitor ?priority f =
    match within_gen ?monitor ?priority f with
    | Error () -> None
    | Ok x -> Some x
  ;;

  let within         ?monitor ?priority f =
    match within_gen ?monitor ?priority f with
    | Error () -> ()
    | Ok () -> ()
  ;;

  let schedule ?monitor ?priority work =
    let scheduler = Scheduler.t () in
    Scheduler.enqueue scheduler
      (Execution_context.create_like (Scheduler.current_execution_context scheduler)
         ?monitor ?priority)
      work ()
  ;;

  let schedule' ?monitor ?priority work =
    Deferred.create (fun i ->
      schedule  ?monitor ?priority
        (fun () -> upon (work ()) (fun a -> Ivar.fill i a)))
  ;;

  let preserve_execution_context f =
    let scheduler = Scheduler.t () in
    let execution_context = Scheduler.current_execution_context scheduler in
    stage (fun a -> Raw_scheduler.enqueue scheduler execution_context f a)
  ;;

  let preserve_execution_context' f =
    let scheduler = Scheduler.t () in
    let execution_context = Scheduler.current_execution_context scheduler in
    let call_and_fill (f, a, i) = upon (f a) (fun r -> Ivar.fill i r) in
    stage (fun a ->
      Deferred.create (fun i ->
        Raw_scheduler.enqueue scheduler
          execution_context call_and_fill (f, a, i)))
  ;;

end

open Exported_for_scheduler

let stream_iter stream ~f =
  let rec loop stream =
    Stream.next stream
    >>> function
    | Nil -> ()
    | Cons (v, stream) -> loop stream; f v
  in
  loop stream
;;

let try_with_rest_handling = ref (`Default `Ignore)

let try_with_ignored_exn_handling = ref `Ignore

let internal_try_with_handle_errors ?rest errors monitor =
  let rest =
    match !try_with_rest_handling with
    | `Default default -> Option.value rest ~default
    | `Force rest -> rest
  in
  match rest with
  | `Raise ->
    stream_iter errors ~f:(fun e -> send_exn (current ()) e ?backtrace:None);
  | `Ignore ->
    match !try_with_ignored_exn_handling with
    | `Ignore -> ()
    | `Eprintf | `Run _ as x ->
      stream_iter errors ~f:(fun exn ->
        match x with
        | `Run f -> f exn
        | `Eprintf ->
          Debug.log "try_with ignored exception" (exn, monitor) <:sexp_of< exn * t >>);
;;

let try_with ?here ?info
      ?(name = "try_with")
      ?extract_exn:(do_extract_exn = false)
      ?(run = `Schedule)
      ?rest
      f =
  (* Because we call [detach_and_get_error_stream monitor] and deal with the errors
     explicitly, [monitor] does not need a parent; thus [send_exn] would never propagate
     an exn past [monitor]. *)
  let monitor = create_with_parent ?here ?info ~name None in
  let errors = detach_and_get_error_stream monitor in
  let d =
    match run with
    | `Now      -> within' ~monitor f
    | `Schedule -> schedule' ~monitor f
  in
  match Deferred.peek d with
  | Some a -> (internal_try_with_handle_errors ?rest errors monitor; return (Ok a))
  | None ->
    choose [ choice d (fun a -> (Ok a, errors))
           ; choice (Stream.next errors)
               (function
                 | Nil -> assert false
                 | Cons (err, errors) ->
                   let err = if do_extract_exn then extract_exn err else err in
                   (Error err, errors));
           ]
    >>| fun (res, errors) ->
    internal_try_with_handle_errors ?rest errors monitor;
    res
;;

let protect ?here ?info ?(name = "Monitor.protect") f ~finally =
  try_with ?here ?info ~name f
  >>= fun r ->
  try_with ?here ?info ~name:"finally" finally
  >>| fun fr ->
  match r, fr with
  | Error e, Error finally_e  ->
    failwiths "Async finally" (e, finally_e) <:sexp_of< exn * exn >>
  | Error e        , Ok ()
  | Ok _           , Error e -> raise e
  | Ok r           , Ok ()   -> r
;;

let handle_errors ?here ?info ?name f handler =
  let monitor = create ?here ?info ?name () in
  stream_iter (detach_and_get_error_stream monitor) ~f:handler;
  within' ~monitor f;
;;

let catch_stream ?here ?info ?name f =
  let monitor = create ?here ?info ?name () in
  let stream = detach_and_get_error_stream monitor in
  within ~monitor f;
  stream
;;

let catch ?here ?info ?name f =
  Stream.next (catch_stream ?here ?info ?name f)
  >>| function
  | Cons (x, _) -> x
  | Nil -> failwith "Monitor.catch got unexpected empty stream"
;;

let is_alive t = Scheduler.monitor_is_alive (Scheduler.t ()) t

let kill t = Scheduler.kill_monitor (Scheduler.t ()) t
