open Core_kernel.Std
open Import
open Deferred_std

module Deferred = Deferred1
module Scheduler = Scheduler1
module Stream = Tail.Stream

module Monitor = Monitor0
include Monitor

type monitor = t [@@deriving sexp_of]

let invariant t =
  Invariant.invariant [%here] t [%sexp_of: t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~name:ignore
      ~here:ignore
      ~id:ignore
      ~parent:ignore
      ~next_error:(check (fun next_error -> assert (Ivar.is_empty next_error)))
      ~handlers_for_all_errors:ignore
      ~tails_for_all_errors:ignore
      ~has_seen_error:ignore
      ~is_detached:ignore)
;;

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

type handler_state =
  | Uninitialized
  | Running of (Execution_context.t * (exn -> unit)) Bag.Elt.t
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
    Running (Bag.add t.handlers_for_all_errors (execution_context, run_f));
;;

let detach_and_get_error_stream t =
  detach t;
  let tail = Tail.create () in
  t.tails_for_all_errors <- tail :: t.tails_for_all_errors;
  Tail.collect tail
;;

let get_next_error t = Ivar.read t.next_error

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
  [@@deriving fields, sexp_of]
end

exception Error_ of Exn_for_monitor.t [@@deriving sexp]

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
  then Debug.log "Monitor.send_exn" (t, exn) [%sexp_of: t * exn];
  t.has_seen_error <- true;
  let scheduler = Scheduler.t () in
  let rec loop t =
    Ivar.fill t.next_error exn;
    t.next_error <- Ivar.create ();
    if t.is_detached then begin
      if Debug.monitor_send_exn
      then Debug.log "Monitor.send_exn found listening monitor" (t, exn)
             [%sexp_of: t * exn];
      Bag.iter t.handlers_for_all_errors ~f:(fun (execution_context, f) ->
        Scheduler.enqueue scheduler execution_context f exn);
      List.iter t.tails_for_all_errors ~f:(fun tail -> Tail.extend tail exn);
    end else
      match t.parent with
      | Some t' -> loop t'
      | None ->
        (* Ignore shutdown errors that reach the top. *)
        if exn <> Shutdown then begin
          (* Do not change this branch to print the exception or to exit.  Having the
             scheduler raise an uncaught exception is the necessary behavior for programs
             that call [Scheduler.go] and want to handle it. *)
          Scheduler.(got_uncaught_exn (t ())) exn (!Config.task_id ())
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

  let schedule_with_data ?monitor ?priority work x =
    let scheduler = Scheduler.t () in
    Scheduler.enqueue scheduler
      (Execution_context.create_like (Scheduler.current_execution_context scheduler)
         ?monitor ?priority)
      work x
  ;;

  let schedule ?monitor ?priority work = schedule_with_data ?monitor ?priority work ()

  let schedule' =
    (* For performance, we use [schedule_with_data] with a closed function, and inline
       [Deferred.create]. *)
    let upon_work_fill_i (work, i) = upon (work ()) (fun a -> Ivar.fill i a) in
    fun ?monitor ?priority work ->
      let i = Ivar.create () in
      schedule_with_data ?monitor ?priority upon_work_fill_i (work, i);
      Ivar.read i
  ;;

  let preserve_execution_context f =
    let scheduler = Scheduler.t () in
    let execution_context = Scheduler.current_execution_context scheduler in
    stage (fun a -> Scheduler.enqueue scheduler execution_context f a)
  ;;

  let preserve_execution_context' f =
    let scheduler = Scheduler.t () in
    let execution_context = Scheduler.current_execution_context scheduler in
    let call_and_fill (f, a, i) = upon (f a) (fun r -> Ivar.fill i r) in
    stage (fun a ->
      Deferred.create (fun i ->
        Scheduler.enqueue scheduler
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

let internal_try_with_handle_errors ?(rest = `Log) errors =
  match rest with
  | `Raise -> stream_iter errors ~f:(fun e -> send_exn (current ()) e ?backtrace:None);
  | `Log ->
    (* We run [within ~monitor:main] to avoid a space leak when a chain of [try_with]'s
       are run each nested within the previous one.  Without the [within], the error
       handling for the innermost [try_with] would keep alive the entire chain. *)
    within ~monitor:main (fun () ->
      stream_iter errors ~f:(fun exn -> !try_with_log_exn exn))
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
  | Some a -> (internal_try_with_handle_errors ?rest errors; return (Ok a))
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
    internal_try_with_handle_errors ?rest errors;
    res
;;

let try_with_or_error ?here ?info ?(name = "try_with_or_error") ?extract_exn f =
  try_with f ?here ?info ~name ?extract_exn ~run:`Now ~rest:`Log
  >>| Or_error.of_exn_result
;;

let try_with_join_or_error ?here ?info ?(name = "try_with_join_or_error") ?extract_exn f =
  try_with_or_error f ?here ?info ~name ?extract_exn >>| Or_error.join
;;

let protect ?here ?info ?(name = "Monitor.protect") f ~finally =
  try_with ?here ?info ~name f
  >>= fun r ->
  try_with ?here ?info ~name:"finally" finally
  >>| fun fr ->
  match r, fr with
  | Error e, Error finally_e  ->
    failwiths "Async finally" (e, finally_e) [%sexp_of: exn * exn]
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

let catch_error ?here ?info ?name f =
  catch ?here ?info ?name f
  >>| Error.of_exn
;;



