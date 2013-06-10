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

type 'a with_optional_monitor_name =
  ?here : Source_code_position.t
  -> ?info : Info.t
  -> ?name : string
  -> 'a

let add_error_handler t f = t.error_handlers <- f :: t.error_handlers

let errors t =
  t.someone_is_listening <- true;
  let tail = Tail.create () in
  add_error_handler t (fun exn -> Tail.extend tail exn);
  Tail.collect tail
;;

let error t =
  let module S = Stream in
  Deferred.create (fun ivar ->
    add_error_handler t (fun exn -> Ivar.fill_if_empty ivar exn));
;;

let create ?here ?info ?name () =
  let parent = current () in
  create_with_parent ?here ?info ?name (Some parent);
;;

module Exn_for_monitor = struct
  type t =
    { exn : exn;
      backtrace : string sexp_list;
      backtrace_history : Backtrace.t sexp_list;
      monitor : monitor;
    }
  with fields, sexp_of
end

exception Error_ of Exn_for_monitor.t with sexp

let extract_exn exn =
  match exn with
  | Error_ error -> error.Exn_for_monitor.exn
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
  let backtrace_history =
    (current_execution_context ()).Execution_context.backtrace_history
  in
  let exn =
    match exn with
    | Error_ _ -> exn
    | _ -> Error_ { Exn_for_monitor. exn; backtrace; backtrace_history; monitor = t }
  in
  t.has_seen_error <- true;
  let rec loop t =
    if t.someone_is_listening then
      List.iter t.error_handlers ~f:(fun f -> f exn)
    else
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
               (<:sexp_of< exn * [ `Pid of Pid.t ] >>))
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
    Scheduler.add_job scheduler
      (Job.create
         (Execution_context.create_like (Scheduler.current_execution_context scheduler)
            ?monitor ?priority)
         work ())
  ;;

  let schedule' ?monitor ?priority work =
    Deferred.create (fun i ->
      schedule  ?monitor ?priority
        (fun () -> upon (work ()) (fun a -> Ivar.fill i a)))
  ;;
end

open Exported_for_scheduler

let stream_iter stream ~f =
  let rec loop stream =
    let module S = Stream in
    S.next stream
    >>> function
    | S.Nil -> ()
    | S.Cons (v, stream) -> loop stream; f v
  in
  loop stream
;;

let try_with ?here ?info
    ?(name = "try_with")
    ?extract_exn:(do_extract_exn = false)
    ?(run = `Schedule)
    ?(rest = `Ignore) fct =
  let module S = Stream in
  let parent = current () in
  let monitor = create_with_parent ?here ?info ~name (Some parent) in
  let errors = errors monitor in
  let f =
    match run with
    | `Now      -> within'   ~monitor fct
    | `Schedule -> schedule' ~monitor fct
  in
  choose [ choice f (fun x -> (Ok x, errors));
           choice (S.next errors)
             (function
             | S.Nil -> assert false
             | S.Cons (err, errors) ->
               let err = if do_extract_exn then extract_exn err else err in
               (Error err, errors));
         ]
  >>| fun (res, errors) ->
  begin match rest with
  | `Ignore -> ()
  | `Raise -> stream_iter errors ~f:(fun e -> send_exn parent e ?backtrace:None);
  end;
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
  stream_iter (errors monitor) ~f:handler;
  within' ~monitor f;
;;

let catch_stream ?here ?info ?name f =
  let monitor = create ?here ?info ?name () in
  let stream = errors monitor in
  within ~monitor f;
  stream
;;

let catch ?here ?info ?name f =
  let module S = Stream in
  S.next (catch_stream ?here ?info ?name f)
  >>| function
  | S.Cons (x, _) -> x
  | S.Nil -> failwith "Monitor.catch got unexpected empty stream"
;;

let is_alive t = Scheduler.monitor_is_alive (Scheduler.t ()) t

let kill t = Scheduler.kill_monitor (Scheduler.t ()) t
