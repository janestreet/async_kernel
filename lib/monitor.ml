open Core.Std
open Import
open Deferred_std

module Monitor = Raw_monitor
module Scheduler = Raw_scheduler
module Stream = Raw_stream

type 'execution_context t_ = 'execution_context Monitor.t =
  { name_opt : string option;
    id : int;
    parent : 'execution_context t_ option;
    errors : (exn, 'execution_context) Raw_tail.t;
    mutable has_seen_error : bool;
    mutable someone_is_listening : bool;
  }
with fields

type t = Execution_context.t Monitor.t with sexp_of

type monitor = t with sexp_of

let debug = Debug.debug

let current_execution_context () = Scheduler.current_execution_context ()

let current () = (current_execution_context ()).Execution_context.monitor

let name t =
  match t.name_opt with
  | Some s -> s
  | None -> Int.to_string (id t)
;;

let next_id =
  let r = ref 0 in
  fun () -> r := !r + 1; !r
;;

let create_with_parent ?name:name_opt parent =
  let t =
    { name_opt;
      id = next_id ();
      parent;
      errors = Tail.create ();
      has_seen_error = false;
      someone_is_listening = false;
    }
  in
  if debug then Debug.log "created monitor" t <:sexp_of< t >>;
  t
;;

let main = create_with_parent ~name:"main" None

let errors t =
  t.someone_is_listening <- true;
  Tail.collect t.errors;
;;

let error t =
  let module S = Stream in
  S.next (Tail.collect t.errors)
  >>| function
  | S.Nil -> assert false
  | S.Cons (error, _) -> error
;;

let create ?name () =
  let parent = current () in
  create_with_parent ?name (Some parent);
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
  let is_shutdown = exn = Monitor.Shutdown in
  let exn =
    match exn with
    | Error_ _ -> exn
    | _ -> Error_ { Exn_for_monitor. exn; backtrace; backtrace_history; monitor = t }
  in
  t.has_seen_error <- true;
  let rec loop t =
    if t.someone_is_listening then
      Tail.extend t.errors exn
    else
      match t.parent with
      | Some t' -> loop t'
      | None ->
        (* Ignore shutdown errors that reach the top. *)
        if not is_shutdown then
          (* Do not change this branch to print the exception or to exit.  Having the
             scheduler raise an uncaught exception is the necessary behavior for programs
             that call [Scheduler.go] and want to handle it. *)
          Scheduler.t.Scheduler.uncaught_exception <-
            Some (Error.create "unhandled exception" (`Pid (Unix.getpid ()), exn)
                    (<:sexp_of< [ `Pid of Pid.t ] * exn >>));
  in
  loop t
;;

module Exported_for_scheduler = struct
  let within_context context f =
    Scheduler.with_execution_context context
      ~f:(fun () ->
        match Result.try_with f with
        | Ok x -> Ok x
        | Error exn ->
          send_exn context.Execution_context.monitor exn ~backtrace:`Get;
          Error ())
  ;;

  type 'a with_options =
    ?block_group:Block_group.t
    -> ?monitor:t
    -> ?priority:Priority.t
    -> 'a

  let within_gen ?block_group ?monitor ?priority f =
    let tmp_context =
      Execution_context.create_like (current_execution_context ())
        ?block_group ?monitor ?priority
    in
    within_context tmp_context f
  ;;

  let within'        ?block_group ?monitor ?priority f =
    match within_gen ?block_group ?monitor ?priority f with
    | Error () -> Deferred.never ()
    | Ok d -> d
  ;;

  let within_v       ?block_group ?monitor ?priority f =
    match within_gen ?block_group ?monitor ?priority f with
    | Error () -> None
    | Ok x -> Some x
  ;;

  let within         ?block_group ?monitor ?priority f =
    match within_gen ?block_group ?monitor ?priority f with
    | Error () -> ()
    | Ok () -> ()
  ;;

  let schedule ?block_group ?monitor ?priority work =
    Scheduler.add_job
      (Job.create
         (Execution_context.create_like (current_execution_context ())
            ?block_group ?monitor ?priority)
         work ())
  ;;

  let schedule' ?block_group ?monitor ?priority work =
    Deferred.create (fun i ->
      schedule  ?block_group ?monitor ?priority
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

let try_with ?(name="try_with") ~reraise f =
  let module S = Stream in
  let parent = current () in
  let monitor = create_with_parent ~name (Some parent) in
  let errors = errors monitor in
  choose [ choice (schedule' ~monitor f)
             (fun x -> (Ok x, errors));
           choice (S.next errors)
             (function
             | S.Nil -> assert false
             | S.Cons (err, errors) -> (Error err, errors));
         ]
  >>| fun (res, errors) ->
  if reraise then stream_iter errors ~f:(fun e -> send_exn parent e ?backtrace:None);
  res
;;

let try_with_raise_rest ?name f = try_with ?name ~reraise:true  f
let try_with            ?name f = try_with ?name ~reraise:false f

let protect ?(name = "Monitor.protect") f ~finally =
  try_with ~name f
  >>= fun r ->
  try_with ~name:(name ^ "::finally") finally
  >>| fun fr ->
  match r, fr with
  | Error e, Error finally_e  ->
    failwiths "Async finally" (e, finally_e) <:sexp_of< exn * exn >>
  | Error e        , Ok ()
  | Ok _           , Error e -> raise e
  | Ok r           , Ok ()   -> r
;;

let handle_errors ?name f handler =
  let monitor = create ?name () in
  stream_iter (errors monitor) ~f:handler;
  within' ~monitor f;
;;

let catch_stream ?name f =
  let monitor = create ?name () in
  within ~monitor f;
  errors monitor;
;;

let catch ?name f =
  let module S = Stream in
  S.next (catch_stream ?name f)
  >>| function
  | S.Cons (x, _) -> x
  | S.Nil -> failwith "Monitor.catch got unexpected empty stream"
;;
