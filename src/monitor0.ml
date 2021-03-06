open Core
open Import

let debug = Debug.monitor

module Forwarding = Types.Forwarding

type t = Types.Monitor.t =
  { name : Info.t
  ; here : Source_code_position.t option
  ; id : int
  ; mutable next_error : exn Types.Ivar.t
  ; (* [Monitor.send_exn] schedules a job for each element of [handlers_for_all_errors]. *)
    mutable handlers_for_all_errors : (Types.Execution_context.t * (exn -> unit)) Bag.t
  ; (* [Monitor.send_exn] extends each tail in [tails_for_all_errors]. *)
    mutable tails_for_all_errors : exn Types.Tail.t list
  ; mutable has_seen_error : bool
  ; mutable forwarding : Forwarding.t
  }
[@@deriving fields]

module Pretty = struct
  type one =
    { name : Info.t
    ; here : Source_code_position.t option
    ; id : int
    ; has_seen_error : bool
    ; is_detached : bool
    }
  [@@deriving sexp_of]

  type t = one list [@@deriving sexp_of]
end

let to_pretty =
  let rec loop
            { name
            ; here
            ; id
            ; forwarding
            ; has_seen_error
            ; next_error = _
            ; handlers_for_all_errors = _
            ; tails_for_all_errors = _
            }
            ac
    =
    let is_detached, parent =
      match forwarding with
      | Detached -> true, None
      | Parent parent -> false, Some parent
      | Report_uncaught_exn -> false, None
    in
    let ac = { Pretty.name; here; id; has_seen_error; is_detached } :: ac in
    match parent with
    | None -> List.rev ac
    | Some t -> loop t ac
  in
  fun t -> loop t []
;;

let sexp_of_t t = Pretty.sexp_of_t (to_pretty t)

let next_id =
  let r = ref 0 in
  fun () ->
    incr r;
    !r
;;

let create_with_parent ?here ?info ?name parent =
  let id = next_id () in
  let name =
    match info, name with
    | Some i, None -> i
    | Some i, Some s -> Info.tag i ~tag:s
    | None, Some s -> Info.of_string s
    | None, None -> Info.create "id" id [%sexp_of: int]
  in
  let t =
    { name
    ; here
    ; forwarding =
        (match parent with
         | None -> Report_uncaught_exn
         | Some parent -> Parent parent)
    ; id
    ; next_error = { cell = Empty }
    ; handlers_for_all_errors = Bag.create ()
    ; tails_for_all_errors = []
    ; has_seen_error = false
    }
  in
  if debug then Debug.log "created monitor" t [%sexp_of: t];
  t
;;

let main = create_with_parent ~name:"main" None
