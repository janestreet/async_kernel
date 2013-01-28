open Core.Std

type 'execution_context t_ =
  { name : Info.t;
    here : Source_code_position.t option;
    id : int;
    parent : 'execution_context t_ option;
    errors : (exn, 'execution_context) Raw_tail.t;
    mutable has_seen_error : bool;
    mutable someone_is_listening : bool;
  }
with fields

module Pretty = struct
  type one =
    { name : Info.t;
      here : Source_code_position.t option;
      id : int;
      has_seen_error : bool;
      someone_is_listening : bool
    }
  with sexp_of

  type t = one list
  with sexp_of
end

let to_pretty =
  let rec loop
      { name; here; id; parent; errors = _; has_seen_error; someone_is_listening }
      ac =
    let ac =
      { Pretty. name; here; id; has_seen_error; someone_is_listening} :: ac
    in
    match parent with
    | None -> List.rev ac
    | Some t -> loop t ac
  in
  fun t -> loop t [];
;;

let sexp_of_t_ _ t = Pretty.sexp_of_t (to_pretty t)

exception Shutdown
