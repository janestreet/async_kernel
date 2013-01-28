open Core.Std
open Import

module Tail = Raw_tail

type 'execution_context t =
  { name_opt : string option;
    id : int;
    parent : 'execution_context t option;
    errors : (exn, 'execution_context) Tail.t;
    mutable has_seen_error : bool;
    mutable someone_is_listening : bool;
  }
with fields

let bogus () =
  { name_opt = None;
    id = -1;
    parent = None;
    errors = Tail.create ();
    has_seen_error = false;
    someone_is_listening = false;
  }
;;

module Pretty = struct
  type one =
    { name_opt : string option;
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
      { name_opt; id; parent; errors = _; has_seen_error; someone_is_listening }
      ac =
    let ac = { Pretty. name_opt; id; has_seen_error; someone_is_listening } :: ac in
    match parent with
    | None -> List.rev ac
    | Some t -> loop t ac
  in
  fun t -> loop t [];
;;

let sexp_of_t _ t = Pretty.sexp_of_t (to_pretty t)

exception Shutdown
