open Core.Std
open Import

module Hole = struct
  type 'a t =
    { name : string;
      mutable value : 'a option;
    }
  with sexp_of

  let create ~name = { name; value = None }

  let get_exn t =
    match t.value with
    | Some a -> a
    | None -> failwithf "attempt to get %s before filling it" t.name ()
  ;;

  let fill t a =
    if Option.is_some t.value
    then Or_error.error "attempt to multiply fill" t.name <:sexp_of< string >>
    else (t.value <- Some a; Ok ())
  ;;

  let empty t = t.value <- None;
end

type 'a t =
| Simple of 'a
| Hole of 'a Hole.t
with sexp_of

let create a = Simple a

let of_hole hole = Hole hole

let get_exn = function
  | Simple a -> a
  | Hole hole -> Hole.get_exn hole
;;

TEST_MODULE = struct
  type a = A of b t
  and b = B of a

  let hole = Hole.create ~name:"hole"
  let a = A (of_hole hole)
  let b = B a
  let () = ok_exn (Hole.fill hole b)

  TEST =
    let A backpatch = a in
    phys_equal (get_exn backpatch) b;
  ;;
end

