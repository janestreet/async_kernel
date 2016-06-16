open! Core_kernel.Std
open! Import
open! Deferred_std

type ('a, 'phantom) t =
  { (* the use of [sexp_opaque] in [dummy]'s type is essential to avoid a segfault, due to
       the use of [Obj.magic]. *)
    dummy                   : 'a sexp_opaque
  ; mutable current_value   : 'a
  ; taken                   : unit Bvar.t
  ; mutable value_available : unit Ivar.t }
[@@deriving fields, sexp_of]

let value_available t = Ivar.read     t.value_available
let is_empty        t = Ivar.is_empty t.value_available

let sexp_of_t sexp_of_a _ t =
  let sexp_of_a =
    (* We avoid using [sexp_of_a] if [is_empty t], because in that case
       [current_value |> [%sexp_of: a]] may segfault. *)
    if is_empty t
    then (fun _ -> "<empty>" |> [%sexp_of: string])
    else sexp_of_a
  in
  t |> [%sexp_of: (a, _) t]
;;

let invariant invariant_a _ t =
  Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~dummy:ignore
      ~current_value:(check (fun current_value ->
        if is_empty t
        then assert (phys_equal current_value t.dummy)
        else invariant_a current_value))
      ~taken:(check (Bvar.invariant Unit.invariant))
      ~value_available:ignore)
;;

let peek t =
  if is_empty t
  then None
  else Some t.current_value
;;

let peek_exn t =
  if is_empty t then failwith "Mvar.peek_exn called on empty mvar";
  t.current_value
;;

let sexp_of_t sexp_of_a _ t = [%sexp (peek t : a option)]

module Read_write = struct
  type nonrec 'a t = ('a, read_write) t [@@deriving sexp_of]

  let invariant invariant_a t = invariant invariant_a ignore t
end

module Read_only = struct
  type nonrec 'a t = ('a, read) t [@@deriving sexp_of]

  let invariant invariant_a t = invariant invariant_a ignore t
end

let read_only (t : _ Read_write.t) = (t :> _ Read_only.t)

(* we use [Obj.magic] here to avoid allocating whenever [t.current_value] is updated by
   [value_available].  We use [t.value_available] as a guard to avoid ever exposing
   [t.dummy] to the outside world. *)
let create () =
  let dummy = Obj.magic 0 in
    { dummy
    ; current_value   = dummy
    ; taken           = Bvar.create ()
    ; value_available = Ivar.create () }
;;

let take_nonempty t =
  assert (not (is_empty t));
  Bvar.broadcast t.taken ();
  let r = t.current_value in
  t.current_value   <- t.dummy;
  t.value_available <- Ivar.create ();
  r
;;

let take_exn t =
  if is_empty t then failwith "Mvar.take_exn called on empty mvar";
  take_nonempty t;
;;

let take t =
  if is_empty t
  then None
  else Some (take_nonempty t)
;;

let set t v =
  t.current_value <- v;
  Ivar.fill_if_empty t.value_available ();
;;

let update     t ~f = set t (f (peek     t))
let update_exn t ~f = set t (f (peek_exn t))

let taken t = Bvar.wait t.taken

let rec put t v =
  if is_empty t
  then begin
    set t v;
    Deferred.unit
  end else begin
    taken t
    >>= fun () ->
    put t v
  end
;;
