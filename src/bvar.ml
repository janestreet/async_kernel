open! Core_kernel
open! Import

type 'a t = 'a Types.Bvar.t =
  { mutable has_any_waiters : bool
  ; mutable ivar            : 'a Ivar.t }
[@@deriving fields, sexp_of]

let invariant invariant_a t =
  Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~has_any_waiters:(check (fun has_any_waiters ->
        if Ivar.has_handlers t.ivar
        then (assert has_any_waiters)))
      ~ivar:(check (fun ivar ->
        Ivar.invariant invariant_a ivar;
        assert (Ivar.is_empty ivar))))
;;

let sexp_of_t _ { has_any_waiters; ivar = _ } =
  (* We don't show [ivar] because it's always empty. *)
  [%message (has_any_waiters : bool)]
;;

include Scheduler1.Bvar

let broadcast t a =
  if t.has_any_waiters
  then (
    t.has_any_waiters <- false;
    Ivar.fill t.ivar a;
    t.ivar <- Ivar.create ())
;;

let wait t =
  t.has_any_waiters <- true;
  Ivar.read t.ivar;
;;
