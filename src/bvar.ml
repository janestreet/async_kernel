open! Core
open! Import

type ('a, 'permission) t = ('a, 'permission) Types.Bvar.t

type 'a repr = 'a Types.Bvar.repr =
  { mutable has_any_waiters : bool
  ; mutable ivar : 'a Ivar.t
  }
[@@deriving fields ~iterators:iter, sexp_of]

include (
  Scheduler1.Bvar :
    module type of struct
      include Scheduler1.Bvar
    end
    with type ('a, 'perm) t := ('a, 'perm) t
     and type 'a repr := 'a repr)

let invariant invariant_a _ t =
  let repr = to_repr t in
  Invariant.invariant repr [%sexp_of: _ repr] (fun () ->
    let check f = Invariant.check_field repr f in
    Fields_of_repr.iter
      ~has_any_waiters:
        (check (fun has_any_waiters ->
           if Ivar.has_handlers repr.ivar then assert has_any_waiters))
      ~ivar:
        (check (fun ivar ->
           Ivar.invariant invariant_a ivar;
           assert (Ivar.is_empty ivar))))
;;

let sexp_of_t _ _ t =
  let { has_any_waiters; ivar = _ } = to_repr t in
  (* We don't show [ivar] because it's always empty. *)
  [%message (has_any_waiters : bool)]
;;

let broadcast t a =
  let repr = to_repr t in
  if repr.has_any_waiters
  then (
    repr.has_any_waiters <- false;
    Ivar.fill_exn repr.ivar a;
    repr.ivar <- Ivar.create ())
;;

let wait t =
  let repr = to_repr t in
  repr.has_any_waiters <- true;
  Ivar.read repr.ivar
;;

let has_any_waiters t =
  let repr = to_repr t in
  repr.has_any_waiters
;;
