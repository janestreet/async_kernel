open Core.Std
open Import

module Ivar = Raw_ivar

(* Deferreds present a covariant view of ivars.  We could actually implement deferreds
   using a record of closures, as in the [essence_of_deferred] record below, for which the
   OCaml type checker can infer covariance.  However, doing so would make [Ivar.read] very
   costly, because it would have to allocate lots of closures and a record.  Instead of
   doing this, we make deferreds an abstract covariant type, which concretely is just the
   ivar, and use [Obj.magic] to convert back and forth between a deferred and its concrete
   representation as an ivar.  This [Obj.magic] is safe because the representation is
   always just an ivar, and the covariance follows from the fact that all the deferred
   operations are equivalent to those implemented directly on top of the
   [essence_of_deferred]. *)
type (+'a, 'execution_context) essence_of_deferred =
  { peek : unit -> 'a option;
    is_determined : unit -> bool;
    upon : ('a -> unit) -> unit;
    upon' : ('a -> unit) -> Unregister.t;
    install_removable_handler : ('a, 'execution_context) Raw_handler.t -> Unregister.t;
  }

type (+'a, 'execution_context) t  (* the abstract covariant type, equivalent to ivar *)

let of_ivar (type a) (type execution_context) (ivar : (a, execution_context) Ivar.t) =
  (Obj.magic ivar : (a, execution_context) t)
;;

let to_ivar (type a) (type execution_context) (t : (a, execution_context) t) =
  (Obj.magic t : (a, execution_context) Ivar.t)
;;

let sexp_of_t sexp_of_a sexp_of_b t = Ivar.sexp_of_t sexp_of_a sexp_of_b (to_ivar t)

type ('a, 'execution_context) deferred = ('a, 'execution_context) t with sexp_of

let peek t = Ivar.peek (to_ivar t)

let is_determined t = Ivar.is_full (to_ivar t)

let create f =
  let result = Ivar.create () in
  f result;
  of_ivar result;
;;

let return a = of_ivar (Ivar.create_full a)

module Scheduler_dependent (Scheduler : Basic_scheduler) = struct

  module Ivar = Ivar.Scheduler_dependent (Scheduler)

  type 'a t = ('a, Scheduler.Execution_context.t) deferred with sexp_of

  type 'a detailed = 'a t with sexp_of

  let sexp_of_detailed sexp_of_a t = Ivar.sexp_of_detailed sexp_of_a (to_ivar t)

  let upon t f = Ivar.upon (to_ivar t) f

  let upon' t f = Ivar.upon' (to_ivar t) f

  let bind t f =
    create (fun bind_result ->
      upon t (fun a -> Ivar.connect ~bind_result ~bind_rhs:(to_ivar (f a))))
  ;;

  let install_removable_handler t f = Ivar.install_removable_handler (to_ivar t) f

end
