open! Core
open! Import
module Ivar = Ivar0
module Handler = Ivar.Handler

(* Deferreds present a covariant view of ivars. We could actually implement deferreds
   using a record of closures, as in the [essence_of_deferred] record below, for which the
   OCaml type checker can infer covariance. However, doing so would make [Ivar.read] very
   costly, because it would have to allocate lots of closures and a record. Instead of
   doing this, we make deferreds an abstract covariant type, which concretely is just the
   ivar, and use [Obj.magic] to convert back and forth between a deferred and its concrete
   representation as an ivar. This [Obj.magic] is safe because the representation is
   always just an ivar, and the covariance follows from the fact that all the deferred
   operations are equivalent to those implemented directly on top of the
   [essence_of_deferred].

   {[
     type (+'a, 'execution_context) essence_of_deferred =
       { peek : unit -> 'a option
       ; is_determined : unit -> bool
       ; upon : ('a -> unit) -> unit
       ; upon' : ('a -> unit) -> Unregister.t
       ; install_removable_handler :
           ('a, 'execution_context) Raw_handler.t -> Unregister.t
       }
   ]} *)

type (+'a : value_or_null) t = 'a Types.Deferred.t

(* the abstract covariant type, equivalent to ivar *)

let of_ivar (type a : value_or_null) (ivar : a Ivar.t) : a t = Obj.magic ivar
let to_ivar (type a : value_or_null) t : a Ivar.t = Obj.magic (t : a t)
let invariant invariant_a t = Ivar.invariant invariant_a (to_ivar t)
let sexp_of_t sexp_of_a t = Ivar.sexp_of_t sexp_of_a (to_ivar t)
let peek (type a : value_or_null) (t : a t) = Ivar.peek (to_ivar t)
let return : ('a : value_or_null). 'a -> 'a t = fun a -> of_ivar (Ivar.create_full a)
let is_determined (type a : value_or_null) (t : a t) = Ivar.is_full (to_ivar t)

let value_exn (type a : value_or_null) (t : a t) =
  Ivar.value
    (to_ivar t)
    ~if_empty_then_failwith:"Deferred.value_exn called on undetermined deferred"
;;

let upon (type a : value_or_null) (t : a t) (f : a -> unit) = Ivar.upon (to_ivar t) f

let create (type a : value_or_null) (f : a Ivar.t -> unit) =
  let result = Ivar.create () in
  f result;
  of_ivar result
;;

(* don't use [create] here as it would allocate one more closure *)
let bind (type (a : value_or_null) (b : value_or_null)) (t : a t) ~(f : a -> b t) : b t =
  let bind_result = Ivar.create () in
  upon t (fun a -> Ivar.connect ~bind_result ~bind_rhs:(to_ivar (f a)));
  of_ivar bind_result
;;

let add_handler (type a : value_or_null) (t : a t) f execution_context =
  Ivar.add_handler (to_ivar t) f execution_context
;;

let remove_handler (type a : value_or_null) (t : a t) h =
  Ivar.remove_handler (to_ivar t) h
;;
