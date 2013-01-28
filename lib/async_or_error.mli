open Core.Std

type 'a t = 'a Or_error.t Deferred.t

include Monad.S with type 'a t := 'a t

val return : 'ok -> 'ok t
val fail : Error.t -> _ t
val of_exn : exn -> _ t

val failwith : string -> _ t

val ok_unit : unit t

val never : unit -> _ t


(* This interface is more generic than [unit -> 'a Deferred.t].
   Various use cases:

   {[
   Or_error.try_with ~f:fct arg
   Or_error.try_with () ~f:(fun () ->
   ....
   )
   let f_or_exn = Or_error.try_with ~f
   ]}

   The label is essentially there to inverse the order with unit if the body
   of the function is big. *)
val try_with      : ?name:string -> f:('arg -> 'a Deferred.t) -> 'arg -> 'a t
val try_with_join : ?name:string -> f:('arg -> 'a          t) -> 'arg -> 'a t

module List : Deferred_intf.Monad_sequence
  with type 'a monad := 'a t
  with type 'a t := 'a list
