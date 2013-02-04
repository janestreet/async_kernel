(** This module is the deferred analog of [Core.Or_error]. It is exposed in std.ml as
    [Deferred.Or_error].

    The mental model for a function returning an ['a Deferred.Or_error.t] is that the
    function never raises. All error cases are caught and expressed as an [Error _]
    result. This module preserves that property.

    Unfortunately, there is no way to enforce this property using the type system, so it
    is more like a convention, or idiom.  A function whose type ends with [... -> 'a
    Deferred.Or_error.t] and still raises should be considered broken, and be fixed.  With
    that property in mind, [Deferred.Or_error.List.iter], for example, does not wrap the
    execution of the given iter function [f] inside a monitor.  If one of these
    application raises, the whole function [Deferred.Or_error.List.iter] will raise as a
    way to try to alert the developer that one the function is broken and needs attention
    and fixing, rather than silently catching the error and converting it to
    [Or_error.Error].

    This behavior is consistent with [Core.Or_error]'s treatment of user-supplied
    functions.

    If you have to deal with a function that does not respect this idiom, you can use
    [Deferred.Or_error.try_with_join] to wrap its execution and enforce this property. *)

open Core.Std

type 'a t = 'a Or_error.t Deferred.t

include Monad.S with type 'a t := 'a t

val return : 'ok -> 'ok t
val fail : Error.t -> _ t
val of_exn : exn -> _ t

val failwith : string -> _ t

val ok_unit : unit t

val never : unit -> _ t


(** This interface is more generic than [unit -> 'a Deferred.t].  Various use cases:

    {[
      Or_error.try_with ~f:fct arg
    ]}
    {[
      Or_error.try_with () ~f:(fun () -> ...)
    ]}
    {[
      let f_or_exn = Or_error.try_with ~f
    ]}

    The label is essentially there to inverse the order with unit if the body
    of the function is big. *)
val try_with      : ?name:string -> f:('arg -> 'a Deferred.t) -> 'arg -> 'a t
val try_with_join : ?name:string -> f:('arg -> 'a          t) -> 'arg -> 'a t

module List : Deferred_intf.Monad_sequence
  with type 'a monad := 'a t
  with type 'a t := 'a list
