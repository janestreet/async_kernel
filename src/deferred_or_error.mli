(** The deferred analog of [Core.Or_error].  It is exposed in std.ml as
    [Deferred.Or_error].

    The mental model for a function returning an ['a Deferred.Or_error.t] is that the
    function never raises.  All error cases are caught and expressed as an [Error _]
    result.  This module preserves that property.

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

open! Core_kernel.Std
open! Import

module Deferred = Deferred1

type 'a t = 'a Or_error.t Deferred.t

(** [return x = Deferred.return (Ok x)] **)
include Applicative.S with type 'a t := 'a t
include Monad.S       with type 'a t := 'a t

(** [fail error = Deferred.return (Error error)] **)
val fail : Error.t -> _ t

val ignore : _ t -> unit t

(** These functions are direct analogs of the corresponding [Core.Or_error] functions. *)
val ok_exn : 'a t -> 'a Deferred.t
val of_exn : exn -> _ t
val of_exn_result : ('a, exn) Result.t Deferred.t -> 'a t
val error : string -> 'a -> ('a -> Sexp.t) -> _ t
val error_string : string -> _ t
val errorf : ('a, unit, string, _ t) format4 -> 'a
val tag : 'a t -> string -> 'a t
val tag_arg : 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t
val unimplemented : string -> _ t
val combine_errors : 'a t list -> 'a list t
val combine_errors_unit : unit t list -> unit t

(** [ok_unit = return ()] *)
val ok_unit : unit t

(** [try_with f] catches exceptions thrown by [f] and returns them in the Result.t as an
    Error.t.  [try_with_join] is like [try_with], except that [f] can throw exceptions or
    return an [Error] directly, without ending up with a nested error; it is equivalent to
    [try_with f >>| Result.join].

    The option [extract_exn] is passed along to [Monitor.try_with ?extract_exn] and
    specifies whether or not the monitor exn wrapper should be skipped ([extract_exn:true]
    or kept ([extract_exn:false]). *)
val try_with
  :  ?extract_exn:bool (** default is [false] *)
  -> ?name:string
  -> (unit -> 'a Deferred.t)
  -> 'a t
val try_with_join
  : ?extract_exn:bool (** default is [false] *)
  -> ?name:string
  -> (unit -> 'a t)
  -> 'a t

module List : Monad_sequence.S
  with type 'a monad := 'a t
  with type 'a t := 'a list
