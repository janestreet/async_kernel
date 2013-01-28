open Core.Std;;

module type Basic_scheduler = sig
  module Execution_context : sig type t with sexp_of end
  type t
  val t : unit -> t
  val current_execution_context : t -> Execution_context.t
  val add_job : t -> Execution_context.t Job.t -> unit
end

(* We make all of the basic async types (deferred, ivar, etc.) abstract, in part to get
   nice type-error messages.  In various places internally within async we need to know
   the equivalence between the abstract type and the underlying "raw" type.  So, types
   where we need this [include Raw] in their mli.  Then [of_raw] and [to_raw] allow
   internal code to go back forth between the abstract and underlying types as needed.*)
module type Raw = sig
  type execution_context
  type ('a, 'execution_context) raw
  type 'a t
  val of_raw : ('a, execution_context) raw -> 'a t
  val to_raw : 'a t -> ('a, execution_context) raw
end

let ok_exn = Or_error.ok_exn
