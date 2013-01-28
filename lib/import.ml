open Core.Std

module type Basic_scheduler = sig
  module Execution_context : sig type t with sexp_of end
  val current_execution_context : unit -> Execution_context.t
  val add_job : Execution_context.t Job.t -> unit
end
