open Core_kernel.Std
open Import

type t = Types.External_job.t = T : Execution_context.t * ('a -> unit) * 'a -> t
with sexp_of
