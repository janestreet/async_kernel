(** Provides a {{!Async_kernel.Clock_intf.Clock}[Clock]} with [Time_ns] as the unit. *)

open! Core_kernel
open! Import

include Clock_intf.Clock with module Time := Time_ns (** @open *)
