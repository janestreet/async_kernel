open Core.Std

type t

val noop : t

val create : (unit -> unit) -> t

val unregister : t -> unit
