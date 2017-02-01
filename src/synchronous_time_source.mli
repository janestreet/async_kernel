open! Core_kernel
open! Import

include module type of struct include Synchronous_time_source0 end

(** A time source with [now t] given by wall-clock time (i.e. [Time_ns.now]), and
    automatically advanced at the start of each Async cycle. *)
val wall_clock : unit -> t
