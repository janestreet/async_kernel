open! Base

(** Like {{!Weak_hashtbl} [Weak_hashtbl]}, but automatically collects keys with unused
    data, rather than requiring user code to call [remove_keys_with_unused_data]. *)

include module type of Weak_hashtbl (** @open *)

val create
  :  ?growth_allowed:bool (** default is [true] *)
  -> ?size:int (** default is [0] *)
  -> (module Hashtbl.Key.S with type t = 'a)
  -> ('a, 'b) t

val reclaim_space_for_keys_with_unused_data : [ `Do_not_use ] -> unit
val set_run_when_unused_data : [ `Do_not_use ] -> unit
