(** Async's analog of [Core_kernel.Std.Gc]. *)

open! Core_kernel.Std

include module type of Core_kernel.Std.Gc
  (** We remove the [Expert] module, which has functions that are superseded by
      Async-friendly functions below. *)
  with module Expert := Core_kernel.Std.Gc.Expert

(** [add_finalizer b f] ensures that [f] runs after [b] becomes unreachable.  [f b] will
    run in its own Async job.  If [f] raises, the unhandled exception will be raised to
    the monitor that called [add_finalizer b f].

    The OCaml runtime only supports finalizers on heap blocks, hence [add_finalizer]
    requires [b : _ Heap_block.t].  [add_finalizer_exn b f] is like [add_finalizer], but
    will raise if [b] is not a heap block.

    The runtime essentially maintains a set of finalizer pairs:

    {v
      'a Heap_block.t * ('a Heap_block.t -> unit)
    v}

    Each call to [add_finalizer] adds a new pair to the set.  It is allowed for many pairs
    to have the same heap block, the same function, or both.  Each pair is a distinct
    element of the set.

    After a garbage collection determines that a heap block [b] is unreachable, it removes
    from the set of finalizers all finalizer pairs [(b, f)] whose block is [b], and then
    and runs [f b] for all such pairs.  Thus, a finalizer registered with [add_finalizer]
    will run at most once.

    In a finalizer pair [(b, f)], it is a mistake for the closure of [f] to reference
    (directly or indirectly) [b] -- [f] should only access [b] via its argument.
    Referring to [b] in any other way will cause [b] to be kept alive forever, since [f]
    itself is a root of garbage collection, and can itself only be collected after the
    pair [(b, f)] is removed from the set of finalizers.

    The [f] function can use all features of OCaml and Async, since it runs as an ordinary
    Async job.  [f] can even make make [b] reachable again.  It can even call
    [add_finalizer] on [b] or other values to register other finalizer functions.
*)
val add_finalizer     : 'a Heap_block.t -> ('a Heap_block.t -> unit) -> unit
val add_finalizer_exn : 'a -> ('a -> unit) -> unit

(** A GC alarm calls a user function after the end of each major GC cycle. *)
module Alarm : sig

  type t [@@deriving sexp_of]

  (** [create f] arranges for [f] to be called after the end of each major GC cycle,
      starting with the current cycle or the next one.  [f] will be run in the monitor
      that [create] was called in. *)
  val create : (unit -> unit) -> t

  (** [delete t] will stop the calls to the function associated with [a].  Calling [delete
      t] again has no effect. *)
  val delete : t -> unit
end
