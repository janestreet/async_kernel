open! Core

(** A multi-producer, single-consumer, lock free queue, specialized for use as the
    external job queue within the Async scheduler. *)
type 'a t

[%%rederive: type nonrec 'a t = 'a t [@@deriving sexp_of]]

(** [create_alone ()] creates a new MPSC queue alone on a cache line, to avoid false
    sharing. *)
val create_alone : unit -> 'a t

(** [enqueue t x] atomically pushes [x] to the front of the queue [t]. *)
val enqueue : 'a t -> 'a -> unit

val dequeue_until_empty : 'a t -> f:('a -> unit) -> unit
