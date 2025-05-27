@@ portable

open! Core

(** A multi-producer, single-consumer, lock free queue, specialized for use as the
    external job queue within the Async scheduler. *)
type 'a t : value mod contended portable

[%%rederive:
  type nonrec ('a : value mod contended portable) t = 'a t [@@deriving sexp_of]]

(** [create_alone ()] creates a new MPSC queue alone on a cache line, to avoid false
    sharing. *)
val create_alone : unit -> 'a t

val is_empty : ('a : value mod contended portable). 'a t -> bool

(** [enqueue t x] atomically pushes [x] to the front of the queue [t]. *)
val enqueue : 'a t @ contended -> 'a @ portable -> unit

val dequeue_until_empty
  :  ('a : value mod contended portable) t @ contended
  -> f:('a -> unit) @ local
  -> unit
