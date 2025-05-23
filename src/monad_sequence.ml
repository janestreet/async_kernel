(** [Monad_sequence.S] is a generic interface specifying functions that deal with a
    container and a monad. It is specialized to the [Deferred] monad and used with various
    containers in modules [Deferred.Array], [Deferred.List], [Deferred.Queue], and
    [Deferred.Sequence]. The [Monad_sequence.how] type specifies the parallelism of
    container iterators. *)

open! Core
open! Import

type how =
  [ `Parallel (** like [`Max_concurrent_jobs Int.max_value] *)
  | `Sequential
    (** [`Sequential] is often but not always the same as [`Max_concurrent_jobs 1] (for
        example, they differ in the [Or_error] monad). *)
  | `Max_concurrent_jobs of int
  ]
[@@deriving sexp_of]

module type S = sig
  type 'a monad
  type 'a t

  val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b monad) -> 'b monad
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b monad) -> 'b monad
  val find : 'a t -> f:('a -> bool monad) -> 'a option monad
  val findi : 'a t -> f:(int -> 'a -> bool monad) -> (int * 'a) option monad
  val find_map : 'a t -> f:('a -> 'b option monad) -> 'b option monad
  val find_mapi : 'a t -> f:(int -> 'a -> 'b option monad) -> 'b option monad
  val exists : 'a t -> f:('a -> bool monad) -> bool monad
  val existsi : 'a t -> f:(int -> 'a -> bool monad) -> bool monad
  val for_all : 'a t -> f:('a -> bool monad) -> bool monad
  val for_alli : 'a t -> f:(int -> 'a -> bool monad) -> bool monad
  val all : 'a monad t -> 'a t monad
  val all_unit : unit monad t -> unit monad

  (** {2 Deferred iterators} *)

  val init : how:how -> int -> f:(int -> 'a monad) -> 'a t monad
  val iter : how:how -> 'a t -> f:('a -> unit monad) -> unit monad
  val iteri : how:how -> 'a t -> f:(int -> 'a -> unit monad) -> unit monad
  val map : how:how -> 'a t -> f:('a -> 'b monad) -> 'b t monad
  val mapi : how:how -> 'a t -> f:(int -> 'a -> 'b monad) -> 'b t monad
  val filter : how:how -> 'a t -> f:('a -> bool monad) -> 'a t monad
  val filteri : how:how -> 'a t -> f:(int -> 'a -> bool monad) -> 'a t monad
  val filter_map : how:how -> 'a t -> f:('a -> 'b option monad) -> 'b t monad
  val filter_mapi : how:how -> 'a t -> f:(int -> 'a -> 'b option monad) -> 'b t monad
  val concat_map : how:how -> 'a t -> f:('a -> 'b t monad) -> 'b t monad
  val concat_mapi : how:how -> 'a t -> f:(int -> 'a -> 'b t monad) -> 'b t monad
end

(** [Monad_sequence.S2_result] is a generic interface specifying functions that deal with
    a container and a monad similar to Result.t or Result.t Deferred.t that contains both
    a value and an error. Unlike [Monad_sequence.S], it does not support the parallelism
    in container iterators, and they will always return the first error encountered. *)

module type S2_result = sig
  type ('a, 'e) monad
  type 'a t

  val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> ('b, 'e) monad) -> ('b, 'e) monad
  val fold : 'a t -> init:'b -> f:('b -> 'a -> ('b, 'e) monad) -> ('b, 'e) monad
  val find : 'a t -> f:('a -> (bool, 'e) monad) -> ('a option, 'e) monad
  val findi : 'a t -> f:(int -> 'a -> (bool, 'e) monad) -> ((int * 'a) option, 'e) monad
  val find_map : 'a t -> f:('a -> ('b option, 'e) monad) -> ('b option, 'e) monad
  val find_mapi : 'a t -> f:(int -> 'a -> ('b option, 'e) monad) -> ('b option, 'e) monad
  val exists : 'a t -> f:('a -> (bool, 'e) monad) -> (bool, 'e) monad
  val existsi : 'a t -> f:(int -> 'a -> (bool, 'e) monad) -> (bool, 'e) monad
  val for_all : 'a t -> f:('a -> (bool, 'e) monad) -> (bool, 'e) monad
  val for_alli : 'a t -> f:(int -> 'a -> (bool, 'e) monad) -> (bool, 'e) monad

  (** {2 Deferred iterators} *)

  val init : int -> f:(int -> ('a, 'e) monad) -> ('a t, 'e) monad
  val iter : 'a t -> f:('a -> (unit, 'e) monad) -> (unit, 'e) monad
  val iteri : 'a t -> f:(int -> 'a -> (unit, 'e) monad) -> (unit, 'e) monad
  val map : 'a t -> f:('a -> ('b, 'e) monad) -> ('b t, 'e) monad
  val mapi : 'a t -> f:(int -> 'a -> ('b, 'e) monad) -> ('b t, 'e) monad
  val filter : 'a t -> f:('a -> (bool, 'e) monad) -> ('a t, 'e) monad
  val filteri : 'a t -> f:(int -> 'a -> (bool, 'e) monad) -> ('a t, 'e) monad
  val filter_map : 'a t -> f:('a -> ('b option, 'e) monad) -> ('b t, 'e) monad
  val filter_mapi : 'a t -> f:(int -> 'a -> ('b option, 'e) monad) -> ('b t, 'e) monad
  val concat_map : 'a t -> f:('a -> ('b t, 'e) monad) -> ('b t, 'e) monad
  val concat_mapi : 'a t -> f:(int -> 'a -> ('b t, 'e) monad) -> ('b t, 'e) monad
end
