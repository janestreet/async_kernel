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
[@@deriving sexp_of ~portable]

module type S_generic_unindexed = sig
  type ('a, 'e) monad
  type 'a t
  type 'fn iterator

  val fold : 'a t -> init:'b -> f:('b -> 'a -> ('b, 'e) monad) -> ('b, 'e) monad
  val find : 'a t -> f:('a -> (bool, 'e) monad) -> ('a option, 'e) monad
  val find_map : 'a t -> f:('a -> ('b option, 'e) monad) -> ('b option, 'e) monad
  val exists : 'a t -> f:('a -> (bool, 'e) monad) -> (bool, 'e) monad
  val for_all : 'a t -> f:('a -> (bool, 'e) monad) -> (bool, 'e) monad
  val all : ('a, 'e) monad t -> ('a t, 'e) monad
  val all_unit : (unit, 'e) monad t -> (unit, 'e) monad

  (** {2 Deferred iterators} *)

  val iter : ('a t -> f:('a -> (unit, 'e) monad) -> (unit, 'e) monad) iterator
  val map : ('a t -> f:('a -> ('b, 'e) monad) -> ('b t, 'e) monad) iterator
  val filter : ('a t -> f:('a -> (bool, 'e) monad) -> ('a t, 'e) monad) iterator
  val filter_map : ('a t -> f:('a -> ('b option, 'e) monad) -> ('b t, 'e) monad) iterator
  val concat_map : ('a t -> f:('a -> ('b t, 'e) monad) -> ('b t, 'e) monad) iterator
end

module type S_generic = sig
  type ('a, 'e) monad
  type 'a t
  type 'fn iterator

  val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> ('b, 'e) monad) -> ('b, 'e) monad
  val findi : 'a t -> f:(int -> 'a -> (bool, 'e) monad) -> ((int * 'a) option, 'e) monad
  val find_mapi : 'a t -> f:(int -> 'a -> ('b option, 'e) monad) -> ('b option, 'e) monad
  val existsi : 'a t -> f:(int -> 'a -> (bool, 'e) monad) -> (bool, 'e) monad
  val for_alli : 'a t -> f:(int -> 'a -> (bool, 'e) monad) -> (bool, 'e) monad

  (** {2 Deferred iterators} *)

  val init : (int -> f:(int -> ('a, 'e) monad) -> ('a t, 'e) monad) iterator
  val iteri : ('a t -> f:(int -> 'a -> (unit, 'e) monad) -> (unit, 'e) monad) iterator
  val mapi : ('a t -> f:(int -> 'a -> ('b, 'e) monad) -> ('b t, 'e) monad) iterator
  val filteri : ('a t -> f:(int -> 'a -> (bool, 'e) monad) -> ('a t, 'e) monad) iterator

  val filter_mapi
    : ('a t -> f:(int -> 'a -> ('b option, 'e) monad) -> ('b t, 'e) monad) iterator

  val concat_mapi
    : ('a t -> f:(int -> 'a -> ('b t, 'e) monad) -> ('b t, 'e) monad) iterator

  include
    S_generic_unindexed
    with type ('a, 'e) monad := ('a, 'e) monad
    with type 'a t := 'a t
    with type 'fn iterator := 'fn iterator
end

module type S = sig
  type 'a monad
  type 'a t

  include
    S_generic
    with type ('a, 'e) monad := 'a monad
     and type 'a t := 'a t
     and type 'fn iterator := how:how -> 'fn
end

module type S_sequential_unindexed = sig
  type 'a monad
  type 'a t

  include
    S_generic_unindexed
    with type ('a, 'e) monad := 'a monad
     and type 'a t := 'a t
     and type 'fn iterator := 'fn
end

(** [Monad_sequence.S2_sequential] is a generic interface specifying functions that deal
    with a container and a monad. The monad is parameterized over an value type ['a] and
    another type ['e], such as the error in [Result.t]. Unlike [Monad_sequence.S], it does
    not support the parallelism in container iterators. With [Result.t], for example, they
    may always return the first error encountered, or combine all errors, depending on
    implementation. *)

module type S2_sequential = sig
  type ('a, 'e) monad
  type 'a t

  include
    S_generic
    with type ('a, 'e) monad := ('a, 'e) monad
     and type 'a t := 'a t
     and type 'fn iterator := 'fn
end
