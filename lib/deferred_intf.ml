open Core.Std

module Deferred = struct
  type 'a t = 'a Ivar.Deferred.t
end

type how = [ `Parallel | `Sequential ] with sexp_of

module type Monad_sequence = sig
  type 'a monad
  type 'a t

  val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b monad) -> 'b monad
  val fold  : 'a t -> init:'b -> f:(       'b -> 'a -> 'b monad) -> 'b monad

  (** default [how] is [`Sequential] *)
  val init       : ?how:how ->  int -> f:(int       ->         'a monad) -> 'a t monad
  val iter       : ?how:how -> 'a t -> f:(       'a ->       unit monad) -> unit monad
  val iteri      : ?how:how -> 'a t -> f:(int -> 'a ->       unit monad) -> unit monad
  val map        : ?how:how -> 'a t -> f:(       'a ->         'b monad) -> 'b t monad
  val filter     : ?how:how -> 'a t -> f:(       'a ->       bool monad) -> 'a t monad
  val filter_map : ?how:how -> 'a t -> f:(       'a  -> 'b option monad) -> 'b t monad

  val find     : 'a t -> f:('a -> bool      monad) -> 'a option monad
  val find_map : 'a t -> f:('a -> 'b option monad) -> 'b option monad

  val all      : 'a   monad t -> 'a t monad
  val all_unit : unit monad t -> unit monad
end

module type Deferred_map = sig

  type ('a, 'b, 'c) t = ('a, 'b, 'c) Map.t

  val change
    :  ('k, 'v, 'comparator) t
    -> 'k
    -> ('v option -> 'v option Deferred.t)
    -> ('k, 'v, 'comparator) t Deferred.t

  val iter
    :  ?how:how
    -> ('k, 'v, _) t
    -> f:(key:'k -> data:'v -> unit Deferred.t)
    -> unit Deferred.t

  val map
    :  ?how:how
    -> ('k, 'v1, 'comparator) t
    -> f:('v1 -> 'v2 Deferred.t)
    -> ('k, 'v2, 'comparator) t Deferred.t

  val mapi
    :  ?how:how
    -> ('k, 'v1, 'comparator) t
    -> f:(key:'k -> data:'v1 -> 'v2 Deferred.t)
    -> ('k, 'v2, 'comparator) t Deferred.t

  val fold
    :  ('k, 'v, _) t
    -> init:'a
    -> f:(key:'k -> data:'v -> 'a -> 'a Deferred.t)
    -> 'a Deferred.t

  val fold_right
    :  ('k, 'v, _) t
    -> init:'a
    -> f:(key:'k -> data:'v -> 'a -> 'a Deferred.t)
    -> 'a Deferred.t

  val filter
    :  ?how:how
    -> ('k, 'v, 'comparable) t
    -> f:(key:'k -> data:'v -> bool Deferred.t)
    -> ('k, 'v, 'comparable) t Deferred.t

  val filter_map
    :  ?how:how
    -> ('k, 'v1, 'comparable) t
    -> f:('v1 -> 'v2 option Deferred.t)
    -> ('k, 'v2, 'comparable) t Deferred.t

  val filter_mapi
    :  ?how:how
    -> ('k, 'v1, 'comparable) t
    -> f:(key:'k -> data:'v1 -> 'v2 option Deferred.t)
    -> ('k, 'v2, 'comparable) t Deferred.t

  (* val compare
   *   :  ('v -> 'v -> int Deferred.t)
   *   -> ('k, 'v, 'comparator) t
   *   -> ('k, 'v, 'comparator) t
   *   -> int Deferred.t
   *
   * val equal
   *   :  ('v -> 'v -> bool Deferred.t)
   *   -> ('k, 'v, 'comparator) t
   *   -> ('k, 'v, 'comparator) t
   *   -> bool Deferred.t *)

  val merge
    : ?how:how
    -> ('k, 'v1, 'comparator) t
    -> ('k, 'v2, 'comparator) t
    -> f:(key:'k
          -> [ `Left of 'v1 | `Right of 'v2 | `Both of 'v1 * 'v2 ]
          -> 'v3 option Deferred.t)
    -> ('k, 'v3, 'comparator) t Deferred.t

  (* val fold_range_inclusive
   *   :  ('k, 'v, 'comparator) t
   *   -> min:'k
   *   -> max:'k
   *   -> init:'a
   *   -> f:(key:'k -> data:'v -> 'a -> 'a Deferred.t)
   *   -> 'a Deferred.t *)

  (* val of_alist_fold
   *   :  ('k * 'v1) list
   *   -> init:'v2
   *   -> f:('v2 -> 'v1 -> 'v2 Deferred.t)
   *   -> ('k, 'v2, 'comparator) t *)

  (* val of_alist_reduce
   *   :  ('k * 'v) list
   *   -> f:('v -> 'v -> 'v Deferred.t)
   *   -> ('k, 'v, 'comparator) t *)

end
