open Core.Std

module Deferred = struct
  type 'a t = ('a, Execution_context.t) Raw_deferred.t
end

type how = [ `Parallel | `Sequential ]

module type Deferred_sequence = sig
  type 'a t

  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b Deferred.t) -> 'b Deferred.t

  (* default [how] is [`Sequential] *)
  val iter       : ?how:how -> 'a t -> f:('a -> unit      Deferred.t) -> unit Deferred.t
  val map        : ?how:how -> 'a t -> f:('a -> 'b        Deferred.t) -> 'b t Deferred.t
  val filter     : ?how:how -> 'a t -> f:('a -> bool      Deferred.t) -> 'a t Deferred.t
  val filter_map : ?how:how -> 'a t -> f:('a -> 'b option Deferred.t) -> 'b t Deferred.t

  val all      : 'a   Deferred.t t -> 'a t Deferred.t
  val all_unit : unit Deferred.t t -> unit Deferred.t
end

module type Deferred_map = sig

  type ('k, 'v) t

  val filter_mapi
    :  ('k, 'v1) t
    -> f:(key:'k -> data:'v1 -> 'v2 option Deferred.t)
    -> ('k, 'v2) t Deferred.t

end
