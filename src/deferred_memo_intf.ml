open! Core
open! Import

module type S = sig
  type 'a deferred

  (** Memoization functions like in [Core.Memo], with re-raising of exceptions
      thrown asynchronously.

      Also see [Lazy_deferred], of which [Deferred.Memo.unit] is a special case. *)

  (** [general hashable f] returns a memoized version of [f], where the results are stored
      in a hash table indexed according to [hashable].  If [f a] asynchronously raises, then
      the error is stored in the hash table and is reraised when [a] is demanded.

      Unlike [Core.Memo.general], this [general] does not support
      [cache_size_bound] due to complexities of asynchrony -- even when one has a deferred
      return by the memoized function, there still may be asynchronous jobs working to
      determine it.

      Unlike [Core.Memo.general], this [general] takes a required [Hashable]
      module argument, to avoid unintentional use of polymorphic comparison. *)
  val general
    :  (module Hashable.S_plain with type t = 'a)
    -> ('a -> 'b deferred)
    -> ('a -> 'b deferred) Staged.t

  (** Memoize a recursive asynchronous function. See [Memo.recursive] for the introduction.
      We do not implement Async-aware dependency cycle detection, so if recursion is not
      well-founded then the computation will just deadlock. *)
  val recursive
    :  (module Hashable.S_plain with type t = 'a)
    -> (('a -> 'b deferred) -> 'a -> 'b deferred)
    -> ('a -> 'b deferred) Staged.t

  val unit : (unit -> 'a deferred) -> (unit -> 'a deferred) Staged.t
end

module type Deferred_memo = sig
  module type S = S

  module Make (M : Monad.Infix with type 'a t = 'a Deferred1.t) :
    S with type 'a deferred := 'a M.t

  include S with type 'a deferred := 'a Deferred1.t
end
