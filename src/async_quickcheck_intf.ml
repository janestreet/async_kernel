open Core_kernel.Std

include Quickcheck_intf

module type Quickcheck_async = sig

  include Quickcheck

  (** Like [test], but for asynchronous tests. *)
  val async_test
    :  ?seed    : seed
    -> ?trials  : int
    -> ?sexp_of : ('a -> Sexp.t)
    -> 'a Generator.t
    -> f:('a -> unit Deferred.t)
    -> unit Deferred.t

end
