open Core_kernel.Std

include Quickcheck_intf

module type Quickcheck_async_configured = sig

  include Quickcheck_configured
    with type 'a gen := 'a Quickcheck.Generator.t

  (** Like [test], but for asynchronous tests. *)
  val async_test
    :  ?seed    : seed
    -> ?trials  : int
    -> ?sexp_of : ('a -> Sexp.t)
    -> 'a Quickcheck.Generator.t
    -> f:('a -> unit Deferred.t)
    -> unit Deferred.t

end
