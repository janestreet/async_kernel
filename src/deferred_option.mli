open! Core_kernel.Std

include Monad.S with type 'a t = 'a option Deferred0.t
