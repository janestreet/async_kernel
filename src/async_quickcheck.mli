open Core_kernel.Std
open Async_quickcheck_intf

include Quickcheck_async

module Configure (Config : Quickcheck_config) : Quickcheck_async
