open Core
open Async_kernel

type t =
  { abort : Error.t Ivar.t
  ; mutable consecutive_unsuccessful_attempts : int
  }

let abort t error = Ivar.fill_if_empty t.abort error
let consecutive_unsuccessful_attempts_so_far t = t.consecutive_unsuccessful_attempts

module Private = struct
  let create ~abort = { abort; consecutive_unsuccessful_attempts = 0 }
  let abort_ivar t = t.abort

  let incr_consecutive_unsuccessful_attempts t =
    t.consecutive_unsuccessful_attempts <- t.consecutive_unsuccessful_attempts + 1
  ;;

  let reset_consecutive_unsuccessful_attempts t = t.consecutive_unsuccessful_attempts <- 0
end
