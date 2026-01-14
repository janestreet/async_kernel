open Core
open Async_kernel

(** Context passed to [connect] in [create_with_connect_context] that provides information
    about the connection state and allows the callback to abort the persistent connection. *)
type t

(** [abort t error] causes the persistent connection to stop retrying and stop
    reconnecting.

    [connected_or_failed_to_connect] will return [Error error]. Calling [abort] multiple
    times is safe; subsequent calls are ignored. *)
val abort : t -> Error.t -> unit

(** Returns the number of consecutive unsuccessful connection attempts so far, not
    including the current attempt. Returns 0 on the first attempt, 1 on the second attempt
    after the first failed, etc. *)
val consecutive_unsuccessful_attempts_so_far : t -> int

module Private : sig
  val create : abort:Error.t Ivar.t -> t
  val abort_ivar : t -> Error.t Ivar.t
  val incr_consecutive_unsuccessful_attempts : t -> unit
  val reset_consecutive_unsuccessful_attempts : t -> unit
end
