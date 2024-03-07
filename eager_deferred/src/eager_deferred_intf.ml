open! Core
open! Async_kernel
open! Import

module type Eager_deferred_or_error = sig
  type +'a deferred
  type 'a t = 'a Or_error.t deferred

  include Monad.S with type 'a t := 'a t

  val fail : Error.t -> _ t
  val ok_unit : unit t
  val ok_exn : 'a t -> 'a deferred
  val of_exn : ?backtrace:[ `Get | `This of string ] -> exn -> _ t

  val of_exn_result
    :  ?backtrace:[ `Get | `This of string ]
    -> ('a, exn) Result.t deferred
    -> 'a t

  val error : string -> 'a -> ('a -> Sexp.t) -> _ t
  val error_s : Sexp.t -> _ t
  val error_string : string -> _ t
  val errorf : ('a, unit, string, _ t) format4 -> 'a
  val tag : 'a t -> tag:string -> 'a t
  val tag_s : 'a t -> tag:Sexp.t -> 'a t
  val tag_s_lazy : 'a t -> tag:Sexp.t Lazy.t -> 'a t
  val tag_arg : 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t
  val unimplemented : string -> _ t
  val find_map_ok : 'a list -> f:('a -> 'b t) -> 'b t

  (** Note that [try_with f] is eager only in the [Ok] case. *)
  val try_with
    :  ?extract_exn:bool
    -> ?run:[ `Now | `Schedule ]
    -> ?rest:[ `Log | `Raise | `Call of exn -> unit ] (** default is [`Raise] *)
    -> ?here:Lexing.position
    -> ?name:string
    -> (unit -> 'a deferred)
    -> 'a t

  (** Note that [try_with_join f] is eager only when no exception is raised by [f]. *)
  val try_with_join
    :  ?extract_exn:bool
    -> ?run:[ `Now | `Schedule ]
    -> ?rest:[ `Log | `Raise | `Call of exn -> unit ] (** default is [`Raise] *)
    -> ?here:Lexing.position
    -> ?name:string
    -> (unit -> 'a t)
    -> 'a t

  val combine_errors : 'a t list -> 'a list t
  val combine_errors_unit : unit t list -> unit t
  val filter_ok_at_least_one : 'a t list -> 'a list t

  module List : Monad_sequence.S with type 'a monad := 'a t with type 'a t := 'a list

  val repeat_until_finished
    :  'state
    -> ('state -> [ `Repeat of 'state | `Finished of 'result ] t)
    -> 'result t
end

module type Eager_deferred1 = sig
  type +'a t

  include Invariant.S1 with type 'a t := 'a t
  include Monad with type 'a t := 'a t

  module Infix : sig
    include Monad.Infix with type 'a t := 'a t

    val ( >>> ) : 'a t -> ('a -> unit) -> unit
  end

  val any : 'a t list -> 'a t
  val any_unit : unit t list -> unit t
  val both : 'a t -> 'b t -> ('a * 'b) t
  val create : ('a Ivar.t -> unit) -> 'a t
  val don't_wait_for : unit t -> unit
  val is_determined : 'a t -> bool
  val never : unit -> _ t
  val ok : 'a t -> ('a, _) Core.Result.t t
  val peek : 'a t -> 'a option
  val unit : unit t
  val upon : 'a t -> ('a -> unit) -> unit
  val value_exn : 'a t -> 'a

  val repeat_until_finished
    :  'state
    -> ('state -> [ `Repeat of 'state | `Finished of 'result ] t)
    -> 'result t

  module List : Monad_sequence.S with type 'a monad := 'a t with type 'a t := 'a list

  (** Similar to {Deferred.Queue} but eager when passing ~how:`Sequential. The functions
      in [Queue] raise if the queue is mutated during execution. *)
  module Queue : Monad_sequence.S with type 'a monad := 'a t with type 'a t := 'a Queue.t

  module Or_error : Eager_deferred_or_error with type 'a deferred := 'a t
  module Memo : Deferred.Memo.S with type 'a deferred := 'a t

  module Result : sig
    include Monad.S2 with type ('a, 'b) t = ('a, 'b) Result.t Deferred.t

    val fail : 'err -> (_, 'err) t

    (** e.g., [failf "Couldn't find bloogle %s" (Bloogle.to_string b)]. *)
    val failf : ('a, unit, string, (_, string) t) format4 -> 'a

    val map_error : ('ok, 'error1) t -> f:('error1 -> 'error2) -> ('ok, 'error2) t

    (** [combine] waits on both inputs and combines their results using [Result.combine]. *)
    val combine
      :  ('ok1, 'err) t
      -> ('ok2, 'err) t
      -> ok:('ok1 -> 'ok2 -> 'ok3)
      -> err:('err -> 'err -> 'err)
      -> ('ok3, 'err) t
  end
end

module type S = sig
  type +'a t

  include Eager_deferred1 with type 'a t := 'a t (** @open *)

  (** Intended usage is to [open Eager_deferred.Use] to shadow operations from the
      non-eager world and rebind them to their eager counterparts. *)
  module Use : sig
    module Deferred : sig
      type nonrec 'a t = 'a t [@@deriving sexp_of]

      include Eager_deferred1 with type 'a t := 'a t
    end

    include Monad.Infix with type 'a t := 'a t
    include module type of Deferred.Let_syntax

    val upon : 'a t -> ('a -> unit) -> unit
    val ( >>> ) : 'a t -> ('a -> unit) -> unit

    val ( >>=? )
      :  ('a, 'e) Core.Result.t t
      -> ('a -> ('b, 'e) Core.Result.t t)
      -> ('b, 'e) Core.Result.t t

    val ( >>|? ) : ('a, 'e) Core.Result.t t -> ('a -> 'b) -> ('b, 'e) Core.Result.t t
  end
end

module type Eager_deferred = sig
  module type S = S

  include S with type 'a t := 'a Deferred.t
end
