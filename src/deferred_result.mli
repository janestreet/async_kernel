open! Core

[@@@implicit_kind:
  ('ok : value_or_null)
  * ('ok1 : value_or_null)
  * ('ok2 : value_or_null)
  * ('ok3 : value_or_null)]

include
  Monad.S2
  [@kind value_or_null mod maybe_null]
  with type ('ok, 'err) t = ('ok, 'err) Result.t Deferred1.t
(** @open *)

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

(** [ok_unit = return ()] *)
val ok_unit : (unit, _) t

module List :
  Monad_sequence.S2_sequential
  with type ('a, 'e) monad := ('a, 'e) t
  with type 'a t := 'a list

(** [repeat_until_finished initial_state f] works just like
    {!Deferred.repeat_until_finished} but with the [Deferred.Result] monad. If [f] returns
    an [Result.Error] the loop terminates and returns. *)
val repeat_until_finished
  :  'state
  -> ('state -> ([ `Repeat of 'state | `Finished of 'result ], 'err) t)
  -> ('result, 'err) t
