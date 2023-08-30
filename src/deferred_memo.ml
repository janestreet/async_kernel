open! Core
open! Deferred_std
include Deferred_memo_intf

module Make (M : Monad.Infix with type 'a t = 'a Deferred1.t) :
  S with type 'a deferred := 'a M.t = struct
  open! M

  let reraise = function
    | Ok x -> x
    | Error exn -> Exn.reraise exn "caught exception in memoized function"
  ;;

  let general' (type a) ~run (hashable : (module Hashable.S_plain with type t = a)) f =
    let module Hashable = (val hashable) in
    let f =
      Memo.general ~hashable:Hashable.hashable (fun a ->
        Monitor.try_with ~rest:`Log ~run (fun () -> f a))
    in
    Staged.stage (fun a -> f a >>| reraise)
  ;;

  let general hashable f = general' ~run:`Now hashable f

  let recursive (type a) (hashable : (module Hashable.S_plain with type t = a)) f_onestep =
    let rec memoized =
      lazy
        (general'
           ~run:`Schedule
           hashable
           (f_onestep (fun x -> (unstage (force memoized)) x)))
    in
    force memoized
  ;;

  let unit f =
    let f = Memo.unit (fun () -> Monitor.try_with ~rest:`Log ~run:`Now f) in
    Staged.stage (fun () -> f () >>| reraise)
  ;;
end

include Make (Deferred1)
