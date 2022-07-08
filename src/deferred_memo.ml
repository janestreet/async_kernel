open Core
open Deferred_std
module Deferred = Deferred1

let reraise = function
  | Ok x -> x
  | Error exn -> Exn.reraise exn "caught exception in memoized function"
;;

let general' (type a) ~run (hashable : (module Hashable.S_plain with type t = a)) f =
  let module Hashable = (val hashable) in
  let f =
    Memo.general ~hashable:Hashable.hashable (fun a ->
      Monitor.try_with
        ~rest:`Log
        ~run
        (fun () -> f a))
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
  let f =
    Memo.unit (fun () ->
      Monitor.try_with
        ~rest:`Log
        ~run:`Now
        f)
  in
  Staged.stage (fun () -> f () >>| reraise)
;;
