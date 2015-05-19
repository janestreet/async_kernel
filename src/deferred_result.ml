open Core_kernel.Std
open Deferred_std

module Deferred = Deferred1

module T = struct
  type ('a, 'error) t = ('a, 'error) Result.t Deferred.t
end

include T

let combine t1 t2 ~ok ~err =
  Deferred.both t1 t2
  >>| fun (t1,t2) ->
  Result.combine t1 t2 ~ok ~err
;;

include Monad.Make2 (struct
  include T

  let return a = Deferred.return (Ok a)

  let bind t f =
    Deferred.bind t (function
      | Ok a -> f a
      | Error _ as error -> Deferred.return error)
  ;;

  let map t ~f = Deferred.map t ~f:(fun r -> Result.map r ~f)
  let map = `Custom map
end)

let ignore = ignore_m

let map_error t ~f = Deferred.map t ~f:(fun r -> Result.map_error r ~f)
