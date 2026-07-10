open Core
module Deferred = Deferred1

module T = struct
  type 'a t = 'a Option.t Deferred.t
end

include T

include Monad.Make (struct
    include T

    let return a = Deferred.return (Some a)

    let bind t ~f =
      Deferred.bind t ~f:(function
        | Some a -> f a
        | None -> Deferred.return None)
    ;;

    let map t ~f = Deferred.map t ~f:(fun r -> Option.map r ~f)
    let map = `Custom map
  end)

module Container = struct
  (* Every function below is some instance of deferred [Option.value_map]. *)

  let value_map option ~default ~f =
    match option with
    | None -> Deferred.return default
    | Some value -> f value
  ;;

  let fold option ~init ~f = value_map option ~default:init ~f:(fun value -> f init value)
  let exists option ~f = value_map option ~default:false ~f
  let for_all option ~f = value_map option ~default:true ~f
  let iter option ~f = value_map option ~default:() ~f

  let map option ~f =
    value_map option ~default:None ~f:(fun value ->
      Deferred.map ~f:Option.return (f value))
  ;;

  let all option =
    value_map option ~default:None ~f:(fun deferred ->
      Deferred.map ~f:Option.return deferred)
  ;;

  let all_unit option = value_map option ~default:() ~f:Fn.id

  (* Several exported functions are equivalent to a deferred [Option.bind]. *)

  let bind option ~f = value_map option ~default:None ~f
  let concat_map = bind
  let filter_map = bind
  let find_map = bind

  (* [find] and [filter] are equivalent. *)

  let find option ~f =
    value_map option ~default:None ~f:(fun value ->
      Deferred.map (f value) ~f:(function
        | false -> None
        | true -> Some value))
  ;;

  let filter = find
end
