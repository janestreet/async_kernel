open! Core
open! Import
module Deferred_array = Eager_deferred_array.Array
open Eager_deferred0

module Iarray = struct
  (* All uses of [unsafe_to_array] are of the form
     [unsafe_to_array t |> func <args?> <~f?>] or
     [unsafe_to_array |> func <args?> <~f?> >>| unsafe_of_array]. In all cases:
     1) [func] does not mutate the input [array], nor hold a reference to it;
     2) [func] never gives [f] (if relevant) a reference to the whole [array];
     3) [func] never returns the original [array].

     These together ensure it is safe to cast the [iarray] into an [array]. *)
  let unsafe_to_array = Iarray.unsafe_to_array__promise_no_mutation

  (* All uses of [unsafe_of_array] are on [array]s which were just created by a function
     which does not hold a reference to the [array], nor give [f] (if relevant) a
     reference to the whole [array]. This means we have a unique reference to the [array],
     so it's safe to cast it into an [iarray]. *)
  let unsafe_of_array = Iarray.unsafe_of_array__promise_no_mutation

  (* All functions have identical behavior to their [Eager_deferred_array] counter-part. *)

  let fold t ~init ~f = unsafe_to_array t |> Deferred_array.fold ~init ~f
  let foldi t ~init ~f = unsafe_to_array t |> Deferred_array.foldi ~init ~f
  let find t ~f = unsafe_to_array t |> Deferred_array.find ~f
  let findi t ~f = unsafe_to_array t |> Deferred_array.findi ~f
  let find_map t ~f = unsafe_to_array t |> Deferred_array.find_map ~f
  let find_mapi t ~f = unsafe_to_array t |> Deferred_array.find_mapi ~f
  let exists t ~f = unsafe_to_array t |> Deferred_array.exists ~f
  let existsi t ~f = unsafe_to_array t |> Deferred_array.existsi ~f
  let for_all t ~f = unsafe_to_array t |> Deferred_array.for_all ~f
  let for_alli t ~f = unsafe_to_array t |> Deferred_array.for_alli ~f
  let all t = unsafe_to_array t |> Deferred_array.all >>| unsafe_of_array
  let all_unit t = unsafe_to_array t |> Deferred_array.all_unit
  let init ~how n ~f = Deferred_array.init ~how n ~f >>| unsafe_of_array
  let iter ~how t ~f = unsafe_to_array t |> Deferred_array.iter ~how ~f
  let iteri ~how t ~f = unsafe_to_array t |> Deferred_array.iteri ~how ~f
  let map ~how t ~f = unsafe_to_array t |> Deferred_array.map ~how ~f >>| unsafe_of_array

  let mapi ~how t ~f =
    unsafe_to_array t |> Deferred_array.mapi ~how ~f >>| unsafe_of_array
  ;;

  let filter ~how t ~f =
    unsafe_to_array t |> Deferred_array.filter ~how ~f >>| unsafe_of_array
  ;;

  let filteri ~how t ~f =
    unsafe_to_array t |> Deferred_array.filteri ~how ~f >>| unsafe_of_array
  ;;

  let filter_map ~how t ~f =
    unsafe_to_array t |> Deferred_array.filter_map ~how ~f >>| unsafe_of_array
  ;;

  let filter_mapi ~how t ~f =
    unsafe_to_array t |> Deferred_array.filter_mapi ~how ~f >>| unsafe_of_array
  ;;

  let concat_map ~how t ~f = map t ~how ~f >>| Iarray.concat
  let concat_mapi ~how t ~f = mapi t ~how ~f >>| Iarray.concat
end
