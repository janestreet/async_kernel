open! Core_kernel
open! Import

module Deferred = Eager_deferred0

module Deferred_result = Eager_deferred_result

module Monitor = struct
  let try_with = Monitor.try_with ~run:`Now
end

(* Copied from [deferred_or_error.ml].  There should be no diffs below this line. *)

include (Deferred_result : Monad.S2
         with type ('a, 'b) t := ('a, 'b) Deferred_result.t
         with module Let_syntax := Deferred_result.Let_syntax)

type 'a t = 'a Or_error.t Deferred.t

include Applicative.Make (struct
    type nonrec 'a t = 'a t
    let return = return
    let apply f x =
      Deferred_result.combine f x
        ~ok:(fun f x -> f x)
        ~err:(fun e1 e2 -> Error.of_list [e1; e2])
    let map = `Custom map
  end)

module Let_syntax = struct
  let return = return
  include Monad_infix
  module Let_syntax = struct
    let return = return
    let map    = map
    let bind   = bind
    let both   = both (* from Applicative.Make *)
    module Open_on_rhs  = struct end
  end
end

open Let_syntax

let ignore = ignore_m

let fail error = Deferred.return (Result.fail error)

let ok_exn t = Deferred.map t ~f:Or_error.ok_exn

let of_exn exn = Deferred.return (Or_error.of_exn exn)

let of_exn_result t = Deferred.map t ~f:Or_error.of_exn_result

let error msg v sexp_of = Deferred.return (Or_error.error msg v sexp_of)

let error_s sexp = Deferred.return (Or_error.error_s sexp)

let error_string msg = Deferred.return (Or_error.error_string msg)

let errorf format = ksprintf error_string format

let tag t ~tag = Deferred.map t ~f:(Or_error.tag ~tag)

let tag_arg t message a sexp_of_a =
  Deferred.map t ~f:(fun t -> Or_error.tag_arg t message a sexp_of_a)
;;

let unimplemented msg = Deferred.return (Or_error.unimplemented msg)

let combine_errors l =
  Deferred.map (Deferred.all l) ~f:Or_error.combine_errors
;;

let combine_errors_unit l =
  Deferred.map (Deferred.all l) ~f:Or_error.combine_errors_unit
;;

let find_map_ok l ~f =
  Deferred.repeat_until_finished (l, []) (fun (l, errors) ->
    match l with
    | [] ->
      let errors = Error.of_list (List.rev errors) in
      Deferred.return (`Finished (Error errors))
    | hd :: tl ->
      Deferred.map (f hd) ~f:(function
        | Error current_error -> `Repeat   (tl, current_error :: errors)
        | Ok    result        -> `Finished (Ok  result)))
;;

let ok_unit = return ()

let try_with ?extract_exn ?here ?name f =
  Deferred.map (Monitor.try_with ?extract_exn ?here ?name f) ~f:(function
    | Error exn -> Error (Error.of_exn exn)
    | Ok _ as ok -> ok)
;;

let try_with_join ?extract_exn ?here ?name f =
  Deferred.map (try_with ?here ?extract_exn ?name f) ~f:Or_error.join
;;
