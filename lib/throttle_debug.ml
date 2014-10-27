open Core.Std
open Import

module type Throttle = module type of Throttle

module Debug (Throttle : Throttle) : Throttle = struct

  let check_invariant = ref true
  let show_messages   = ref false

  open Throttle

  let debug name ts arg sexp_of_arg sexp_of_result f =
    let invariant = T2.invariant ignore ignore in
    if !show_messages
    then eprints (concat [ "Throttle."; name ]) (ts, arg)
           <:sexp_of< (_, _) T2.t list * arg >>;
    if !check_invariant then List.iter ts ~f:invariant;
    let result_or_exn = Result.try_with f in
    if !show_messages
    then eprints (concat [ "Throttle."; name; "-result" ])
           (result_or_exn, ts)
           <:sexp_of< (result, exn) Result.t * (_, _) T2.t list >>;
    Result.ok_exn result_or_exn;
  ;;

  module T2 = T2

  type nonrec 'a t = 'a t with sexp_of

  type nonrec 'a outcome = 'a outcome with sexp_of

  let capacity_available        = capacity_available
  let cleaned                   = cleaned
  let create                    = create
  let create_with               = create_with
  let invariant                 = invariant
  let is_dead                   = is_dead
  let max_concurrent_jobs       = max_concurrent_jobs
  let num_jobs_running          = num_jobs_running
  let num_jobs_waiting_to_start = num_jobs_waiting_to_start
  let prior_jobs_done           = prior_jobs_done

  let enqueue t f =
    debug "enqueue" [t] () <:sexp_of< unit >> <:sexp_of< _ Deferred.t >>
      (fun () -> enqueue t f)
  ;;

  let enqueue' t f =
    debug "enqueue'" [t] () <:sexp_of< unit >> <:sexp_of< _ outcome Deferred.t >>
      (fun () -> enqueue' t f)
  ;;

  let kill t =
    debug "kill" [t] () <:sexp_of< unit >> <:sexp_of< unit >>
      (fun () -> kill t)
  ;;

  let at_kill t f =
    debug "at_kill" [t] () <:sexp_of< unit >> <:sexp_of< unit >>
      (fun () -> at_kill t f)
  ;;

  module Sequencer = Sequencer
end
