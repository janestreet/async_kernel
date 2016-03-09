open! Core_kernel.Std
open! Import

module Scheduler = Scheduler0

module T1 = struct
  type -'rw t = 'rw Types.Time_source.t1 =
    { events        : Job.t Timing_wheel_ns.t
    (* We store [handle_fired] in [t] to avoid allocating it every time we call
       [advance_clock]. *)
    ; handle_fired  : Job.t Timing_wheel_ns.Alarm.t -> unit
    ; is_wall_clock : bool
    ; scheduler     : Scheduler.t
    }
  [@@deriving fields]

  let sexp_of_t _ { events; handle_fired = _; is_wall_clock; scheduler = _ } =
    if is_wall_clock
    then [%message "<wall_clock>"]
    else [%message (is_wall_clock : bool)
                     (* We don't display the [Job.t]s in [events] because those are
                        pool pointers, which are uninformative. *)
                     (events : _ Timing_wheel_ns.t)]
  ;;

  let invariant (type rw) (t : rw t) =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~events:(check (Timing_wheel_ns.invariant Job.invariant))
        ~handle_fired:ignore
        ~is_wall_clock:ignore
        ~scheduler:ignore)
  ;;
end

open T1

type t = read T1.t [@@deriving sexp_of]

let invariant = invariant

module Read_write = struct
  type t = read_write T1.t [@@deriving sexp_of]

  let invariant = invariant
end
