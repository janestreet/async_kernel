open Core
include Synchronous_time_source0

let create = Scheduler1.create_time_source
let wall_clock = Scheduler1.wall_clock

let advance_by_alarms_yielding_in_between t ~to_ =
  let send_exn = None in
  prepare_to_advance t ~send_exn;
  let%bind.Deferred () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
  let next_alarm_before ~deadline t =
    match Expert.max_alarm_time_in_min_timing_wheel_interval t with
    | None -> None
    | Some next_event ->
      if Time_ns.( < ) next_event deadline then Some next_event else None
  in
  let rec loop () =
    let next_alarm = next_alarm_before ~deadline:to_ t in
    let to_ = Option.value ~default:to_ next_alarm in
    advance_internal t ~to_ ~send_exn;
    let%bind.Deferred () = Async_kernel_scheduler.yield_until_no_jobs_remain () in
    if Option.is_some next_alarm then loop () else Deferred.return ()
  in
  let%map.Deferred () = loop () in
  finish_advancing t
;;
