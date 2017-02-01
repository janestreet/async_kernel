open! Core_kernel
open! Import

include Synchronous_time_source0

let wall_clock =
  let t =
    lazy (
      let t =
        create ~now:(Time_ns.now ()) () ~wrap_callback:(fun callback ->
          let monitor = Monitor.current () in
          stage (fun () ->
            try
              callback ()
            with exn ->
              Monitor.send_exn monitor exn ~backtrace:`Get))
      in
      let advance ~now =
        match advance_by_alarms t ~to_:now with
        | Ok () -> ()
        | Error error ->
          eprint_s [%message "\
bug in Synchronous_time_source -- callbacks are wrapped and should not raise"
                               (error : Error.t)]
      in
      (Scheduler.t ()).advance_synchronous_wall_clock <- Some advance;
      t)
  in
  fun () -> read_only (force t)
;;
