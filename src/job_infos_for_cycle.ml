open! Core
open! Import

type t =
  { start_times : Time_stamp_counter.t Queue.t
  ; end_times : Time_stamp_counter.t Queue.t
  }
[@@deriving fields ~iterators:iter, sexp_of]

let latest_cycle_start_times t = t.start_times
let latest_cycle_end_times t = t.end_times
let create () = { start_times = Queue.create (); end_times = Queue.create () }

let[@inline always] before_job_run t =
  [%probe
    "async_record_job_times"
      (let start_time = Time_stamp_counter.now () in
       Queue.enqueue t.start_times start_time)]
;;

let[@inline always] after_job_finished t =
  [%probe
    "async_record_job_times"
      (let end_time = Time_stamp_counter.now () in
       Queue.enqueue t.end_times end_time)]
;;

let[@inline always] on_exception_in_run_jobs t =
  (* If a job raised, we will have recorded the start time but not the end time, so we add
     the end time in the exception handler. *)
  [%probe
    "async_record_job_times"
      (let missing_end_times_due_to_jobs_that_raised =
         Queue.length t.start_times - Queue.length t.end_times
       in
       match missing_end_times_due_to_jobs_that_raised with
       | 1 ->
         let end_time = Time_stamp_counter.now () in
         Queue.enqueue t.end_times end_time
       | missing_end_times ->
         (* <=0 missing end times should never happen. >1 missing end times is only
            possible if a job calls [Job_queue.run_jobs] (and then a job raises) which we
            don't expect to happen. *)
         failwithf
           "Job_queue.add_missing_end_times_for_jobs_that_raised bug: unexpected number \
            of end_times missing: %d"
           missing_end_times
           ())]
;;

let on_cycle_start t =
  [%probe
    "async_record_job_times"
      (let clear_queue field = Queue.clear (Field.get field t) in
       Fields.iter ~start_times:clear_queue ~end_times:clear_queue)]
;;

let invariant t = assert (Queue.length t.end_times <= Queue.length t.start_times)

module Private = struct
  let before_job_run = before_job_run
  let after_job_finished = after_job_finished
  let on_exception_in_run_jobs = on_exception_in_run_jobs
  let on_cycle_start = on_cycle_start
end
