module Debug_in_this_directory = Debug

open Core.Std  let _ = _squelch_unused_module_warning_

module Debug = Debug_in_this_directory

module Epoll_max_ready_events              = Config.Epoll_max_ready_events
module Max_inter_cycle_timeout             = Config.Max_inter_cycle_timeout
module Max_num_open_file_descrs            = Config.Max_num_open_file_descrs
module Max_num_threads                     = Config.Max_num_threads
module Max_num_jobs_per_priority_per_cycle = Config.Max_num_jobs_per_priority_per_cycle

let concat = String.concat

let eprints = Core.Std.Debug.eprints
let eprint  = Core.Std.Debug.eprint

let _squelch_unused_module_warning_ = ()
