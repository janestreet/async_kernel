module Time_ns_in_this_directory = Time_ns
open Core

(* module Time_ns = Time_ns_in_this_directory *)
include Async_kernel_config.Print_debug_messages_for

(* Calls [Async_kernel_config.task_id] if on the main domain,
   and returns a dummy sexp otherwise *)
let task_id =
  let async_kernel_config_task_id =
    Capsule.Initial.Data.wrap Async_kernel_config.task_id
  in
  fun () ->
    Basement.Stdlib_shim.Domain.Safe.DLS.access (fun access ->
      match Capsule.Expert.get_initial access with
      | Some access ->
        !(Capsule.Expert.Data.unwrap ~access async_kernel_config_task_id) ()
      | None -> Sexp.Atom "<not initial domain>")
;;

include struct
  module Time_ns = struct
    include Time_ns_in_this_directory

    let sexp_of_t = Basement.Stdlib_shim.Obj.magic_portable sexp_of_t
    let now = Basement.Stdlib_shim.Obj.magic_portable now
  end
end

let log message a sexp_of_a =
  prerr_endline
    (Sexp.to_string_hum
       ([%sexp_of: Sexp.t * Time_ns.t * string * a]
          (task_id (), Time_ns.now (), message, a)))
;;

let log_string message = log message () [%sexp_of: unit]
