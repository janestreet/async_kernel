open! Core
open! Import

type ('a, 'permissions) t =
  { execution_context : Execution_context.t Capsule.Initial.Data.t
  ; bvar :
      ('a Modes.Portended.t, read_write) Bvar.t Modes.Aliased.t Capsule.Initial.Data.t
  }

let create () =
  { execution_context =
      Async_kernel_scheduler.current_execution_context () |> Capsule.Initial.Data.wrap
  ; bvar = { aliased = Bvar.create () } |> Capsule.Initial.Data.wrap
  }
;;

type bvar_broadcast = { f : 'a 'b. ('a, ([> write ] as 'b)) Bvar.t -> 'a -> unit }

let bvar_broadcast = Capsule.Initial.Data.wrap { f = Bvar.broadcast }

let broadcast { execution_context; bvar } value =
  Async_kernel_scheduler.portable_enqueue_job
    execution_context
    (Capsule.Data.create (fun () #(access, { aliased = bvar }) : _ ->
       let bvar_broadcast = (Capsule.Data.unwrap ~access bvar_broadcast).f in
       bvar_broadcast bvar { portended = value }))
    bvar
;;

let wait ({ execution_context = _; bvar } : ('a : value mod contended, _) t) =
  Bvar.wait (Capsule.Initial.Data.unwrap bvar).aliased
  |> Deferred.map ~f:(fun { portended } -> portended)
;;
