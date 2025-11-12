open! Core
open! Import

type 'a t =
  { execution_context : Execution_context.t Capsule.Initial.Data.t
  ; ivar : 'a Modes.Portended.t Ivar.t Modes.Aliased.t Capsule.Initial.Data.t
  }

let create () =
  { execution_context =
      Async_kernel_scheduler.current_execution_context () |> Capsule.Initial.Data.wrap
  ; ivar = { aliased = Ivar.create () } |> Capsule.Initial.Data.wrap
  }
;;

type fill_ivar_if_empty = { f : 'a. 'a Ivar.t -> 'a -> unit }

let fill_ivar_if_empty = Capsule.Initial.Data.wrap { f = Ivar.fill_if_empty }

let fill_if_empty { execution_context; ivar } value =
  Async_kernel_scheduler.portable_enqueue_job
    execution_context
    (Capsule.Data.create (fun () : _ ->
       fun #(access, { aliased = ivar }) ->
       let fill_ivar_if_empty = (Capsule.Data.unwrap ~access fill_ivar_if_empty).f in
       fill_ivar_if_empty ivar { portended = value }))
    ivar
;;

let read ({ execution_context = _; ivar } : ('a : value mod contended) t) =
  Ivar.read (Capsule.Initial.Data.unwrap ivar).aliased
  |> Deferred.map ~f:(fun { portended } -> portended)
;;
