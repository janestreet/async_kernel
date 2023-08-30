open! Core
open! Import
open! Deferred_std
module Deferred = Deferred1

module Counting_semaphore : sig
  type t

  val wait_to_acquire_job_token : t -> unit Deferred.t
  val release_job_token : t -> unit
  val abort : t -> unit
  val create : max_concurrent_jobs:int -> t
end = struct
  type t =
    { mutable max_concurrent_jobs : int
    ; mutable waiter : unit Ivar.t option
    ; mutable aborted : bool
    }

  let wait_to_acquire_job_token ({ max_concurrent_jobs; waiter; aborted } as t) =
    match aborted with
    | true -> Deferred.never ()
    | false ->
      if max_concurrent_jobs > 0
      then (
        t.max_concurrent_jobs <- max_concurrent_jobs - 1;
        Deferred.return ())
      else (
        assert (Option.is_none waiter);
        let ivar = Ivar.create () in
        t.waiter <- Some ivar;
        Ivar.read ivar)
  ;;

  let release_job_token ({ max_concurrent_jobs; waiter; aborted = _ } as t) =
    match waiter with
    | Some ivar ->
      Ivar.fill_exn ivar ();
      t.waiter <- None
    | None -> t.max_concurrent_jobs <- max_concurrent_jobs + 1
  ;;

  let abort t =
    t.aborted <- true;
    t.waiter <- None
  ;;

  let create ~max_concurrent_jobs =
    { max_concurrent_jobs; waiter = None; aborted = false }
  ;;
end

module T = struct
  type 'a t =
    { compute :
        Execution_context.t -> Counting_semaphore.t -> ('a Deferred.t -> unit) -> unit
    }
  [@@unboxed]

  let return x = { compute = (fun _ _ k -> k (return x)) }

  let map =
    `Custom
      (fun t ~f ->
        { compute =
            (fun exec_ctx semaphore k ->
              t.compute exec_ctx semaphore (fun d ->
                k
                  (let%map result = d in
                   f result)))
        })
  ;;

  let apply t_f t =
    { compute =
        (fun exec_ctx semaphore k ->
          t_f.compute exec_ctx semaphore (fun df ->
            t.compute exec_ctx semaphore (fun dv ->
              k
                (let%bind f = df in
                 let%map v = dv in
                 f v))))
    }
  ;;
end

include T
include Applicative.Make (T)

let enqueue' scheduler ctx f =
  let ivar = Ivar.create () in
  Scheduler.enqueue scheduler ctx (fun () -> upon (f ()) (Ivar.fill_exn ivar)) ();
  Ivar.read ivar
;;

let job f =
  { compute =
      (fun exec_ctx semaphore k ->
        Deferred.upon (Counting_semaphore.wait_to_acquire_job_token semaphore) (fun () ->
          k
            (enqueue' (Scheduler.t ()) exec_ctx (fun () ->
               let%map a = f () in
               Counting_semaphore.release_job_token semaphore;
               a))))
  }
;;

let run t ~max_concurrent_jobs =
  let semaphore = Counting_semaphore.create ~max_concurrent_jobs in
  (* The name is set to the empty string in order to prevent [Monitor.send_exn]
     from appending information about this monitor to the exceptions we forward.
     This matters because we want simliar behavior to [Throttle] and not break
     existing tests. *)
  let monitor = Monitor.create ~name:"" () in
  let parent_monitor = Monitor.current () in
  Monitor.detach_and_iter_errors monitor ~f:(fun err ->
    Counting_semaphore.abort semaphore;
    Monitor.send_exn parent_monitor err);
  let exec_ctx =
    Execution_context.create_like
      ~monitor
      (Scheduler.current_execution_context (Scheduler.t ()))
  in
  let ivar = Ivar.create () in
  t.compute exec_ctx semaphore (fun r -> Deferred.upon r (Ivar.fill_exn ivar));
  Ivar.read ivar
;;

let of_thunk thunk =
  { compute =
      (fun exec_ctx semaphore k ->
        let t = thunk () in
        t.compute exec_ctx semaphore k)
  }
;;

let ( *> ) t1 t2 =
  { compute =
      (fun exec_ctx semaphore k ->
        t1.compute exec_ctx semaphore (fun d1 ->
          t2.compute exec_ctx semaphore (fun d2 ->
            k
              (let%bind () = d1 in
               d2))))
  }
;;

let both_unit = ( *> )
