open Core
module Scheduler = Scheduler1
include Deferred0

(* To avoid a space leak, it is necessary that [never] allocates a new ivar whenever it is
   called.  Code can bind on [never ()], so if we re-used the ivar, we could endlessly
   accumulate handlers. *)
let never () = Ivar.read (Ivar.create ())

module M = Monad.Make (struct
  include Deferred0

  let map t ~f =
    (* We manually inline [Deferred.create] here, because the non-flambda compiler isn't
         able to optimize away the closure that would be be created. *)
    let result = Ivar.create () in
    upon t (fun a -> Ivar.fill_exn result (f a));
    of_ivar result
  ;;

  let map = `Custom map
end)

include M

(* We rebind all the various [return]s because the use of the [Monad.Make] functor
   causes the compiler to not inline [return], and hence makes it impossible to
   statically allocate constants like [return ()].  By rebinding [return] as
   [Deferred0.return], the compiler can see that:

   {[
     return a = { Ivar.Immutable. cell = Full a } ]}

   And hence, if [a] is constant, then the return is constant and can be statically
   allocated.  When compiling with flambda, the compiler inlines [return] and this manual
   rebinding would not help; we've decided to do it anyway so that non-flambda builds
   get the optimization. *)
let return = Deferred0.return

module Let_syntax = struct
  include M.Let_syntax

  let return = Deferred0.return

  module Let_syntax = struct
    include M.Let_syntax.Let_syntax

    let return = Deferred0.return
  end
end

open Let_syntax

(* We shadow [all] on-purpose here, since the default definition introduces a chain of
   binds as long as the list. *)
let all = `Make_sure_to_define_all_elsewhere
let _ = all
let unit = return ()

let both t1 t2 =
  create (fun result ->
    upon t1 (fun a1 -> upon t2 (fun a2 -> Ivar.fill_exn result (a1, a2))))
;;

module Infix = struct
  include Monad_infix

  let ( >>> ) = upon
  let ppx_both = both
end

open Infix

let don't_wait_for (_ : unit t) = ()

module Choice = struct
  type +'a t = T : 'b Deferred0.t * ('b -> 'a) -> 'a t

  let map (T (t, f1)) ~f:f2 = T (t, fun x -> f2 (f1 x))
end

module Unregister = struct
  (* This representation saves 2n words for a list of n choices. *)
  type 'r t =
    | Nil : 'r t
    | Cons : 'a Deferred0.t * ('a -> 'r) * 'a Deferred0.Handler.t * 'r t -> 'r t

  let rec process = function
    | Nil -> ()
    | Cons (t, _f, handler, rest) ->
      remove_handler t handler;
      process rest
  ;;
end

let choice t f = Choice.T (t, f)

let enabled choices =
  let result = Ivar.create () in
  let unregisters = ref Unregister.Nil in
  let ready _ =
    if Ivar.is_empty result
    then (
      Unregister.process !unregisters;
      Ivar.fill_exn result (fun () ->
        List.rev
          (List.fold choices ~init:[] ~f:(fun ac (Choice.T (t, f)) ->
             match peek t with
             | None -> ac
             | Some v -> f v :: ac))))
  in
  let execution_context = Scheduler.(current_execution_context (t ())) in
  unregisters
    := List.fold choices ~init:Unregister.Nil ~f:(fun acc (Choice.T (t, f)) ->
         Cons (t, f, Deferred0.add_handler t ready execution_context, acc));
  Ivar.read result
;;

let rec choose_result choices =
  match choices with
  | Unregister.Nil -> assert false
  | Unregister.Cons (t, f, _, rest) ->
    (match peek t with
     | None -> choose_result rest
     | Some v -> f v)
;;

let generic_choose choices =
  let result = Ivar.create () in
  let execution_context = Scheduler.(current_execution_context (t ())) in
  (* A back-patched ref could be used here, but using lazy saves some memory
     because the GC eventually removes the extra indirection. *)
  let rec unregisters =
    lazy
      (List.fold_right choices ~init:Unregister.Nil ~f:(fun (Choice.T (t, f)) acc ->
         Unregister.Cons (t, f, Deferred0.add_handler t ready execution_context, acc)))
  and ready : 'a. 'a -> unit =
    fun _ ->
    if Ivar.is_empty result
    then (
      let unregisters = Lazy.force unregisters in
      Unregister.process unregisters;
      Ivar.fill_exn result (choose_result unregisters))
  in
  let (_ : _) = Lazy.force unregisters in
  Ivar.read result
;;

(* [choose2] is a specialization of [choose] that has better memory usage.
   At the time of writing, [choose2] keeps 22 extra words alive
   for the duration of choice staying undetermined, while the equivalent generic
   [choose] keeps 27 (see the benchmark in ../bench/bin/bench_choose_memory_usage.ml). *)
let choose2 a fa b fb =
  let result = Ivar.create () in
  let execution_context = Scheduler.(current_execution_context (t ())) in
  let rec a_handler = lazy (Deferred0.add_handler a ready execution_context)
  and b_handler = lazy (Deferred0.add_handler b ready execution_context)
  and ready : 'a. 'a -> unit =
    fun _ ->
    if Ivar.is_empty result
    then (
      (* The order of these operations matters:
         if we call [fb] or [fa] first and [remove_handler] after, then
         any exceptions raised will cause the handlers to remain and then
         the "second choice" gets a chance to run.
      *)
      remove_handler a (Lazy.force a_handler);
      remove_handler b (Lazy.force b_handler);
      match peek a with
      | Some av -> Ivar.fill_exn result (fa av)
      | None -> Ivar.fill_exn result (fb (value_exn b)))
  in
  let (_ : _) = Lazy.force a_handler in
  let (_ : _) = Lazy.force b_handler in
  Ivar.read result
;;

let choose choices =
  match choices with
  | [ Choice.T (a, fa); Choice.T (b, fb) ] -> choose2 a fa b fb
  | choices -> generic_choose choices
;;

let any_f ts f = choose (List.map ts ~f:(fun t -> choice t f))
let any ts = any_f ts Fn.id
let any_unit ts = any_f ts (Fn.ignore : unit -> unit)

let for_ start ~to_ ~do_ =
  if start > to_
  then return ()
  else (
    let rec loop i =
      let%bind () = do_ i in
      if i >= to_ then return () else loop (i + 1)
    in
    loop start)
;;

let repeat_until_finished state f =
  create (fun finished ->
    let rec loop state =
      f state
      >>> function
      | `Repeat state -> loop state
      | `Finished result -> Ivar.fill_exn finished result
    in
    loop state)
;;

let forever state f =
  repeat_until_finished state (fun state ->
    let%map state = f state in
    `Repeat state)
  >>> never_returns
;;

type how = Monad_sequence.how [@@deriving sexp_of]

module type Monad_sequence = Monad_sequence.S with type 'a monad := 'a t

let fold t ~init ~f =
  create (fun result ->
    let rec loop t b =
      match t with
      | [] -> Ivar.fill_exn result b
      | x :: xs -> f b x >>> fun b -> loop xs b
    in
    loop t init)
;;

let seqmap t ~f = fold t ~init:[] ~f:(fun bs a -> f a >>| fun b -> b :: bs) >>| List.rev
let all ds = seqmap ds ~f:Fn.id
let all_unit ds = fold ds ~init:() ~f:(fun () d -> d)
let ok x = x >>| fun x -> Ok x

module For_tests = struct
  let generic_choose = generic_choose
end
