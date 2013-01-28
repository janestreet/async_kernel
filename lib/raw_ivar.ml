open Core.Std
open Import

module Handler = Raw_handler

type ('a, 'execution_context) t =
  { (* For debugging, we can assign a unique id to each ivar.  But we don't keep it
       around normally, since it costs a word per ivar. *)
(*    id : int; *)
    mutable cell : ('a, 'execution_context) cell;
  }
and ('a, 'execution_context) cell =
| Empty
| Empty_one_handler of ('a -> unit) * 'execution_context
| Empty_many_handlers of ('a, 'execution_context) Handler.t Bag.t
| Full of 'a
(* We maintain an invariant that the directed graph of ivars with [Indir]s as edges is
   acyclic.  The only functions that create an [Indir] are [squash] and [connect], and
   for those, the target of the [Indir] is always a non-[Indir].  Thus, the newly added
   edges are never part of a cycle. *)
| Indir of ('a, 'execution_context) t

type ('a, 'execution_context) ivar = ('a, 'execution_context) t

let equal (t : (_, _) t) t' = phys_equal t t'

(* [Detailed] is used for showing the detailed structure of the ivar graph, including
   indirections.  It is only used for debugging. *)
module Detailed = struct
  type 'a t =
    { (* id : int; *)
      cell : 'a cell;
    }
  and 'a cell =
  | Empty
  | Empty_one_handler
  | Empty_many_handlers of int
  | Full of 'a
  | Indir of 'a t
  with sexp_of
end

let rec detailed { (* id; *) cell } =
  let module D = Detailed in
  let cell =
    match cell with
    | Empty -> D.Empty
    | Empty_one_handler _ -> D.Empty_one_handler
    | Empty_many_handlers bag -> D.Empty_many_handlers (Bag.length bag)
    | Full a -> D.Full a
    | Indir t -> D.Indir (detailed t)
  in
  { D. (* id; *) cell }
;;

let allowed_transition cell cell' =
  match cell, cell' with
  | Indir _              , (Indir _ | Empty | Empty_one_handler _ | Empty_many_handlers _ | Full _)
  | Empty                , (Indir _ | Empty_one_handler _ | Empty_many_handlers _ | Full _)
  | Empty_one_handler _  , (Indir _ | Empty_many_handlers _ | Full _)
  | Empty_many_handlers _, (Indir _ | Full _)
    -> true
  | _, _ -> false
;;

include (struct
  let counter = ref 0
  let create_with_cell cell = (* incr counter; *) { (* id = !counter ; *) cell }
end : sig
  val create_with_cell : ('a, 'execution_context) cell -> ('a, 'execution_context) t
end)

let create () = create_with_cell Empty

let create_full a = create_with_cell (Full a)

(* [squash t] returns the non-[Indir] ivar at the end of the (possibly empty) chain of
   [Indir]s starting with [t] and ensures that all [Indir]s along that chain are replaced
   with an [Indir] pointing to the end of the chain. *)
let squash =
  let rec squash t prev_indir prev_indirs =
    (* [prev_indir.cell = Indir t] *)
    let cell = t.cell in
    match cell with
    | Indir t' -> squash t' t (prev_indir :: prev_indirs)
    | _ ->
      (* [t] is the end of the chain, and [prev_indir] points to it; set all the indirs
         leading up to [prev_indir] to point to [t] too. *)
      let indir = prev_indir.cell in
      List.iter prev_indirs ~f:(fun prev_indir -> prev_indir.cell <- indir);
      t
  in
  fun t ->
    match t.cell with
    | Indir t' ->
      begin match t'.cell with
      | Indir t'' -> squash t'' t' [t]
      | _ ->
        (* Nothing to do, since [t] is a chain with a single [Indir]. *)
        t'
      end
    | _ ->
      (* Nothing to do, since [t] isn't an [Indir]. *)
      t
;;

let sexp_of_t sexp_of_a _ t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> Sexp.List [ Sexp.Atom "Full"; sexp_of_a a ]
  | Empty | Empty_one_handler _ | Empty_many_handlers _ -> Sexp.Atom "Empty"
;;

let sexp_of_ivar = sexp_of_t

let peek t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full a -> Some a
  | Empty | Empty_one_handler _ | Empty_many_handlers _ -> None
;;

let is_empty t =
  let t = squash t in
  match t.cell with
  | Indir _ -> assert false (* fulfilled by [squash] *)
  | Full _ -> false
  | Empty | Empty_one_handler _ | Empty_many_handlers _ -> true
;;

let is_full t = not (is_empty t)

let debug_space_leaks = ref None

let debug_bag_check bag =
  match !debug_space_leaks with
  | None -> ()
  | Some bound -> assert (Bag.length bag < bound)
;;

module Scheduler_dependent (Scheduler : Basic_scheduler) = struct
  type 'a t = ('a, Scheduler.Execution_context.t) ivar with sexp_of

  type 'a detailed = 'a t

  let sexp_of_detailed sexp_of_a t = Detailed.sexp_of_t sexp_of_a (detailed t)

  let add_jobs_for_handlers handlers v =
    Bag.iter handlers ~f:(fun handler -> Scheduler.add_job (Handler.create_job handler v))
  ;;

  let fill t v =
    let t = squash t in
    match t.cell with
    | Indir _ -> assert false (* fulfilled by [squash] *)
    | Full _ -> failwiths "Ivar.fill of full ivar" t <:sexp_of< a t >>
    | Empty -> t.cell <- Full v;
    | Empty_one_handler (run, execution_context) ->
      t.cell <- Full v;
      Scheduler.add_job (Job.create execution_context run v);
    | Empty_many_handlers handlers ->
      t.cell <- Full v;
      add_jobs_for_handlers handlers v;
  ;;

  let install_removable_handler =
    let add_to_bag t bag handler =
      assert (match t.cell with Empty_many_handlers _ -> true | _ -> false);
      let elt = Bag.add bag handler in
      debug_bag_check bag;
      Unregister.create (fun () ->
        (* [t] may have redirected since we first called [add_to_bag], so we [squash] it
           again. *)
        let t = squash t in
        match t.cell with
        | Indir _ -> assert false (* fulfilled by [squash] *)
        | Empty_many_handlers bag' -> Bag.remove bag' elt
        | Full _ -> ()
        (* The cell was [Empty_many_handlers] before calling [add_to_bag], so it
           could not have transitioned back to [Empty] or [Empty_one_handler]. *)
        | Empty | Empty_one_handler _ -> assert false
      );
    in
    fun t handler ->
      let t = squash t in
      match t.cell with
      | Indir _ -> assert false (* fulfilled by [squash] *)
      | Empty ->
        let bag = Bag.create () in
        t.cell <- Empty_many_handlers bag;
        add_to_bag t bag handler;
      | Empty_one_handler (run, execution_context) ->
        let bag = Bag.create () in
        t.cell <- Empty_many_handlers bag;
        ignore (Bag.add bag { Handler. execution_context; run });
        add_to_bag t bag handler;
      | Empty_many_handlers bag -> add_to_bag t bag handler
      | Full v ->
        let still_installed = ref true in
        Scheduler.add_job (Handler.create_job
                             (Handler.filter handler ~f:(fun _ -> !still_installed))
                             v);
        Unregister.create (fun () -> still_installed := false);
  ;;

  let upon' t run =
    install_removable_handler t
      { Handler.
        execution_context = Scheduler.current_execution_context ();
        run;
      }
  ;;

  (* [upon] is conceptually the same as

     let upon t f = ignore (upon' t run)

     However, below is a more efficient implementation, which is worth doing because
     [upon] is very widely used and is so much more common than [upon'].  The below
     implementation avoids the use of the bag of handlers in the extremely common case of
     one handler for the deferred. *)
  let upon =
    fun t run ->
      let execution_context = Scheduler.current_execution_context () in
      let t = squash t in
      match t.cell with
      | Indir _ -> assert false (* fulfilled by [squash] *)
      | Full v -> Scheduler.add_job (Job.create execution_context run v)
      | Empty -> t.cell <- Empty_one_handler (run, execution_context)
      | Empty_many_handlers bag ->
        ignore (Bag.add bag { Handler. run; execution_context })
      | Empty_one_handler (run', execution_context') ->
        let bag = Bag.create () in
        ignore (Bag.add bag { Handler. run; execution_context });
        ignore (Bag.add bag { Handler. run = run'; execution_context = execution_context' });
        t.cell <- Empty_many_handlers bag;
  ;;

  (* [connect] takes ivars [bind_result] and [bind_rhs], and makes [bind_rhs]
     be an [Indir] pointing to the non-indir cell reachable from [bind_result].  On entry
     to [connect], [bind_result] and [bind_rhs] may be chains, since [bind_rhs] is an
     arbitrary user-supplied deferred, and [bind_result] is returned to the user prior to
     being [connect]ed, and may have been converted to an indirection in the case of
     right-nested binds.

     The purpose of [connect] is to make tail-recursive bind loops use constant space.
     E.g.

     let rec loop i =
       if i = 0
       then return ()
       else after (sec 1.) >>= fun () -> loop (i -1)

     [connect] makes intermediate bind results all be [Indir]s pointing at the outermost
     bind, rather than being a linear-length chain, with each pointing to the previous
     one.  Then, since the program is only holding on to the innermost and outermost binds
     all the intermediate ones can be garbage collected.

     [connect] works by squashing its arguments so that the [bind_rhs] always points
     at the ultimate result. *)
  let connect ~bind_result ~bind_rhs =
    if not (phys_equal bind_result bind_rhs) then begin
      let bind_result = squash bind_result in
      let indir = Indir bind_result in
      (* [repoint_indirs bind_rhs] repoints to [indir] all the ivars in the chain
         reachable from [bind_rhs], and returns the non-[Indir] cell at the end of the
         chain.  After repointing, we will merge the handlers in that cell with the
         handlers in [bind_result], and put the merged set of handlers in
         [bind_result]. *)
      let rec repoint_indirs ivar =
        let cell = ivar.cell in
        match cell with
        | Indir ivar' -> ivar.cell <- indir; repoint_indirs ivar'
        | Full _ -> cell
        | Empty _ | Empty_one_handler _ | Empty_many_handlers _ ->
          (* It is possible that [bind_result] and [bind_rhs] are not equal, but their
             chains of indirs lead to the same non-[Indir] cell, in which case we cannot
             set that cell to point to itself, because that would introduce a cycle. *)
          if not (phys_equal ivar bind_result) then ivar.cell <- indir;
          cell
      in
      let bind_rhs_contents = repoint_indirs bind_rhs in
      (* update [bind_result] with the union of handlers in [bind_result] and
         [bind_rhs] *)
      match bind_result.cell, bind_rhs_contents with
      | Indir _, _ | _, Indir _
        -> assert false (* fulfilled by [squash] and [point_to_indir] *)
      (* [connect] is only used in bind, whose ivar is only ever exported as a read-only
         deferred.  Thus, [bind_result] must be empty. *)
      | Full _, _ -> assert false
      | _, Empty -> ()
      | Empty, _ -> bind_result.cell <- bind_rhs_contents;
      | Empty_one_handler (run, execution_context), Full v ->
        bind_result.cell <- bind_rhs_contents;
        Scheduler.add_job (Job.create execution_context run v);
      | Empty_many_handlers handlers, Full v ->
        bind_result.cell <- bind_rhs_contents;
        add_jobs_for_handlers handlers v;
      | Empty_many_handlers bag, Empty_one_handler (run, execution_context) ->
        (* No need to set [bind_result], since it's already a bag. *)
        ignore (Bag.add bag { Handler. run; execution_context });
        debug_bag_check bag;
      | Empty_many_handlers bag_bind_result, Empty_many_handlers bag_bind_rhs ->
        (* No need to set [bind_result], since it's already a bag. *)
        Bag.transfer ~src:bag_bind_rhs ~dst:bag_bind_result;
        debug_bag_check bag_bind_rhs;
      | Empty_one_handler (run, execution_context), Empty_many_handlers bag_bind_rhs ->
        (* We already have a bag; move it over to [bind_result]. *)
        bind_result.cell <- bind_rhs_contents;
        let handler = { Handler. run; execution_context } in
        ignore (Bag.add bag_bind_rhs handler);
        debug_bag_check bag_bind_rhs;
      | Empty_one_handler (run1, ec1), Empty_one_handler (run2, ec2) ->
        (* We don't have a bag; create one. *)
        let bag = Bag.create () in
        bind_result.cell <- Empty_many_handlers bag;
        ignore (Bag.add bag { Handler. run = run1; execution_context = ec1});
        ignore (Bag.add bag { Handler. run = run2; execution_context = ec2});
        debug_bag_check bag;
    end
  ;;
end

TEST_MODULE = struct
  include Scheduler_dependent (struct
    module Execution_context = Unit
    let add_job job = Job.run job
    let current_execution_context () = ()
  end)

  let (-->) i1 i2 =
    match i1.cell with
    | Indir i2' -> phys_equal i2' i2
    | _ -> false
  ;;

  let r = ref 0
  let run i = r := !r + i
  let execution_context = ()
  let empty_one_handler = Empty_one_handler (run, execution_context)
  let handler1 = { Handler. execution_context; run }
  let handler2 = { Handler. execution_context; run }

  let squash t = ignore (squash t)

  let connect bind_result bind_rhs = connect ~bind_result ~bind_rhs

  (* ==================== peek, is_empty, is_full ==================== *)

  TEST_UNIT =
    let t = create () in
    assert (is_empty t);
    assert (not (is_full t));
    assert (peek t = None);
  ;;

  TEST_UNIT =
    let t = create_full 13 in
    assert (not (is_empty t));
    assert (is_full t);
    assert (peek t = Some 13);
  ;;

  (* ==================== equal ==================== *)

  TEST_UNIT =
    let t1 = create () in
    let t2 = create () in
    assert (equal t1 t1);
    assert (equal t2 t2);
    assert (not (equal t1 t2));
  ;;

  (* ==================== squash ==================== *)

  TEST_UNIT =
    let t = create () in
    squash t;
    assert (t.cell = Empty);
  ;;

  TEST_UNIT =
    let t1 = create () in
    let t2 = create_with_cell (Indir t1) in
    squash t2;
    assert (t2 --> t1);
  ;;

  TEST_UNIT =
    let t1 = create () in
    let t2 = create_with_cell (Indir t1) in
    let t3 = create_with_cell (Indir t2) in
    let t4 = create_with_cell (Indir t3) in
    squash t4;
    assert (t2 --> t1);
    assert (t3 --> t1);
    assert (t4 --> t1);
  ;;

  (* ==================== fill ==================== *)

  TEST_UNIT =
    let t = create () in
    fill t 13;
    assert (peek t = Some 13);
  ;;

  TEST_UNIT =
    let t = create () in
    fill t 13;
    assert (try fill t 14; false with _ -> true);
  ;;

  TEST_UNIT =
    let t1 = create () in
    let t2 = create_with_cell (Indir t1) in
    fill t2 13;
    assert (peek t1 = Some 13);
    assert (peek t2 = Some 13);
  ;;

  TEST_UNIT =
    r := 13;
    let t = create_with_cell empty_one_handler in
    fill t 17;
    assert (t.cell = Full 17);
    assert (!r = 30);
  ;;

  TEST_UNIT =
    r := 13;
    let t = create_with_cell (Empty_many_handlers (Bag.of_list [ handler1; handler2])) in
    fill t 17;
    assert (t.cell = Full 17);
    assert (!r = 47);
  ;;

  (* ==================== upon ==================== *)

  TEST_UNIT =
    let t = create () in
    r := 1;
    upon t (fun i -> r := !r + i);
    assert (!r = 1);
    fill t 13;
    assert (!r = 14);
  ;;

  TEST_UNIT =
    let t = create () in
    r := 1;
    upon t (fun i -> r := !r + i);
    upon t (fun i -> r := !r + i);
    assert (!r = 1);
    fill t 13;
    assert (!r = 27);
  ;;

  TEST_UNIT =
    let t = create () in
    r := 1;
    let num_handlers = 1000 in
    for i = 1 to num_handlers do
      upon t (fun i -> r := !r + i);
    done;
    assert (!r = 1);
    fill t 13;
    assert (!r = num_handlers * 13 + 1);
  ;;

  TEST_UNIT =
    let t1 = create () in
    let t2 = create_with_cell (Indir t1) in
    r := 1;
    upon t2 (fun i -> r := !r + i);
    fill t1 13;
    assert (!r = 14);
  ;;

  (* ==================== upon' ==================== *)

  TEST_UNIT =
    let t = create () in
    r := 1;
    let u = upon' t (fun i -> r := !r + i) in
    assert (!r = 1);
    Unregister.unregister u;
    fill t 13;
    assert (!r = 1);
  ;;

  TEST_UNIT =
    let t = create () in
    r := 1;
    let u = upon' t (fun i -> r := !r + i) in
    assert (!r = 1);
    fill t 13;
    assert (!r = 14);
    Unregister.unregister u;
    assert (!r = 14);
  ;;

  TEST_UNIT =
    let t = create () in
    r := 1;
    let u1 = upon' t (fun i -> r := !r + i) in
    let _  = upon' t (fun i -> r := !r + i) in
    assert (!r = 1);
    Unregister.unregister u1;
    fill t 13;
    assert (!r = 14);
  ;;

  TEST_UNIT =
    let t1 = create () in
    let t2 = create () in
    r := 1;
    let u1 = upon' t1 (fun () -> r := !r + 13) in
    let _ = upon' t2 (fun () -> r := !r + 17) in
    connect t1 t2;
    Unregister.unregister u1;
    fill t1 ();
    assert (!r = 18);
  ;;

  (* ==================== connect ==================== *)

  TEST_UNIT =
    let i1 = create () in
    let i2 = create () in
    connect i1 i2;
    assert (i1.cell = Empty);
    assert (i2 --> i1);
  ;;

  TEST_UNIT =
    let a1 = create () in
    let b1 = create () in
    let b2 = create_with_cell (Indir b1) in
    connect a1 b2;
    assert (a1.cell = Empty);
    assert (b1 --> a1);
    assert (b2 --> a1);
  ;;

  TEST_UNIT =
    let a1 = create () in
    let a2 = create_with_cell (Indir a1) in
    let b1 = create () in
    let b2 = create_with_cell (Indir b1) in
    connect a2 b2;
    assert (a1.cell = Empty);
    assert (a2 --> a1);
    assert (b1 --> a1);
    assert (b2 --> a1);
  ;;

  TEST_UNIT =
    let a = create () in
    let b = create_with_cell (Indir a) in
    let c = create_with_cell (Indir a) in
    connect b c;
    assert (a.cell = Empty);
    assert (b --> a);
    assert (c --> a);
  ;;

  TEST_UNIT =
    let a1 = create () in
    connect a1 a1;
    assert (a1.cell = Empty);
  ;;

  TEST_UNIT =
    let a1 = create () in
    let a2 = create_with_cell (Indir a1) in
    connect a1 a2;
    assert (a1.cell = Empty);
    assert (a2 --> a1);
  ;;

  TEST_UNIT =
    let a1 = create () in
    let a2 = create_with_cell (Indir a1) in
    connect a2 a1;
    assert (a1.cell = Empty);
    assert (a2 --> a1);
  ;;

  TEST_UNIT =
    let a1 = create () in
    let b1 = create_with_cell empty_one_handler in
    connect a1 b1;
    assert (phys_equal a1.cell empty_one_handler);
    assert (b1 --> a1);
  ;;

  TEST_UNIT =
    let a1 = create_with_cell empty_one_handler in
    let b1 = create () in
    connect a1 b1;
    assert (phys_equal a1.cell empty_one_handler);
    assert (b1 --> a1);
  ;;

  TEST_UNIT =
    let a1 = create_with_cell empty_one_handler in
    let b1 = create_with_cell empty_one_handler in
    connect a1 b1;
    begin match a1.cell with
    | Empty_many_handlers bag ->
      assert (Bag.length bag = 2);
      Bag.iter bag ~f:(fun handler ->
        assert (phys_equal handler.Handler.execution_context execution_context);
        assert (phys_equal handler.Handler.run run));
    | _ -> assert false
    end;
    assert (b1 --> a1);
  ;;

  TEST_UNIT =
    let a1 = create () in
    let empty_many_handlers = Empty_many_handlers (Bag.create ()) in
    let b1 = create_with_cell empty_many_handlers in
    connect a1 b1;
    assert (phys_equal a1.cell empty_many_handlers);
    assert (b1 --> a1);
  ;;

  TEST_UNIT =
    let empty_many_handlers = Empty_many_handlers (Bag.create ()) in
    let a1 = create_with_cell empty_many_handlers in
    let b1 = create () in
    connect a1 b1;
    assert (phys_equal a1.cell empty_many_handlers);
    assert (b1 --> a1);
  ;;

  TEST_UNIT =
    let bag1 = Bag.of_list [ handler1 ] in
    let empty_many_handlers1 = Empty_many_handlers bag1 in
    let a1 = create_with_cell empty_many_handlers1 in
    let b1 = create_with_cell (Empty_many_handlers (Bag.of_list [ handler2 ])) in
    connect a1 b1;
    assert (phys_equal a1.cell empty_many_handlers1);
    begin match a1.cell with
    | Empty_many_handlers bag ->
      begin match Bag.to_list bag with
      | [ h1; h2 ] ->
        assert (phys_equal h1 handler1 && phys_equal h2 handler2
                || phys_equal h1 handler2 && phys_equal h2 handler1);
      | _ -> assert false
      end
    | _ -> assert false
    end;
    assert (b1 --> a1);
  ;;

  TEST_UNIT =
    let bag1 = Bag.of_list [ handler1 ] in
    let empty_many_handlers1 = Empty_many_handlers bag1 in
    let a1 = create_with_cell empty_many_handlers1 in
    let b1 = create_with_cell empty_one_handler in
    connect a1 b1;
    assert (phys_equal a1.cell empty_many_handlers1);
    begin match a1.cell with
    | Empty_many_handlers bag ->
      begin match Bag.to_list bag with
      | [ h1; h2 ] ->
        assert (phys_equal h1 handler1 || phys_equal h2 handler1);
      | _ -> assert false
      end
    | _ -> assert false
    end;
    assert (b1 --> a1);
  ;;

  TEST_UNIT =
    let i1 = create () in
    let i2 = create_with_cell (Full 13) in
    connect i1 i2;
    assert (i1.cell = Full 13);
    assert (i2.cell = Full 13);
  ;;

  TEST_UNIT =
    let a1 = create () in
    let b1 = create_with_cell (Full 13) in
    let b2 = create_with_cell (Indir b1) in
    connect a1 b2;
    assert (a1.cell = Full 13);
    assert (b1.cell = Full 13);
    assert (b2 --> a1);
  ;;

  TEST_UNIT =
    let a1 = create_with_cell empty_one_handler in
    let b1 = create_with_cell (Full 13) in
    connect a1 b1;
    assert (a1.cell = Full 13);
    assert (b1.cell = Full 13);
  ;;

  TEST_UNIT =
    let a1 = create_with_cell (Empty_many_handlers (Bag.create ())) in
    let b1 = create_with_cell (Full 13) in
    connect a1 b1;
    assert (a1.cell = Full 13);
    assert (b1.cell = Full 13);
  ;;

end
