open Core.Std
open Import

module Priority = Linux_ext.Priority

let priority_zero = Priority.of_int 0

let getpriority =
  match Linux_ext.getpriority with
  | Error _ -> const priority_zero
  | Ok f -> f
;;

let setpriority =
  match Linux_ext.setpriority with
  | Error _ -> Fn.ignore
  | Ok f -> f
;;

let set_thread_name =
  match Linux_ext.pr_set_name_first16 with
  | Ok f -> f
  | Error _ -> Fn.ignore
;;

(* We define everything in an [Internal] module, and then wrap in a
   [Mutex.critical_section] each thread-safe function exposed in the mli. *)
module Internal = struct
  module Mutex = Nano_mutex

  let debug = Debug.thread_pool
  let check_invariant = ref false

  let error = Or_error.error

  module Pool_id : Unique_id = Unique_id.Int63 (Unit)

  module Work_ = struct
    (* [work] is polymorphic in ['work_group] because of a type cycle. *)
    type 'work_group t_ =
      { (* When this work starts running, the name of the thread will be set
           (via [Linux_ext.pr_set_name]) to [name]. *)
        name : string;
        work_group : 'work_group;
        doit : unit -> unit;
        priority : Priority.t;
      }
    with sexp_of
  end

  module Work_group = struct
    type t =
      { (* A work group is created in a particular thread pool, identified by [in_pool],
           and must only be used with that pool. *)
        in_pool : Pool_id.t;
        mutable is_in_use : bool;
        (* The thread pool guarantees that requests to have threads assigned to this
           work group will be met by at least [min_assignable_threads] threads. *)
        min_assignable_threads : int;
        (* The thread pool will never assign more than [max_assigned_threads] to the work
           group. *)
        max_assigned_threads : int;
        (* [num_assigned_threads] says how many threads in the pool are assigned to
           work for this group.  The pool guarantees that:

           |  0 <= num_assigned_threads <= max_assigned_threads *)
        mutable num_assigned_threads : int;
        (* [unfinished_work] says how many pieces of work have been submitted to the group
           that have not yet been completed (including work for helper threads in the
           group).  This is at least as large as [Queue.length work_queue], but typically
           is greater because it also includes work that threads are doing. *)
        mutable unfinished_work : int;
        (* [work_queue] holds work to be done in this group for which no thread has been
           assigned.  The [sexp_opaque] is to prevent infinite unwinding when converting
           to a sexp. *)
        work_queue : t sexp_opaque Work_.t_ Queue.t;
        (* [work_groups_elt] holds the element of the pool's [work_groups] bag for this
           group.  It is [Some] until the user calls [finished_with_work_group]. The
           [sexp_opaque] is to prevent infinite unwinding when converting to a sexp. *)
        mutable work_groups_elt : t sexp_opaque Bag.Elt.t option;
        (* [want_a_thread_elt] holds the element of the pool's
           [work_groups_that_want_a_thread] for this work group, if any.  The
           [sexp_opaque] is to prevent infinite unwinding when converting to a sexp. *)
        mutable want_a_thread_elt : t sexp_opaque Doubly_linked.Elt.t option;
      }
    with fields, sexp_of

    (* Each work group "reserves" a number of available threads that it requires be
       assignable to it, should it request them. *)
    let num_reserved_threads t =
      max 0 (t.min_assignable_threads - t.num_assigned_threads)
    ;;

    (* A work group wants a thread if it has work to do and isn't using all of the threads
       it is allowed to.  Whenever we change [t.work_queue] or [t.num_assigned_threads] we
       may need to update [t.want_a_thread_elt], which [is_some] iff [wants_a_thread
       t]. *)
    let wants_a_thread t =
      not (Queue.is_empty t.work_queue)
      && t.num_assigned_threads < t.max_assigned_threads
    ;;

    let invariant t : unit =
      try
        let check invariant field = invariant (Field.get field t) in
        Fields.iter
          ~in_pool:ignore
          ~is_in_use:(check (fun is_in_use ->
            assert (is_in_use = is_some t.work_groups_elt);
            if not is_in_use then begin
              assert (t.num_assigned_threads = 0);
              assert (t.unfinished_work = 0);
              assert (Queue.is_empty t.work_queue); (* because [unfinished_work = 0] *)
            end))
          ~min_assignable_threads:(check (fun min_assignable_threads ->
            assert (min_assignable_threads >= 0)))
          ~max_assigned_threads:(check (fun max_assigned_threads ->
            assert (max_assigned_threads >= t.min_assignable_threads)))
          ~num_assigned_threads:(check (fun num_assigned_threads ->
            assert (num_assigned_threads >= 0);
            assert (num_assigned_threads <= t.max_assigned_threads)))
          ~unfinished_work:(check (fun unfinished_work ->
            assert (unfinished_work >= Queue.length t.work_queue)))
          ~work_queue:(check (fun work_queue ->
            assert (Queue.is_empty work_queue
                    || t.num_assigned_threads >= t.min_assignable_threads)))
          ~work_groups_elt:(check (function
          | None -> assert false
          | Some elt -> assert (phys_equal t (Bag.Elt.value elt))))
          ~want_a_thread_elt:(check (fun o ->
            assert (is_some o = wants_a_thread t);
            match o with
            | None -> ()
            | Some e -> assert (phys_equal t (Doubly_linked.Elt.value e))));
        (* A work group can only be wanting if it is using its
           [min_assignable_threads]. *)
        if wants_a_thread t then
          assert (t.num_assigned_threads >= t.min_assignable_threads)
      with exn ->
        failwiths "invariant failed" (exn, t) <:sexp_of< exn * t >>
    ;;
  end

  module Work = struct
    include Work_

    type t = Work_group.t Work_.t_ with sexp_of
  end

  module Work_queue = struct
    type elt =
    | Stop
    | Work of Work.t
    with sexp_of

    type t = elt Squeue.t with sexp_of

    let create () = Squeue.create 1

    let enqueue t work = Squeue.push_uncond t work
  end

  module Thread = struct
    type t =
      { (* [name] is the name of the thread that the OS knows, i.e. the argument supplied
           to the most recent call to [set_thread_name] by the thread. *)
        mutable name : string;
        (* [thread_id] is the OCaml thread id of the OCaml thread that this corresponds
           to.  It is an option only because we create this object before creating the
           thread.  We set it to [Some] as soon as we create the thread, and then never
           change it. *)
        mutable thread_id : int option;
        (* [is_assigned] is true iff this thread is assigned to a work group.  If a thread
           is not assigned, it is "available". *)
        mutable is_assigned : bool;
        (* [priority] is the priority of the thread that the OS knows, i.e. the argument
           supplied in the most recent call to [setpriority] by the thread. *)
        mutable priority : Priority.t;
        mutable kind : [ `General | `Helper ];
        (* [unfinished_work] is the amount of work remaining for this thread to do.  It
           includes all the work in [work_queue], plus perhaps an additional work that
           is running. *)
        mutable unfinished_work : int;
        (* [work_queue] is where this thread pulls work from.  Each thread has its own
           queue. *)
        work_queue : Work_queue.t;
      }
    with fields, sexp_of

    let invariant t : unit =
      try
        let check invariant field = invariant (Field.get field t) in
        Fields.iter
          ~name:ignore
          ~thread_id:(check (fun o -> assert (is_some o)))
          ~is_assigned:ignore
          ~priority:ignore
          ~kind:ignore
          ~unfinished_work:(check (fun unfinished_work ->
            assert (unfinished_work = Squeue.length t.work_queue
                   || unfinished_work = Squeue.length t.work_queue + 1)))
          ~work_queue:ignore
      with exn ->
        failwiths "Thread.invariant failed" (exn, t) <:sexp_of< exn * t >>
    ;;

    let create priority =
      { name = "";
        thread_id = None;
        is_assigned = false;
        priority;
        kind = `General;
        unfinished_work = 0;
        work_queue = Work_queue.create ();
      }
    ;;

    let enqueue_work t work =
      t.unfinished_work <- t.unfinished_work + 1;
      Work_queue.enqueue t.work_queue (Work_queue.Work work);
    ;;

    let stop t = Work_queue.enqueue t.work_queue Work_queue.Stop

    let initialize_ocaml_thread t =
      set_thread_name t.name;
      (* We call [getpriority] to see whether we need to set the priority.  This is only
         used for initialization, not for ongoing work.  This is not a performance
         optimization.  It is done so that in programs that don't use priorities, we never
         call [setpriority], and thus prevent problems due to the user's "ulimit -e" being
         too restrictive. *)
      if not (Priority.equal (getpriority ()) t.priority) then
        setpriority t.priority;
    ;;

    let set_name t name =
      if String.(<>) name t.name then begin
        set_thread_name name;
        t.name <- name;
      end;
    ;;

    let set_priority t priority =
      if not (Priority.equal t.priority priority) then begin
        setpriority priority;
        t.priority <- priority;
      end;
    ;;

  end

  module Helper_thread = struct
    (* [Helper_thread.t] is a wrapper around [Thread.t] so the [threads] bag in the thread
       pool doesn't interfere with finalizers that clients attach to
       [Helper_thread.t]'s. *)
    type t =
      { in_pool : Pool_id.t;
        mutable is_in_use : bool;
        thread : Thread.t;
        work_group : Work_group.t;
        (* [default_name] will be used as the name of work run by the helper thread,
           unless that work is added with an overriding name. *)
        default_name : string;
        (* [default_priority] will be used as the priority of work run by the helper
           thread, unless that work is added with an overriding priority. *)
        default_priority : Priority.t;
      }
    with fields, sexp_of
  end

  (* [Thread_pool.t] *)
  type t =
    { id : Pool_id.t;
      mutable is_in_use : bool;
      (* [mutex] is used to protect all access to [t] and its substructures, since the
         threads actually doing the work need to access [t]. *)
      mutex : Mutex.t;
      default_priority : Priority.t;
      (* [max_num_threads] is the maximum number of threads that the thread pool is
         allowed to create.  The thread pool guarantees the thread requirements
         of work groups by maintaing the invariant that:

         |  num_assigned_threads + num_reserved_threads <= max_num_threads *)
      max_num_threads : int;
      (* [num_assigned_threads] is the number of threads that are currently assigned to
         some work_group.  If they are general-pool threads, then they are in the middle
         of doing a piece of work.  If they are helper threads, then they are simply
         assigned, and may not have any work to do. *)
      mutable num_assigned_threads : int;
      (* [num_reserved_threads] is the sum over all work groups [w] of
         [Work_group.num_reserved_threads w]. *)
      mutable num_reserved_threads : int;
      (* [num_threads] is the number of threads that have been created by the pool. *)
      mutable num_threads : int;
      (* [threads] holds all the threads that have been created by the pool. *)
      mutable threads : Thread.t list;
      (* [available_threads] holds all threads that have been created, but that are not
         currently assigned.  It is used as a stack so that the most recently used
         available thread is assigned first, on the theory that this is better for
         locality. *)
      mutable available_threads : Thread.t list;
      (* [unfinished_work] holds the amount of work that has been submitted to the pool
         but not yet been completed, over all work groups. *)
      mutable unfinished_work : int;
      (* [work_groups] holds all the work groups in use with the thread pool. *)
      work_groups : Work_group.t Bag.t;
      (* [work_groups_that_want_a_thread] has exactly one entry for every work group [w]
         such that [Work_group.wants_a_thread w].  It is used as a queue so that as
         threads as become available, the pool round robins among the work groups that
         want threads.  We use a doubly linked list so that we can remove a work group
         when it no longer wants a thread, which happens when all its work has been
         assigned to threads, or because it has received its [max_assigned_threads]. *)
      work_groups_that_want_a_thread : Work_group.t Doubly_linked.t;
    }
  with fields, sexp_of

  let invariant t : unit =
    try
      let module W = Work_group in
      let check invariant field = invariant (Field.get field t) in
      Fields.iter
        ~id:ignore
        ~is_in_use:(check (fun is_in_use ->
          if not is_in_use then begin
            assert (t.unfinished_work = 0);
            assert (t.num_assigned_threads = 0);
            assert (t.num_threads = 0)
          end))
        ~mutex:(check Mutex.invariant)
        ~default_priority:ignore
        ~max_num_threads:(check (fun max_num_threads ->
          assert (max_num_threads >= 1);
          assert (t.num_assigned_threads + t.num_reserved_threads <= max_num_threads)))
        ~num_assigned_threads:(check (fun num_assigned_threads ->
          assert (num_assigned_threads
                  = List.fold t.threads ~init:0 ~f:(fun n thread ->
                    if thread.Thread.is_assigned then n + 1 else n))))
        ~num_reserved_threads:(check (fun num_reserved_threads ->
          assert (num_reserved_threads =
              Bag.fold t.work_groups ~init:0 ~f:(fun n work_group ->
                n + W.num_reserved_threads work_group))))
        ~num_threads:(check (fun num_threads ->
          assert (num_threads = List.length t.threads);
          assert (num_threads <= t.max_num_threads)))
        ~threads:(check (fun threads -> List.iter threads ~f:Thread.invariant))
        ~available_threads:(check (fun available_threads ->
          assert (List.length available_threads = t.num_threads - t.num_assigned_threads);
          List.iter available_threads ~f:(fun thread ->
            assert (List.exists t.threads ~f:(fun thread' -> phys_equal thread thread'));
            assert (not thread.Thread.is_assigned))))
        ~unfinished_work:(check (fun unfinished_work ->
          assert (unfinished_work =
              Bag.fold t.work_groups ~init:0 ~f:(fun n work_group ->
                n + work_group.W.unfinished_work))))
        ~work_groups:(check (fun work_groups ->
          Bag.iter work_groups ~f:(fun work_group ->
            W.invariant work_group;
            assert (work_group.W.max_assigned_threads <= t.max_num_threads);
            if W.wants_a_thread work_group then
              assert
                (Doubly_linked.exists t.work_groups_that_want_a_thread
                   ~f:(fun work_group' ->
                     phys_equal work_group work_group')))))
        ~work_groups_that_want_a_thread:(check (fun dll ->
          Doubly_linked.invariant dll;
          Doubly_linked.iter dll ~f:(fun work_group ->
            match work_group.W.want_a_thread_elt with
            | None -> assert false
            | Some elt -> assert (phys_equal work_group (Doubly_linked.Elt.value elt)));
          (* If work groups want threads, then the only reason they can't have them is
             if the thread pool doesn't have any avalable. *)
          if not (Doubly_linked.is_empty dll) then
            assert (t.num_assigned_threads + t.num_reserved_threads = t.max_num_threads)))
    with exn ->
      failwiths "Thread_pool.invariant failed" (exn, t) <:sexp_of< exn * t >>
  ;;

  let create ~max_num_threads =
    if max_num_threads < 1 then
      error "Thread_pool.create max_num_threads was < 1" max_num_threads
        (<:sexp_of< int >>)
    else
      let t =
        { id = Pool_id.create ();
          is_in_use = true;
          mutex = Mutex.create ();
          default_priority = getpriority ();
          max_num_threads;
          num_assigned_threads = 0;
          num_reserved_threads = 0;
          num_threads = 0;
          threads = [];
          available_threads = [];
          unfinished_work = 0;
          work_groups = Bag.create ();
          work_groups_that_want_a_thread = Doubly_linked.create ();
        }
      in
      Ok t
  ;;

  let finished_with t =
    if debug then Debug.log "Thread_pool.finished_with" t <:sexp_of< t >>;
    if not t.is_in_use then
      Ok ()
    else if t.unfinished_work > 0 then
      error "\
Thread_pool.finished_with called on thread pool that has unfinished work"
        t <:sexp_of< t >>
    else if t.num_assigned_threads > 0 then
      error "\
Thread_pool.finished_with called on thread pool that has unfinished helper threads"
        t <:sexp_of< t >>
    else begin
      t.is_in_use <- false;
      List.iter t.threads ~f:Thread.stop;
      t.num_threads <- 0;
      t.threads <- [];
      t.available_threads <- [];
      Ok ();
    end;
  ;;

  let create_work_group
      ?(min_assignable_threads = 0)
      ?max_assigned_threads
      t =
    let max_assigned_threads =
      Option.value max_assigned_threads ~default:t.max_num_threads
    in
    if not t.is_in_use then
      error "create_work_group called on finished thread pool" t <:sexp_of< t >>
    else if min_assignable_threads < 0 then
      error "create_work_group got min_assignable_threads < 0"
        min_assignable_threads <:sexp_of< int >>
    else if min_assignable_threads > max_assigned_threads then
      error "create_work_group got min_assignable_threads > max_assigned_threads"
        (`Min min_assignable_threads, `Max max_assigned_threads)
        (<:sexp_of< [ `Min of int ] * [ `Max of int ] >>)
    else if max_assigned_threads > t.max_num_threads then
      error "create_work_group got max_assigned_threads > max_num_threads"
        (`Max_assigned_threads max_assigned_threads, `Max_num_threads t.max_num_threads)
        (<:sexp_of< [ `Max_assigned_threads of int ] * [ `Max_num_threads of int ] >>)
    else begin
      let new_reserved = t.num_reserved_threads + min_assignable_threads in
      if t.num_assigned_threads + new_reserved > t.max_num_threads then
        error "create_work_group with not enough threads to satisfy min_assignable"
          (min_assignable_threads, t) <:sexp_of< int * t >>
      else begin
        t.num_reserved_threads <- new_reserved;
        let work_group =
          { Work_group.
            in_pool = t.id;
            is_in_use = true;
            min_assignable_threads;
            max_assigned_threads;
            num_assigned_threads = 0;
            unfinished_work = 0;
            work_queue = Queue.create ();
            work_groups_elt = None;
            want_a_thread_elt = None;
          }
        in
        work_group.Work_group.work_groups_elt <- Some (Bag.add t.work_groups work_group);
        Ok work_group
      end
    end
  ;;

  let finished_with_work_group t work_group =
    if debug then
      Debug.log "finished_with_work_group" (work_group, t) <:sexp_of< Work_group.t * t >>;
    let module W = Work_group in
    if not (Pool_id.equal t.id work_group.W.in_pool) then
      error "finished_with_work_group called on work group not in pool" (work_group, t)
        (<:sexp_of< Work_group.t * t >>)
    else if not t.is_in_use then
      error "finished_with_work_group called on finished thread pool" t <:sexp_of< t >>
    else if not work_group.W.is_in_use then
      Ok ()
    else if work_group.W.num_assigned_threads > 0 then
      error "\
finish_with_work_group called on work group with assigned threads"
        work_group <:sexp_of< W.t >>
    else if work_group.W.unfinished_work > 0 then
      error "\
finish_with_work_group called on work group with unfinished work"
        work_group <:sexp_of< W.t >>
    else begin
      work_group.W.is_in_use <- false;
      begin match work_group.W.work_groups_elt with
      | None -> assert false
      | Some elt ->
        Bag.remove t.work_groups elt;
        work_group.W.work_groups_elt <- None;
      end;
      t.num_reserved_threads <-
        t.num_reserved_threads - W.num_reserved_threads work_group;
      Ok ();
    end
  ;;

  let update_want_a_thread t work_group =
    let module W = Work_group in
    match W.wants_a_thread work_group, work_group.W.want_a_thread_elt with
    | true, Some _
    | false, None
      -> ()
    | true, None ->
      work_group.W.want_a_thread_elt
        <- Some (Doubly_linked.insert_last t.work_groups_that_want_a_thread work_group);
    | false, Some elt ->
      Doubly_linked.remove t.work_groups_that_want_a_thread elt;
      work_group.W.want_a_thread_elt <- None;
  ;;

  let delta_assigned_threads t work_group i =
    if debug then
      Debug.log "delta_assigned_threads" (i, work_group, t)
        (<:sexp_of< int * Work_group.t * t >>);
    let module W = Work_group in
    t.num_assigned_threads <- t.num_assigned_threads + i;
    let num_previously_reserved = W.num_reserved_threads work_group in
    work_group.W.num_assigned_threads <- work_group.W.num_assigned_threads + i;
    let num_now_reserved = W.num_reserved_threads work_group in
    let delta_num_reserved = num_now_reserved - num_previously_reserved in
    t.num_reserved_threads <- t.num_reserved_threads + delta_num_reserved;
  ;;

  let assign_thread t thread work_group =
    if debug then
      Debug.log "assign_thread" (thread, work_group, t)
        (<:sexp_of< Thread.t * Work_group.t * t >>);
    thread.Thread.is_assigned <- true;
    delta_assigned_threads t work_group 1;
  ;;

  let unassign_thread t thread work_group =
    if debug then
      Debug.log "unassign_thread" (thread, work_group, t)
        (<:sexp_of< Thread.t * Work_group.t * t >>);
    thread.Thread.is_assigned <- false;
    delta_assigned_threads t work_group (-1);
  ;;

  let idle_thread t thread =
    if debug then Debug.log "idle_thread" (thread, t) <:sexp_of< Thread.t * t >>;
    t.available_threads <- thread :: t.available_threads
  ;;

  let maybe_reassign_thread t thread work_group =
    if debug then
      Debug.log "maybe_reassign_thread" (thread, work_group, t)
        (<:sexp_of< Thread.t * Work_group.t * t >>);
    let module W = Work_group in
    (* We choose the next work group that we will assign [thread] to.  We continue to use
       [thread] for [work_group] if no other work group wants a thread or we must keep
       [thread] reserved for [work_group] to meet its [min_assignable_threads]. *)
    if Doubly_linked.is_empty t.work_groups_that_want_a_thread
      || work_group.W.num_assigned_threads <= work_group.W.min_assignable_threads
    then begin
      match Queue.dequeue work_group.W.work_queue with
      | None ->
        unassign_thread t thread work_group;
        (* [work_group] did not want a thread, because it has no work.  After unassigning
           [thread], [work_group] still doesn't want a thread, because it still has no
           work.  So, no need to call [update_want_a_thread]. *)
        idle_thread t thread;
      | Some work ->
        (* We might have emptied [work_queue], in which case [work_group] might have
           transitioned from wanting a thread to not wanting a thread. *)
        update_want_a_thread t work_group;
        Thread.enqueue_work thread work;
    end else begin
      unassign_thread t thread work_group;
      (* If [work_group] had its [max_assigned_threads], then it now doesn't, and may
         have transitioned from not wanting a thread to wanting a thread. *)
      update_want_a_thread t work_group;
      assert (t.num_assigned_threads + t.num_reserved_threads < t.max_num_threads);
      match Doubly_linked.remove_first t.work_groups_that_want_a_thread with
      | None -> idle_thread t thread
      | Some work_group ->
        work_group.W.want_a_thread_elt <- None;
        let work =
          (* [work_queue] must be nonempty, since [work_group] wants a thread *)
          Option.value_exn (Queue.dequeue work_group.W.work_queue)
            ~error:(Error.create "work_queue unexpectedly empty" (work_group, t)
                      <:sexp_of< Work_group.t * t >>)
        in
        Thread.enqueue_work thread work;
        assign_thread t thread work_group;
        (* Even though [work_group] has been assigned [thread], it might want yet another
           thread. *)
        update_want_a_thread t work_group;
    end;
  ;;

  let create_thread t =
    if debug then Debug.log "create_thread" t <:sexp_of< t >>;
    let thread = Thread.create t.default_priority in
    let ocaml_thread =
      Core.Std.Thread.create (fun () ->
        Thread.initialize_ocaml_thread thread;
        let rec loop () =
          match Squeue.pop thread.Thread.work_queue with
          | Work_queue.Stop -> ()
          | Work_queue.Work work ->
            if debug then
              Debug.log "thread got work" (work, thread, t)
                (<:sexp_of< Work.t * Thread.t * t >>);
            Thread.set_name thread work.Work.name;
            Thread.set_priority thread work.Work.priority;
            (try
               work.Work.doit () (* the actual work *)
             with _ -> ());
            if debug then Debug.log "thread finished with work" (work, thread, t)
              (<:sexp_of< Work.t * Thread.t * t >>);
            Mutex.critical_section t.mutex ~f:(fun () ->
              let module W = Work_group in
              let work_group = work.Work.work_group in
              t.unfinished_work <- t.unfinished_work - 1;
              work_group.W.unfinished_work <- work_group.W.unfinished_work - 1;
              thread.Thread.unfinished_work <- thread.Thread.unfinished_work - 1;
              begin match thread.Thread.kind with
              | `Helper -> ()
              | `General -> maybe_reassign_thread t thread work_group;
              end);
            loop ()
        in
        loop ()) ()
    in
    thread.Thread.thread_id <- Some (Core.Std.Thread.id ocaml_thread);
    t.num_threads <- t.num_threads + 1;
    t.threads <- thread :: t.threads;
    thread
  ;;

  let assign_available_thread_to t work_group =
    if debug then
      Debug.log "assign_available_thread_to" (work_group, t)
        (<:sexp_of< Work_group.t * t >>);
    let module W = Work_group in
    if work_group.W.num_assigned_threads = work_group.W.max_assigned_threads
      || (work_group.W.num_assigned_threads >= work_group.W.min_assignable_threads
          && t.num_assigned_threads + t.num_reserved_threads = t.max_num_threads)
    then `Not_allowed
    else begin
      let thread =
        match t.available_threads with
        | thread :: rest -> t.available_threads <- rest; thread
        | [] -> create_thread t
      in
      (* If we had a thread available to assign to [work_group], then [work_group] must
         not have had any work to do.  Hence, it must be that [not (wants_a_thread
         work_group)].  Thus, assigning a thread to [work_group] won't change
         [wants_a_thread work_group], and there is no need to call [update_want_a_thread
         work_group]. *)
      assert (Queue.is_empty work_group.W.work_queue);
      assign_thread t thread work_group;
      `Ok thread
    end
  ;;

  let inc_unfinished_work t work_group =
    t.unfinished_work <- t.unfinished_work + 1;
    work_group.Work_group.unfinished_work <- work_group.Work_group.unfinished_work + 1;
  ;;

  let default_thread_name = "thread-pool thread"

  let add_work_for_group ?priority ?name t work_group doit =
    if debug then
      Debug.log "add_work_for_group" (work_group, t) <:sexp_of< Work_group.t * t >>;
    let module W = Work_group in
    if not (Pool_id.equal t.id work_group.W.in_pool) then
      error "add_work_for_group called on group not in pool" (work_group, t)
        (<:sexp_of< Work_group.t * t >>)
    else if not t.is_in_use then
      error "add_work_for_group called on finished thread pool" t <:sexp_of< t >>
    else if not work_group.W.is_in_use then
      error "add_work_for_group called on finished work group" (work_group, t)
        (<:sexp_of< Work_group.t * t >>)
    else begin
      let work =
        { Work.
          doit;
          work_group;
          name = Option.value name ~default:default_thread_name;
          priority = Option.value priority ~default:t.default_priority;
        }
      in
      inc_unfinished_work t work_group;
      let work_queue = work_group.W.work_queue in
      if not (Queue.is_empty work_queue) then
        Queue.enqueue work_queue work
      else begin
        match assign_available_thread_to t work_group with
        | `Ok thread -> Thread.enqueue_work thread work
        | `Not_allowed ->
          Queue.enqueue work_queue work;
          (* [work_queue] was empty prior to us adding [work].  So, [work_group] may have
             transitioned from not wanting a thread to wanting a thread. *)
          update_want_a_thread t work_group;
      end;
      Ok ()
    end
  ;;

  let create_helper_thread ?priority ?name t work_group =
    if debug then
      Debug.log "create_helper_thread" (work_group, t) <:sexp_of< Work_group.t * t >>;
    if not (Pool_id.equal t.id work_group.Work_group.in_pool) then
      error "create_helper_thread called on work group not in pool" (work_group, t)
        (<:sexp_of< Work_group.t * t >>)
    else if not t.is_in_use then
      error "create_helper_thread called on finished thread pool" t <:sexp_of< t >>
    else
      match assign_available_thread_to t work_group with
      | `Not_allowed ->
        error "create_helper_thread could not get a thread" (work_group, t)
          (<:sexp_of< Work_group.t * t >>);
      | `Ok thread ->
        thread.Thread.kind <- `Helper;
        Ok { Helper_thread.
             default_name = Option.value name ~default:"helper_thread";
             default_priority = Option.value priority ~default:t.default_priority;
             in_pool = t.id;
             is_in_use = true;
             thread;
             work_group;
           }
  ;;

  let add_work_for_helper_thread ?priority ?name t helper_thread doit =
    if debug then
      Debug.log "add_work_for_helper_thread" (helper_thread, t)
        (<:sexp_of< Helper_thread.t * t >>);
    let module W = Work_group in
    let module H = Helper_thread in
    if not (Pool_id.equal t.id helper_thread.H.in_pool) then
      error "add_work_for_helper_thread called on helper thread not in pool"
        (helper_thread, t) (<:sexp_of< Helper_thread.t * t >>)
    else if not t.is_in_use then
      error "add_work_for_helper_thread called on finished thread pool" t <:sexp_of< t >>
    else if not helper_thread.H.is_in_use then
      error "add_work_for_helper_thread called on finished helper thread"
        (helper_thread, t) <:sexp_of< Helper_thread.t * t >>
    else begin
      let { Helper_thread. thread; work_group; _ } = helper_thread in
      inc_unfinished_work t work_group;
      Thread.enqueue_work thread
        { Work.
          name = Option.value name ~default:(Helper_thread.default_name helper_thread);
          work_group;
          doit;
          priority = Option.value priority ~default:(Helper_thread.default_priority helper_thread);
        };
      Ok ();
    end
  ;;

  let finished_with_helper_thread t helper_thread =
    if debug then
      Debug.log "finished_with_helper_thread" (helper_thread, t)
        (<:sexp_of< Helper_thread.t * t >>);
    let module H = Helper_thread in
    if not (Pool_id.equal t.id helper_thread.H.in_pool) then
      error "finished_with_helper_thread called on helper thread not in pool"
        (helper_thread, t) (<:sexp_of< Helper_thread.t * t >>)
    else if not t.is_in_use then
      error "finished_with_helper_thread called on finished thread pool" t <:sexp_of< t >>
    else if not helper_thread.H.is_in_use then
      Ok ()
    else begin
      let thread = helper_thread.H.thread in
      if thread.Thread.unfinished_work > 0 then
        error "\
finished_with_helper_thread called on helper thread with unfinished work"
          (helper_thread, t) <:sexp_of< Helper_thread.t * t >>
      else begin
        thread.Thread.kind <- `General;
        maybe_reassign_thread t thread helper_thread.H.work_group;
        Ok ();
      end
    end
  ;;
end

(* Now we define everything to be exported, being careful to wrap everything in
   [Mutex.critical_section] that needs to be. *)
open Internal

type t = Internal.t with sexp_of

let critical_section t ~f =
  Mutex.critical_section t.mutex ~f:(fun () ->
    protect ~f ~finally:(fun () -> if !check_invariant then invariant t));
;;

let invariant t = critical_section t ~f:(fun () -> invariant t)

let create ~max_num_threads =
  Result.map (create ~max_num_threads) ~f:(fun t ->
    if !check_invariant then invariant t;
    t)
;;

let finished_with t = critical_section t ~f:(fun () -> finished_with t)

let max_num_threads = max_num_threads

let num_threads = num_threads

let default_priority = default_priority

module Work_group = struct
  module Work_group = Internal.Work_group

  (* We use an extra wrapper around work groups so that client code can attach finalizers
     to them, and we can hold on to the internal [Work_group.t] without preventing those
     finalizers from firing. *)
  type t = { work_group : Work_group.t }
  with fields, sexp_of
end

module Helper_thread = struct
  open Helper_thread

  type t = Helper_thread.t with sexp_of

  let default_name = default_name
  let default_priority = default_priority
end

let create_work_group ?min_assignable_threads ?max_assigned_threads t =
  critical_section t ~f:(fun () ->
    match create_work_group ?min_assignable_threads ?max_assigned_threads t with
    | Error _ as x -> x
    | Ok work_group -> Ok { Work_group. work_group })
;;

let add_work_for_group ?priority ?name t work_group doit =
  critical_section t ~f:(fun () ->
    add_work_for_group ?priority ?name t (Work_group.work_group work_group) doit);
;;

let finished_with_work_group t work_group =
  critical_section t ~f:(fun () ->
    finished_with_work_group t (Work_group.work_group work_group));
;;

let create_helper_thread ?priority ?name t work_group =
  critical_section t ~f:(fun () ->
    create_helper_thread ?priority ?name t (Work_group.work_group work_group));
;;

let add_work_for_helper_thread ?priority ?name t helper_thread doit =
  critical_section t ~f:(fun () ->
    add_work_for_helper_thread ?priority ?name t helper_thread doit);
;;

let finished_with_helper_thread t helper_thread =
  critical_section t ~f:(fun () ->
    finished_with_helper_thread t helper_thread);
;;

TEST_MODULE = struct

  let () = check_invariant := true

  let wait_until_no_unfinished_work t =
    let rec loop i =
      if t.unfinished_work > 0 then begin
        Time.pause (sec 0.01);
        loop (i + 1);
      end;
    in
    loop 0
  ;;

  (* A simple thread-safe [Ivar] implementation.*)
  module Ivar : sig
    type 'a t with sexp_of

    val create : unit -> _ t
    val fill : 'a t -> 'a -> unit
    val read : 'a t -> 'a (* blocks until value is available *)
  end = struct
    module Mutex = Core.Std.Mutex

    type 'a t =
      { mutable value : 'a option;
        mutable num_waiting : int;
        mutex : Mutex.t sexp_opaque;
        (* Threads that do [Ivar.read] when [is_none value] block using [Condition.wait
           full].  When [Ivar.fill] sets [value], it uses [Condition.broadcast] to wake
           up all the blocked threads. *)
        full : Condition.t sexp_opaque;
      }
    with sexp_of

    let create () =
      { value = None;
        num_waiting = 0;
        mutex = Mutex.create ();
        full = Condition.create ();
      }
    ;;

    let critical_section t ~f = Mutex.critical_section t.mutex ~f

    let fill t v =
      critical_section t ~f:(fun () ->
        if is_some t.value
        then failwith "Ivar.fill of full ivar"
        else begin
          t.value <- Some v;
          Condition.broadcast t.full;
        end);
    ;;

    let read t =
      match t.value with
      | Some v -> v
      | None ->
        critical_section t ~f:(fun () ->
          match t.value with
          | Some v -> v
          | None ->
            t.num_waiting <- t.num_waiting + 1;
            Condition.wait t.full t.mutex;
            t.num_waiting <- t.num_waiting - 1;
            match t.value with
            | Some v -> v
            | None -> assert false)
    ;;
  end

  (* [create] and [finished_with]. *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    assert (max_num_threads t = 1);
    assert (num_threads t = 0); (* no threads should have been created *)
    ok_exn (finished_with t);
  ;;

  (* Error cases for [create]. *)
  TEST =
    List.for_all [ -1; 0 ] ~f:(fun max_num_threads ->
      Result.is_error (create ~max_num_threads))
  ;;

  (* Error cases for [finished_with] and [finished_with_work_group]. *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    let work_group = ok_exn (create_work_group t) in
    let worker_should_continue = Ivar.create () in
    ok_exn (add_work_for_group t work_group (fun () -> Ivar.read worker_should_continue));
    assert (Result.is_error (finished_with t));
    assert (Result.is_error (finished_with_work_group t work_group));
    Ivar.fill worker_should_continue ();
    wait_until_no_unfinished_work t;
    ok_exn (finished_with_work_group t work_group);
    ok_exn (finished_with t);
  ;;

  (* Error cases for [create_work_group]. *)
  TEST_UNIT =
    let max_num_threads = 1 in
    let t = ok_exn (create ~max_num_threads) in
    assert (Result.is_error (create_work_group t ~min_assignable_threads:(-1)));
    assert (Result.is_error
              (create_work_group t ~min_assignable_threads:1 ~max_assigned_threads:0));
    assert (Result.is_error
              (create_work_group t
                 ~min_assignable_threads:(max_num_threads + 1)
                 ~max_assigned_threads:(max_num_threads + 1)));
    assert (Result.is_ok (create_work_group t));
    assert (Result.is_ok (finished_with t));
    assert (Result.is_error (create_work_group t));
    ok_exn (finished_with t);
  ;;

  (* Error cases for [add_work_for_group]. *)
  TEST_UNIT =
    let max_num_threads = 1 in
    let t = ok_exn (create ~max_num_threads) in
    let work_group = ok_exn (create_work_group t) in
    ok_exn (finished_with_work_group t work_group);
    assert (Result.is_error (add_work_for_group t work_group ignore));
    let work_group = ok_exn (create_work_group t) in
    ok_exn (finished_with t);
    assert (Result.is_error (add_work_for_group t work_group ignore));
  ;;

  (* Check that the expected concurrency is used. *)
  TEST_UNIT =
    List.iter [ 1; 2; 5; 10; 100; 1000 ] ~f:(fun num_jobs ->
      List.iter [ 1; 2; 5; 10; 100 ] ~f:(fun max_num_threads ->
        List.iter [ 1; 2; 5; 10; 100 ] ~f:(fun max_assigned_threads ->
          if debug then
            eprintf "num_jobs = %d  max_num_threads = %d  max_assigned_threads = %d\n%!"
              num_jobs max_num_threads max_assigned_threads;
          if max_assigned_threads <= max_num_threads then
            let expected_max_concurrent_jobs = min num_jobs max_assigned_threads in
            let max_observed_concurrent_jobs = ref 0 in
            let num_concurrent_jobs = ref 0 in
            let job_starts = ref [] in
            let t = ok_exn (create ~max_num_threads) in
            let work_group = ok_exn (create_work_group t ~max_assigned_threads) in
            let worker_threads_have_fully_started = Ivar.create () in
            let worker_threads_should_continue = Ivar.create () in
            let _ : Core.Std.Thread.t =
              Core.Std.Thread.create (fun () ->
                let start = Time.now () in
                let rec loop () =
                  if t.is_in_use then begin
                    let how_long = Time.diff (Time.now ()) start in
                    if Time.Span.(>=) how_long (sec 10.) then begin
                      Debug.log "thread-pool unit test hung"
                        (t,
                         worker_threads_have_fully_started,
                         worker_threads_should_continue)
                        (<:sexp_of< t * unit Ivar.t * unit Ivar.t >>);
                      exit 1;
                    end else begin
                      Time.pause (sec 0.1);
                      loop ();
                    end
                  end;
                in
                loop ()
              ) ()
            in
            let jobs = ref [] in
            for i = 0 to num_jobs - 1; do
              let job =
                ok_exn (add_work_for_group t work_group (fun () ->
                  job_starts := i :: !job_starts;
                  if List.length !job_starts = expected_max_concurrent_jobs then
                    Ivar.fill worker_threads_have_fully_started ();
                  incr num_concurrent_jobs;
                  max_observed_concurrent_jobs :=
                    max !max_observed_concurrent_jobs !num_concurrent_jobs;
                  assert (!num_concurrent_jobs <= max_num_threads);
                  Ivar.read worker_threads_should_continue;
                  decr num_concurrent_jobs))
              in
              jobs := job :: !jobs
            done;
            Ivar.read worker_threads_have_fully_started;
            assert (!num_concurrent_jobs = expected_max_concurrent_jobs);
            assert (List.length !job_starts = expected_max_concurrent_jobs);
            if max_num_threads = 1 then
              assert (!job_starts = List.init expected_max_concurrent_jobs ~f:Fn.id);
            Ivar.fill worker_threads_should_continue ();
            wait_until_no_unfinished_work t;
            assert (!max_observed_concurrent_jobs = expected_max_concurrent_jobs);
            if max_num_threads = 1 then
              assert (List.rev !job_starts = List.init num_jobs ~f:Fn.id);
            assert (t.num_threads <= max_num_threads);
            ok_exn (finished_with t);
        )))
  ;;

  (* Multiple work groups. *)
  TEST_UNIT =
    let max_num_threads = 10 in
    let t = ok_exn (create ~max_num_threads) in
    (* Create as many as you want with [~min_assignable_threads:0] *)
    for _i = 1 to 100; do
      ignore (ok_exn (create_work_group t ~min_assignable_threads:0) : Work_group.t);
    done;
    ok_exn (finished_with t);
  ;;

  TEST_UNIT =
    (* Ensure [min_assignable_threads] works. *)
    let t = ok_exn (create ~max_num_threads:2) in
    let w1 = ok_exn (create_work_group t) in
    let w2 = ok_exn (create_work_group t ~min_assignable_threads:1) in
    let num_w1_jobs = 2 in
    let num_w1_jobs_started = ref 0 in
    let w1_jobs_continue = Ivar.create () in
    for _i = 1 to num_w1_jobs; do
      ok_exn (add_work_for_group t w1 (fun () ->
        incr num_w1_jobs_started;
        Ivar.read w1_jobs_continue))
    done;
    let w2_jobs_finished = Ivar.create () in
    let num_w2_jobs = 2 in
    let num_w2_jobs_remaining = ref num_w2_jobs in
    for _i = 1 to num_w2_jobs; do
      ok_exn (add_work_for_group t w2 (fun () ->
        decr num_w2_jobs_remaining;
        if !num_w2_jobs_remaining = 0 then Ivar.fill w2_jobs_finished ()))
    done;
    Ivar.read w2_jobs_finished;
    assert (!num_w1_jobs_started <= 1);
    Ivar.fill w1_jobs_continue ();
    wait_until_no_unfinished_work t;
    ok_exn (finished_with t);
  ;;

  TEST_UNIT =
    (* Ensure we're round robining among work groups that want a thread. *)
    List.iter [ 1; 2; 10; 100 ] ~f:(fun num_work_groups ->
      List.iter [ 1; 2; 10; 100 ] ~f:(fun num_jobs_per_group ->
        let t = ok_exn (create ~max_num_threads:1) in
        let finish_order = ref [] in
        let start = Ivar.create () in
        ok_exn (add_work_for_group t
                  (ok_exn (create_work_group t))
                  (fun () -> Ivar.read start));
        for work_group_num = 1 to num_work_groups do
          let work_group =
            ok_exn (create_work_group t ~min_assignable_threads:0 ~max_assigned_threads:1)
          in
          for _i = 1 to num_jobs_per_group do
            ok_exn (add_work_for_group t work_group (fun () ->
              finish_order := work_group_num :: !finish_order));
          done
        done;
        Ivar.fill start ();
        wait_until_no_unfinished_work t;
        let finish_order = List.rev !finish_order in
        assert (finish_order =
            List.concat
              (List.init num_jobs_per_group ~f:(fun _ ->
                List.init num_work_groups ~f:(fun i -> i + 1))));
        ok_exn (finished_with t)))
  ;;

  (* Ensure that if a work group has multiple jobs that it would like to run
     simultaneously in parallel, and threads only become available one-at-a-time, that the
     work group gets to ramp up to its full parallelism. *)
  TEST_UNIT =
    List.iter [ 2; 10; 100 ] ~f:(fun max_parallelism ->
      let t = ok_exn (create ~max_num_threads:max_parallelism) in
      let w1 = ok_exn (create_work_group t) in
      let w1_continue = Ivar.create () in
      let all_w1s_started = Ivar.create () in
      let num_w1s_running = ref 0 in
      for _i = 1 to max_parallelism do
        ok_exn (add_work_for_group t w1 (fun () ->
          incr num_w1s_running;
          if !num_w1s_running = max_parallelism then Ivar.fill all_w1s_started ();
          Ivar.read w1_continue))
      done;
      Ivar.read all_w1s_started;
      (* At this point, [w1] is using all threads. *)
      let w2 = ok_exn (create_work_group t) in
      let w2_continue = Ivar.create () in
      let num_w2s_running = ref 0 in
      for _i = 1 to max_parallelism do
        ok_exn (add_work_for_group t w2 (fun () ->
          incr num_w2s_running;
          if !num_w2s_running = max_parallelism then Ivar.fill w2_continue ();
          Ivar.read w2_continue))
      done;
      (* Now, we will free up all the threads [w1] is using, so that [w2] can use them. *)
      Ivar.fill w1_continue ();
      wait_until_no_unfinished_work t;
      ok_exn (finished_with t))
  ;;

  (* Helper threads. *)

  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    let work_group = ok_exn (create_work_group t) in
    let helper_thread = ok_exn (create_helper_thread t work_group) in
    let helper_continue = Ivar.create () in
    let helper_finished = Ivar.create () in
    let work_finished = Ivar.create () in
    ok_exn (add_work_for_helper_thread t helper_thread
              (fun () ->
                Ivar.read helper_continue;
                Ivar.fill helper_finished ();
              ));
    ok_exn (add_work_for_group t work_group (fun () -> Ivar.fill work_finished ()));
    Ivar.fill helper_continue ();
    Ivar.read helper_finished;
    ok_exn (finished_with_helper_thread t helper_thread);
    Ivar.read work_finished;
    wait_until_no_unfinished_work t;
    ok_exn (finished_with t);
  ;;

  (* Can't finish a thread pool with an unfinished helper thread. *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    let work_group = ok_exn (create_work_group t) in
    let helper_thread = ok_exn (create_helper_thread t work_group) in
    assert (Result.is_error (finished_with t));
    ok_exn (finished_with_helper_thread t helper_thread);
    ok_exn (finished_with t);
  ;;

  (* Error cases for mismatches between pool, work group, and helper thread. *)
  TEST_UNIT =
    let t = ok_exn (create ~max_num_threads:1) in
    let t_bad = ok_exn (create ~max_num_threads:1) in
    let work_group = ok_exn (create_work_group t) in
    let helper_thread = ok_exn (create_helper_thread t work_group) in
    assert (Result.is_error (create_helper_thread        t_bad work_group          ));
    assert (Result.is_error (add_work_for_group          t_bad work_group ignore   ));
    assert (Result.is_error (finished_with_work_group    t_bad work_group          ));
    assert (Result.is_error (add_work_for_helper_thread  t_bad helper_thread ignore));
    assert (Result.is_error (finished_with_helper_thread t_bad helper_thread       ));
    ok_exn (finished_with_helper_thread t helper_thread);
    ok_exn (finished_with t);
  ;;

  (* Setting thread name and priority. *)
  TEST_UNIT =
    Result.iter Core.Std.Unix.RLimit.nice ~f:(fun rlimit_nice ->
      let test_parameters =
        let open Core.Std.Unix.RLimit in
        let nice_limit = get rlimit_nice in
        match nice_limit.max with
        | Infinity ->
          let max = 40 in
          `Test ({ nice_limit with cur = Limit (Int64.of_int_exn max) }, max)
        | Limit max ->
          if Int64.( < ) max (Int64.of_int 2)
          then `Cannot_test
          else `Test ({ nice_limit with cur = Limit max }, (Int64.to_int_exn max))
      in
      match test_parameters with
      | `Cannot_test -> ()
      | `Test (nice_limit, cur_limit) ->
        Core.Std.Unix.RLimit.set rlimit_nice nice_limit;
        for priority = 20 - cur_limit to 20 do
          let initial_priority = Priority.of_int priority in
          match Linux_ext.getpriority, Linux_ext.pr_get_name with
          | Error _, _ | _, Error _ -> ()
          | Ok getpriority, Ok get_name ->
            let t = ok_exn (create ~max_num_threads:2) in
            let work_group = ok_exn (create_work_group t) in
            let check4 ~name ~priority
                (check : ?name:string -> ?priority:Priority.t -> unit -> unit Or_error.t) =
              ok_exn (check ());
              ok_exn (check ~name ());
              ok_exn (check ~priority ());
              ok_exn (check ~name ~priority ());
              wait_until_no_unfinished_work t;
            in
            check4 ~name:"new name" ~priority:(Priority.decr initial_priority)
              (fun ?name ?priority () ->
                add_work_for_group ?priority ?name t work_group (fun () ->
                  assert (get_name () = Option.value name ~default:default_thread_name);
                  assert (getpriority ()
                          = Option.value priority ~default:(default_priority t))));
            check4 ~name:"new name" ~priority:(Priority.decr initial_priority)
              (fun ?name ?priority () ->
                let helper_thread =
                  ok_exn (create_helper_thread t work_group ?priority ?name)
                in
                let default_thread_name = Option.value name ~default:default_thread_name in
                let default_priority =
                  Option.value priority ~default:(default_priority t)
                in
                check4 ~name:"new name 2" ~priority:(Priority.decr initial_priority)
                  (fun ?name ?priority () ->
                    add_work_for_helper_thread ?priority ?name t helper_thread (fun () ->
                      assert (get_name () = Option.value name ~default:default_thread_name);
                      assert (getpriority ()
                              = Option.value priority ~default:default_priority)));
                finished_with_helper_thread t helper_thread);
            ok_exn (finished_with t);
        done)
  ;;
end
