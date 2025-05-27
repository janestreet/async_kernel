open! Core

type 'a t = 'a list Atomic.t

[%%rederive
  type ('a : value mod contended portable) t = 'a list Atomic.t
  [@@deriving sexp_of ~portable]]

(* We make the atomic for the external job queue alone on a cache line to avoid false
   sharing; the tradeoff here is we use moderately more memory but this is fine since
   there will only ever be one of these per process. *)
let[@inline] create_alone () = Atomic.make_alone []

let is_empty (type a : value mod contended portable) (t : a t) =
  List.is_empty (Atomic.get t)
;;

let[@inline] enqueue t x = Atomic.update t ~pure_f:(fun xs -> x :: xs)

let push_front_many (t : ('a : value mod contended portable) t) xs =
  let xs = List.rev xs in
  Atomic.update t ~pure_f:(fun curr -> curr @ xs)
;;

let[@inline] dequeue_all t =
  (* To optimize for the empty case, we first do a [get] to check if the queue is empty,
     and only do the [exchange] with an empty list if we know the list to be non-empty. *)
  match Atomic.get t with
  | [] -> []
  | _ -> Atomic.exchange t []
;;

let[@inline] dequeue_until_empty
  (type a : value mod contended portable)
  (t : a t)
  ~(f @ local)
  =
  let xs = dequeue_all t in
  let[@inline] rec loop = function
    | [] -> ()
    | x :: xs ->
      (try f x with
       | exn ->
         (* If a job raises, re-enqueue all external jobs that we hadn't gotten to. *)
         push_front_many t xs;
         raise exn);
      loop xs
  in
  loop (List.rev xs) [@nontail]
;;

let%expect_test "queue ordering" =
  let print_contents t = t |> Atomic.get |> [%sexp_of: int list] |> print_s in
  let t = create_alone () in
  print_contents t;
  [%expect {| () |}];
  enqueue t 1;
  print_contents t;
  [%expect {| (1) |}];
  enqueue t 2;
  print_contents t;
  [%expect {| (2 1) |}];
  print_endline "Dequeued items:";
  dequeue_until_empty t ~f:(fun x -> print_s [%sexp (x : int)]);
  print_endline "Queue contents:";
  print_contents t;
  [%expect
    {|
    Dequeued items:
    1
    2
    Queue contents:
    ()
    |}];
  enqueue t 1;
  enqueue t 2;
  enqueue t 3;
  print_contents t;
  [%expect {| (3 2 1) |}];
  push_front_many t [ 6; 5; 4 ];
  print_contents t;
  [%expect {| (3 2 1 4 5 6) |}];
  print_endline "Dequeued items:";
  dequeue_until_empty t ~f:(fun x -> print_s [%sexp (x : int)]);
  [%expect
    {|
    Dequeued items:
    6
    5
    4
    1
    2
    3
    |}]
;;
