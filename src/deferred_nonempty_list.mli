(** This interface is generated lazily: if you need a function from [Nonempty_list] that
    isn't present here, feel free to add it. *)

open! Core

type 'a t := 'a Nonempty_list.t
type 'a monad := 'a Deferred1.t
type how := Monad_sequence.how

val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b monad) -> 'b monad
val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b monad) -> 'b monad
val all : 'a monad t -> 'a t monad
val all_unit : unit monad t -> unit monad

(** Iterators *)

val iter : how:how -> 'a t -> f:('a -> unit monad) -> unit monad
val iteri : how:how -> 'a t -> f:(int -> 'a -> unit monad) -> unit monad
val map : how:how -> 'a t -> f:('a -> 'b monad) -> 'b t monad
val mapi : how:how -> 'a t -> f:(int -> 'a -> 'b monad) -> 'b t monad
val filter_map : how:how -> 'a t -> f:('a -> 'b option monad) -> 'b list monad
val filter_mapi : how:how -> 'a t -> f:(int -> 'a -> 'b option monad) -> 'b list monad
val concat_map : how:how -> 'a t -> f:('a -> 'b t monad) -> 'b t monad
val concat_mapi : how:how -> 'a t -> f:(int -> 'a -> 'b t monad) -> 'b t monad
