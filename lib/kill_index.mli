(** Internal to Async -- a timestamp used to implement [Monitor.kill].

    A monitor is dead if it has an ancestor whose kill index is [dead].

    Kill indices are used so that we can efficiently check whether a monitor is dead or
    alive, given that a monitor has a pointer to its parent, but not its children.  So,
    there is no way when killing a monitor to visit all of its descendants.  Instead, we
    rely on lazily checking descendants when they are used.

    Each monitor has a [kill_index : Kill_index.t], and the scheduler has a single
    [global_kill_index : Kill_index.t].  We maintain an invariant on the monitor tree that
    if a monitor's kill index equals [global_kill_index], then all of its ancestors kill
    indices also equal [global_kill_index].  This ensures that any monitor whose kill
    index equals [global_kill_index] is alive.

    All kill indices initially start with value [Kill_index.initial].  To kill a monitor,
    we set that monitor's kill index to [Kill_index.dead] and increment the scheduler's
    [global_kill_index].  A monitor can be in one of three situations:

    - [kill_index = global_kill_index].  The monitor is alive.
    - [kill_index = dead].  The monitor is dead.  A dead monitor is never revived.
    - Otherwise, some monitor has been killed since the monitor was last known to be
    alive.  We must check the monitor's ancestors to see if an ancestor is dead.  If so,
    we will set its [kill_index] to [dead].  If not, we will set its [kill_index] to
    [global_kill_index].
*)

open Core.Std

type t with sexp_of

include Equal.    S with type t := t
include Invariant.S with type t := t

val dead : t
val initial : t
val next : t -> t
