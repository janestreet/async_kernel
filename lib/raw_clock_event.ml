open Core.Std

module Ivar = Raw_ivar

(* Clock events start in the [Uninitialized] state just for their creation (because of
   cyclic types), and then are immediately marked as [Waiting] or [Happened], depending
   on the time.  A [Waiting] event will then either [Happen] at the appropriate time,
   or be [Aborted] prior to when it would have [Happened].

   Uninitialized
   |           |
   v           v
   Waiting --> Happened
   |
   v
   Aborted *)
module T = struct
  type 'execution_context t =
    { mutable state : 'execution_context state;
    }
  and 'execution_context state =
  | Uninitialized
  | Aborted
  | Happened
  | Waiting of 'execution_context waiting
  and 'execution_context waiting =
    { (* The [sexp_opaque] prevents an infinite recursion when converting to a sexp. *)
      event : 'execution_context t sexp_opaque Events.Event.t;
      ready : ([ `Happened | `Aborted ], 'execution_context) Ivar.t;
    }
  with sexp_of
end

include T
