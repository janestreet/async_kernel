open Core.Std

module Ivar = Raw_ivar

module Deferred = struct
  type 'a t = ('a, Execution_context.t) Raw_deferred.t with sexp_of
  let of_ivar = Raw_deferred.of_ivar
  let of_raw = Fn.id
  let to_raw = Fn.id
end

include Ivar.Scheduler_dependent (Raw_scheduler) (struct
  type 'a t = ('a, Execution_context.t) Ivar.t
  let of_raw = Fn.id
  let to_raw = Fn.id
end)

type 'a ivar = 'a t

let invariant a_invariant t = Raw_ivar.invariant a_invariant ignore t

let read = Deferred.of_ivar

let fill_if_empty t v = if is_empty t then fill t v

include Bin_prot.Utils.Make_binable1 (struct
  module Binable = struct
    type 'a t = 'a option with bin_io
  end

  type 'a t = 'a ivar

  let to_binable t = peek t

  let of_binable = function
    | None -> create ()
    | Some a -> create_full a
  ;;
end)
