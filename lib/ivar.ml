open Core.Std
open Import

module Deferred  = Raw_deferred
module Ivar      = Raw_ivar
module Scheduler = Raw_scheduler

include struct
  open Ivar
  let create      = create
  let create_full = create_full
  let equal       = equal
  let is_empty    = is_empty
  let is_full     = is_full
  let peek        = peek
end

include Ivar.Scheduler_dependent (Scheduler)

type 'a ivar = 'a t

type 'a deferred = ('a, Execution_context.t) Raw_deferred.t

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
