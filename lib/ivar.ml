open Core.Std

module Deferred = Raw_deferred

include Raw_ivar

let read = Deferred.of_ivar

let fill_if_empty t v = if is_empty t then fill t v

include Bin_prot.Utils.Make_binable1 (struct
  module Binable = struct
    type 'a t = 'a option with bin_io
  end

  type nonrec 'a t = 'a t

  let to_binable t = peek t

  let of_binable = function
    | None -> create ()
    | Some a -> create_full a
  ;;
end)
