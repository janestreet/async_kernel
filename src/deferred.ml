include Deferred1

module Array    = Deferred_array
module List     = Deferred_list
module Map      = Deferred_map
module Memo     = Deferred_memo
module Option   = Deferred_option
module Or_error = Deferred_or_error
module Queue    = Deferred_queue
module Result   = Deferred_result
module Sequence = Deferred_sequence

include Monad_sequence_unit_tests.Make (Core_kernel.Std.Array)    (Array)
include Monad_sequence_unit_tests.Make (Core_kernel.Std.Sequence) (Sequence)

include Monad_sequence_unit_tests.Make
    (struct
      include Core_kernel.Std.Queue
      let compare cmp t1 t2 = Core_kernel.Std.List.compare cmp (to_list t1) (to_list t2)
    end)
    (Queue)

include Monad_sequence_unit_tests.Make
    (struct
      include Core_kernel.Std.List
      let compare cmp t1 t2 = compare cmp t1 t2
    end)
    (List)
