open Core.Std
open Import

module T = struct
  type ('a, 'execution_context) t =
    { execution_context : 'execution_context;
      run : 'a -> unit;
    }
end

include T

let filter t ~f = { t with run = fun a -> if f a then t.run a }

let prepend t ~f = { t with run = fun a -> t.run (f a) }

let create_job { execution_context; run } v = Job.create execution_context run v

