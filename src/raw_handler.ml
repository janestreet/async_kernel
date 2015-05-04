module T = struct
  type 'a t =
    { run : 'a -> unit
    ; execution_context : Execution_context.t
    }
end

include T

let filter t ~f = { t with run = fun a -> if f a then t.run a }

let prepend t ~f = { t with run = fun a -> t.run (f a) }
