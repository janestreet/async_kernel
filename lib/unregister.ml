type t = unit -> unit

let noop () = ()

let create f = f

let unregister f = f ()
