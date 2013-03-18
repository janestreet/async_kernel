open Core.Std

type t = int with sexp_of

let invariant t = assert (t >= -1)

let equal (t1 : t) t2 = t1 = t2

let dead = -1

let initial = 0

let next t = t + 1
