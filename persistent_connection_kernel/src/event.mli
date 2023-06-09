open! Core

type ('conn, 'address) t =
  | Attempting_to_connect
  | Obtained_address of 'address
  | Failed_to_connect of Error.t
  | Connected of 'conn
  | Disconnected
[@@deriving sexp_of]

val log_level : _ t -> [ `Info | `Debug | `Error ]
