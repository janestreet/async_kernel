open! Core

type ('conn, 'conn_error, 'address) t =
  | Attempting_to_connect
  | Obtained_address of 'address
  | Failed_to_connect of 'conn_error
  | Connected of 'conn
  | Disconnected
[@@deriving sexp_of]

val log_level : _ t -> [ `Info | `Debug | `Error ]
