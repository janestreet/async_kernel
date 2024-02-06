open! Core

type ('conn, 'conn_error, 'address) t =
  | Attempting_to_connect
  | Obtained_address of 'address
  | Failed_to_connect of 'conn_error
  | Connected of ('conn[@sexp.opaque])
  | Disconnected
[@@deriving sexp_of]

let log_level = function
  | Attempting_to_connect | Connected _ | Disconnected | Obtained_address _ -> `Info
  | Failed_to_connect _ -> `Error
;;
