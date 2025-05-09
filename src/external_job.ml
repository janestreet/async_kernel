open! Core
open! Import
include Types.External_job

let sexp_of_t _ = Sexp.Atom "<job>"

module Encapsulated = struct
  let create ~execution_context ~f ~a =
    let open struct
      (* If a record has only immutable fields, it it always sound to construct a
         [Capsule.Data.t] for that record from [Capsule.Data.t]s within the same
         capsule containing that record's fields. Soon this will be supported by the
         language directly, but in the meantime we have to use magic.
      *)
      external magic_unwrap_capsule : ('a, 'k) Capsule.Data.t -> 'a = "%identity"
      external magic_wrap_capsule : 'a -> ('a, 'k) Capsule.Data.t = "%identity"
    end in
    let execution_context = magic_unwrap_capsule execution_context in
    let f = magic_unwrap_capsule f in
    let a = magic_unwrap_capsule a in
    magic_wrap_capsule (T ({ execution_context; f; a } : _ inner))
  ;;
end
