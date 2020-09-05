let fprintf = Format.fprintf

(* taken from https://github.com/rescript-lang/ocaml/blob/c7767462f3b3b42de9f6e662fa0a94381fd404fd/typing/typedecl.ml#L2000 *)
let report_error ppf = function
  | Typedecl.Nonrec_gadt ->
      fprintf ppf
        "@[GADT case syntax needs to be used in a `rec` type.@]"
  | anythingElse ->
      Typedecl.report_error ppf anythingElse

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Typedecl.Error (loc, err) ->
        Some (Super_location.error_of_printer loc report_error err)
      | _ ->
        None
    )
