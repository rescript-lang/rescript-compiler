open Format

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/env.ml#L1842 *)
(* modified branches are commented *)
let report_error ppf = function
  | Env.Illegal_renaming(name, modname, filename) -> fprintf ppf
      (* modified *)
      "Hello what error msg goes here: %a@ contains the compiled interface for @ \
       %s when %s was expected"
      Location.print_filename filename name modname
  | Inconsistent_import(name, source1, source2) -> fprintf ppf
      (* TODO: this one needs to be muuuuch better *)
      "@[<hov>The files %a@ and %a@ \
              make inconsistent assumptions@ over interface %s@]"
      Location.print_filename source1 Location.print_filename source2 name
  | Need_recursive_types(import, export) ->
      fprintf ppf
        "@[<hov>Unit %s imports from %s, which uses recursive types.@ %s@]"
        export import "The compilation flag -rectypes is required"
  | Missing_module(_, path1, path2) ->
      fprintf ppf "@[@[<hov>";
      if Path.same path1 path2 then
        fprintf ppf "Internal path@ %s@ is dangling." (Path.name path1)
      else
        fprintf ppf "Internal path@ %s@ expands to@ %s@ which is dangling."
          (Path.name path1) (Path.name path2);
      fprintf ppf "@]@ @[%s@ %s@ %s.@]@]"
        "The compiled interface for module" (Ident.name (Path.head path2))
        "was not found"
  | Illegal_value_name(_loc, name) ->
      fprintf ppf "'%s' is not a valid value identifier."
        name

let () =
  Location.register_error_of_exn
    (function
      | Env.Error (Missing_module (loc, _, _)
              | Illegal_value_name (loc, _)
               as err) when loc <> Location.none ->
          Some (Super_location.error_of_printer loc report_error err)
      | Env.Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
