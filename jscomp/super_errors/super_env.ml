let fprintf = Format.fprintf

(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/typing/env.ml#L1842 *)
(* modified branches are commented *)
let report_error ppf = function
  | Env.Illegal_renaming(name, modname, filename) ->
      (* modified *)
      fprintf ppf
        "@[You referred to the module %s, but we've found one called %s instead.@ \
          Is the name's casing right?@]"
        name modname
  | Inconsistent_import(name, source1, source2) ->
      (* modified *)
     fprintf ppf "@[<v>\
        @[@{<info>It's possible that your build is stale.@}@ Try to clean the artifacts and build again?@]@,@,\
        @[@{<info>Here's the original error message@}@]@,\
      @]";
      fprintf ppf
        "@[<hov>The files %a@ and %a@ \
              make inconsistent assumptions@ over interface %s@]"
      Location.print_filename source1 Location.print_filename source2 name
  | Need_recursive_types(import, export) ->
      fprintf ppf
        "@[<hov>Unit %s imports from %s, which uses recursive types.@ %s@]"
        export import "The compilation flag -rectypes is required"
  | Depend_on_unsafe_string_unit(import, export) ->
      fprintf ppf
        "@[<hov>Unit %s imports from %s, compiled with -unsafe-string.@ %s@]"
        export import "This compiler has been configured in strict \
                       safe-string mode (-force-safe-string)"
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

(* This will be called in super_main. This is how you'd override the default error printer from the compiler & register new error_of_exn handlers *)
let setup () =
  Location.register_error_of_exn
    (function
      | Env.Error (Missing_module (loc, _, _)
              | Illegal_value_name (loc, _)
               as err) when loc <> Location.none ->
          Some (Super_location.error_of_printer loc report_error err)
      | Env.Error err -> Some (Super_location.error_of_printer_file report_error err)
      | _ -> None
    )
