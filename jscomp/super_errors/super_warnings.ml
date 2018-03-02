let fprintf = Format.fprintf
(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/utils/warnings.ml#L251 *)
(* actual modified message branches are commented *)
let message (warning : Warnings.t)  =
  match warning with
  | Partial_match "" ->
      "You forgot to handle a possible value here, though we don't have more information on the value."
  | Partial_match s ->
      "You forgot to handle a possible value here, for example: \n" ^ s
  | Unerasable_optional_argument ->
      String.concat ""
        ["This optional parameter in final position will, in practice, not be optional.\n";
        "  Reorder the parameters so that at least one non-optional one is in final position or, if all parameters are optional, insert a final ().\n\n";
        "  Explanation: If the final parameter is optional, it'd be unclear whether a function application that omits it should be considered fully applied, or partially applied. Imagine writing `let title = display(\"hello!\")`, only to realize `title` isn't your desired result, but a curried call that takes a final optional argument, e.g. `~showDate`.\n\n";
        "  Formal rule: an optional argument is considered intentionally omitted when the 1st positional (i.e. neither labeled nor optional) argument defined after it is passed in."
        ]
  | Bad_module_name (modname) ->
      "This file's name is potentially invalid. The build systems conventionally turn a file name into a module name by upper-casing the first letter. " ^ modname ^ " isn't a valid module name.\n" ^
      "Note: some build systems might e.g. turn kebab-case into CamelCase module, which is why this isn't a hard error."
  | Statement_type -> "This expression returns a value, but you're not doing anything with it. If this is on purpose, put `|> ignore` at the end."
  | _ -> Warnings.message warning
;;
