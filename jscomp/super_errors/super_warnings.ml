let fprintf = Format.fprintf
(* taken from https://github.com/BuckleScript/ocaml/blob/d4144647d1bf9bc7dc3aadc24c25a7efa3a67915/utils/warnings.ml#L251 *)
(* actual modified message branches are commented *)
let message (warning : Warnings.t)  =
  match warning with
  | Deprecated s -> s ^ " is deprecated. "
  | Partial_match "" ->
      "You forgot to handle a possible value here, though we don't have more information on the value."
  | Partial_match s ->
      "You forgot to handle a possible value here, for example: \n" ^ s
  | Unerasable_optional_argument ->
      String.concat "\n\n"
        ["This is an optional argument at the final position of the function; omitting it while calling the function might be confused with currying. For example:";
        "  let myTitle = displayTitle \"hello!\";";
        "if `displayTitle` accepts an optional argument at the final position, it'd be unclear whether `myTitle` is a curried function or the final result.";
        "Here's the language's rule: an optional argument is erased as soon as the 1st positional (i.e. neither labeled nor optional) argument defined after it is passed in.";
        "To solve this, you'd conventionally add an extra () argument at the end of the function declaration."]
  | Bad_module_name (modname) ->
      "This file's name is potentially invalid. The build systems conventionally turn a file name into a module name by upper-casing the first letter. " ^ modname ^ " isn't a valid module name.\n" ^
      "Note: some build systems might e.g. turn kebab-case into CamelCase module, which is why this isn't a hard error."
  | Statement_type -> "This expression returns a value, but you're not doing anything with it. If this is on purpose, put `|> ignore` at the end."
  | _ -> Warnings.message warning
;;
