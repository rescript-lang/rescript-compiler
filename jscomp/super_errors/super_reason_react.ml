(* This file detects common error from
  [ReasonReact](https://reasonml.github.io/reason-react/) and provide
  situation-specific hints. See the mli file to see which heurisics we detect
  and related comments *)
open Types

let deconstruct_component_type t =
  match t.desc with
  | Tconstr (p, types, _) when Path.last p = "componentSpec" -> Some types
  | _ -> None

let type_is_component_spec t =
  deconstruct_component_type t <> None

(* recursively drill down the types (first item is the type alias, if any. Second is the content of the alias) *)
let rec get_to_bottom_of_aliases f = function
  | (_alias1, type1) :: (_alias2, type2) :: rest ->
    begin match get_to_bottom_of_aliases f rest with
    | false -> f (type1, type2)
    | true -> true
    end
  | _ -> false

let state_escape_scope = get_to_bottom_of_aliases (function
  (* https://github.com/BuckleScript/ocaml/blob/ddf5a739cc0978dab5e553443825791ba7b0cef9/typing/printtyp.ml?utf8=✓#L1348 *)
  (* so apparently that's the logic for detecting "the constructor out of scope" error *)
  | ({desc = Tconstr (p, _, _)}, {desc = Tvar _; level}) 
    when level < Path.binding_time p -> true
  | _ -> false
)

let is_array_wanted_reactElement = get_to_bottom_of_aliases (function
  | ({desc = Tconstr (path1, _, _)}, {desc = Tconstr (path2, _, _)}) 
    when Path.last path1 = "array" && Path.last path2 = "reactElement" -> true
  | _ -> false
)
