(* This file detects common error from
  [ReasonReact](https://reasonml.github.io/reason-react/) and provide
  situation-specific hints. See the mli file to see which heurisics we detect
  and related comments *)
open Types

let rec drill_through_tlink_and_tsubst t =
  match t.desc with
  | Tlink t
  | Tsubst t -> drill_through_tlink_and_tsubst t
  | t -> t

let is_weak_type_after_drilling t =
  match drill_through_tlink_and_tsubst t with
  | Tvar _ -> true
  | _ -> false

let component_spec_weak_type_variables t =
  match drill_through_tlink_and_tsubst t with
  (* ReasonReact <=0.3.4 *)
  | Tconstr (
      Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _),
      [state; _initial_state; retained_props; _initial_retained_props; action],
      _
    ) ->
    (
      state |> is_weak_type_after_drilling,
      retained_props |> is_weak_type_after_drilling,
      action |> is_weak_type_after_drilling
    )
  (* future ReasonReact version with retainedProps removed *)
  | Tconstr (
      Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _),
      [state; _initial_state; action],
      _
    ) ->
    (
      state |> is_weak_type_after_drilling,
      false,
      action |> is_weak_type_after_drilling
    )
  | _ -> (false, false, false)

let component_spec_weak_type_variables_in_module_type (mty : Types.module_type) =
  match mty with
  | Mty_signature signature_values ->
      signature_values
        |> List.map (function
          | Sig_value (_id, value_desc) ->
            let typ = value_desc.val_type in
            component_spec_weak_type_variables typ
          | _ -> (false, false, false)
        )
        |> List.filter (function
          | (false, false, false) -> false
          | _ -> true
        )
  | _ -> []

(* recursively drill down the types (first item is the type alias, if any. Second is the content of the alias) *)
let rec get_to_bottom_of_aliases f = function
  | (_alias1, type1) :: (_alias2, type2) :: rest ->
    begin match get_to_bottom_of_aliases f rest with
    | false -> f (type1, type2)
    | true -> true
    end
  | _ -> false

let state_escape_scope = get_to_bottom_of_aliases (function
  (* https://github.com/BuckleScript/ocaml/blob/ddf5a739cc0978dab5e553443825791ba7b0cef9/typing/printtyp.ml?#L1348 *)
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

let is_componentSpec_wanted_reactElement = get_to_bottom_of_aliases (function
  | ({desc = Tconstr (path1, _, _)}, {desc = Tconstr (path2, _, _)})
    when Path.last path1 = "componentSpec" && Path.last path2 = "reactElement" -> true
  | _ -> false
)
