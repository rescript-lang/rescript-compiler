(* This file detects common error from
  [ReasonReact](https://reasonml.github.io/reason-react/) and provide
  situation-specific hints. See the mli file to see which heurisics we detect
  and related comments *)
open Types

let rec drill_through_tlink_and_tsubst t =
  match t.desc with
  | Tlink t
  | Tsubst t -> drill_through_tlink_and_tsubst t
  | _ -> t

let is_weak_type_after_drilling t =
  match drill_through_tlink_and_tsubst t with
  | {desc = Tvar _} -> true
  | _ -> false

let component_spec_weak_type_variables t =
  match drill_through_tlink_and_tsubst t with
  (* ReasonReact <=0.3.4 *)
  | {desc = Tconstr (
      Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _),
      [state; _initial_state; retained_props; _initial_retained_props; action],
      _
    )} ->
    (
      state |> is_weak_type_after_drilling,
      retained_props |> is_weak_type_after_drilling,
      action |> is_weak_type_after_drilling
    )
  (* future ReasonReact version with retainedProps removed *)
  | {desc = Tconstr (
      Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _),
      [state; _initial_state; action],
      _
    )} ->
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

(* `trace` is a funny data structure. It's an always even list of tuples. This error:
  this is foo (aliased as array(int)), wanted bar (aliased as array(string))
  the incompatible part: int vs string
  gives the following `trace` data structure:
  [
    (foo, array(int)),
    (bar, array(string)),
    (_, int),
    (_, string)
  ]
 *)
(* recursively walk the trace from right to left, calling f and checking if f matches part of the trace *)
let check_each_trace_chunk_bottom_up f = fun t ->
  let t_flipped = List.rev t in
  let rec check f = function
  (* we flipped the trace, so instead of [t1, t2, t3, t4, ...] it's [t4, t3, ...] *)
  | (_alias2, type2) :: (_alias1, type1) :: rest ->
    if f (type1, type2) then true
    else check f rest
  | _ -> false
  in
  check f t_flipped


let state_escape_scope = check_each_trace_chunk_bottom_up (function
  (* https://github.com/BuckleScript/ocaml/blob/ddf5a739cc0978dab5e553443825791ba7b0cef9/typing/printtyp.ml?#L1348 *)
  (* so apparently that's the logic for detecting "the constructor out of scope" error *)
  | ({desc = Tconstr (p, _, _)}, {desc = Tvar _; level})
    when level < Path.binding_time p -> true
  | _ -> false
)

let trace_both_component_spec = check_each_trace_chunk_bottom_up (function
  | ({desc = Tconstr (
      (Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _)),
      ([state1; _; _; _; action1] | [state1; _; action1]),
      _
    )},
    {desc = Tconstr (
      (Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _)),
      ([state2; _; _; _; action2] | [state2; _; action2]),
      _
    )})
    -> true
  | _ -> false
)

let is_array_wanted_react_element = check_each_trace_chunk_bottom_up (function
  | ({desc = Tconstr (path1, _, _)},
    {desc = Tconstr (
      (Pdot ((Pident {name = "ReasonReact"}), "reactElement", _)),
      _,
      _
    )}) when Path.last path1 = "array"-> true
  | _ -> false
)

let is_component_spec_wanted_react_element = check_each_trace_chunk_bottom_up (function
  | ({desc = Tconstr (
      (Pdot ((Pident {name = "ReasonReact"}), "componentSpec", _)),
      ([state1; _; _; _; action1] | [state1; _; action1]),
      _
    )},
    {desc = Tconstr (
      (Pdot ((Pident {name = "ReasonReact"}), "reactElement", _)),
      _,
      _
    )}) -> true
  | _ -> false
)
