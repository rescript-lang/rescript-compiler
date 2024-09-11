let extract_concrete_typedecl :
    (Env.t -> Types.type_expr -> Path.t * Path.t * Types.type_declaration) ref =
  ref (Obj.magic ())

type 'value value = Known of 'value | Unknown

type object_property = {
  key: string;
  value: runtime_js_value list value;
  optional: bool;
}
and runtime_js_value =
  | String of {value: string value}
  | Number of {value: string value}
  | BigInt of {value: string value}
  | Boolean of {value: bool value}
  | NullLiteral
  | UndefinedLiteral
  | Array of {element_type: runtime_js_value value}
  | Object of {
      properties: object_property list;
      can_have_unknown_properties: bool;
    }
  | Dict of {value_type: runtime_js_value list}
  | Promise of {resolved_type: runtime_js_value value}
  | Any

let rec debug_print_runtime_value (value : runtime_js_value) =
  match value with
  | String {value = Known v} -> Printf.sprintf "String(%s)" v
  | String {value = Unknown} -> "String"
  | Number {value = Known v} -> Printf.sprintf "Number(%s)" v
  | Number {value = Unknown} -> "Number"
  | BigInt {value = Known v} -> Printf.sprintf "BigInt(%s)" v
  | BigInt {value = Unknown} -> "BigInt"
  | Boolean {value = Known v} -> Printf.sprintf "Boolean(%b)" v
  | Boolean {value = Unknown} -> "Boolean"
  | NullLiteral -> "Null"
  | UndefinedLiteral -> "Undefined"
  | Array {element_type = Known v} ->
    Printf.sprintf "Array(%s)" (debug_print_runtime_value v)
  | Array {element_type = Unknown} -> "Array"
  | Object {properties} ->
    Printf.sprintf "Object(%s)"
      (properties
      |> List.map (fun {key; value; optional} ->
             Printf.sprintf "{key: %s, value: %s, optional: %b}" key
               (match value with
               | Known v ->
                 v |> List.map debug_print_runtime_value |> String.concat ", "
               | Unknown -> "Unknown")
               optional)
      |> String.concat ", ")
  | Promise {resolved_type = Known v} ->
    Printf.sprintf "Promise(%s)" (debug_print_runtime_value v)
  | Any -> "Any"
  | _ -> "__other__"

type runtime_representation = {possible_values: runtime_js_value list}

let tag_type_to_possible_values (tag_type : Ast_untagged_variants.tag_type) :
    runtime_js_value =
  match tag_type with
  | String v -> String {value = Known v}
  | Int v -> Number {value = Known (string_of_int v)}
  | Float v -> Number {value = Known v}
  | BigInt v -> BigInt {value = Known v}
  | Bool v -> Boolean {value = Known v}
  | Null -> NullLiteral
  | Undefined -> UndefinedLiteral
  | Untagged (IntType | FloatType) -> Number {value = Unknown}
  | Untagged StringType -> String {value = Unknown}
  | Untagged BooleanType -> Boolean {value = Unknown}
  | Untagged ObjectType ->
    Object {properties = []; can_have_unknown_properties = true}
  | Untagged UnknownType -> Any
  | _ -> Any

let process_fields fields env to_runtime_representation =
  fields
  |> List.map (fun (label : Types.label_declaration) ->
         {
           optional = false (* TODO: Replicate existing rules*);
           key = label.ld_id.name (* TODO: @as attribute *);
           value = Known (to_runtime_representation label.ld_type env);
         })

let rec to_runtime_representation (type_expr : Types.type_expr) (env : Env.t)
    : runtime_js_value list =
  match type_expr.desc with
  (* Builtins *)
  | Tconstr (p, _, _) when Path.same p Predef.path_string ->
    [String {value = Unknown}]
  | Tconstr (p, _, _) when Path.same p Predef.path_bool ->
    [Boolean {value = Unknown}]
  | Tconstr (p, _, _)
    when Path.same p Predef.path_float || Path.same p Predef.path_int ->
    [Number {value = Unknown}]
  | Tconstr (p, [inner], _) when Path.same p Predef.path_option ->
    [UndefinedLiteral] @ to_runtime_representation inner env
  | Tconstr (p, [inner], _) when Path.same p Predef.path_dict ->
    [Dict {value_type = to_runtime_representation inner env}]
  (* Types needing lookup*)
  | Tconstr (_, _, _) -> (
    try
      match !extract_concrete_typedecl env type_expr with
      | _, _, {type_kind = Type_abstract | Type_open} -> [Any]
      | _, _, {type_kind = Type_record (fields, _)} ->
        [
          Object
            {
              properties = process_fields fields env to_runtime_representation;
              can_have_unknown_properties = false;
            };
        ]
      | _, _, {type_kind = Type_variant consructors; type_attributes} ->
        let _unboxed = Ast_untagged_variants.process_untagged type_attributes in
        let tag_name = Ast_untagged_variants.process_tag_name type_attributes in

        consructors
        |> List.map (fun (c : Types.constructor_declaration) ->
               let tag_type =
                 Ast_untagged_variants.process_tag_type c.cd_attributes
               in
               match (c.cd_args, tag_type) with
               | Cstr_tuple [], None -> String {value = Known c.cd_id.name}
               | Cstr_tuple [], Some tag_type ->
                 tag_type_to_possible_values tag_type
               | Cstr_tuple payloads, maybe_tag_type ->
                 let tag_value =
                   match maybe_tag_type with
                   | Some tag_type -> tag_type_to_possible_values tag_type
                   | None -> String {value = Known c.cd_id.name}
                 in
                 Object
                   {
                     properties =
                       [
                         {
                           optional = false;
                           key =
                             (match tag_name with
                             | None -> "TAG"
                             | Some t -> t);
                           value = Known [tag_value];
                         };
                       ]
                       @ (payloads
                         |> List.mapi (fun index (payload : Types.type_expr) ->
                                {
                                  optional = false;
                                  key = "_" ^ string_of_int index;
                                  value =
                                    Known
                                      (to_runtime_representation payload env);
                                }));
                     can_have_unknown_properties = false;
                   }
               | Cstr_record fields, maybe_tag_type ->
                 let tag_value =
                   match maybe_tag_type with
                   | Some tag_type -> tag_type_to_possible_values tag_type
                   | None -> String {value = Known c.cd_id.name}
                 in
                 Object
                   {
                     properties =
                       [
                         {
                           optional = false;
                           key =
                             (match tag_name with
                             | None -> "TAG"
                             | Some t -> t);
                           value = Known [tag_value];
                         };
                       ]
                       @ process_fields fields env to_runtime_representation;
                     can_have_unknown_properties = false;
                   })
    with Not_found -> [Any])
  (* Polyvariants *)
  | Tvariant {row_fields; row_closed} ->
    row_fields
    |> List.map (fun ((label, field) : string * Types.row_field) ->
           match field with
           | Rpresent None -> [String {value = Known label}]
           | Rpresent (Some inner) ->
             [
               Object
                 {
                   can_have_unknown_properties = not row_closed;
                   properties =
                     [
                       {
                         key = "NAME";
                         value = Known [String {value = Known label}];
                         optional = false;
                       };
                       {
                         key = "VAL";
                         optional = false;
                         value = Known (to_runtime_representation inner env);
                       };
                     ];
                 };
             ]
           | _ -> [])
    |> List.concat
  | _ -> []

let runtime_values_match (a : runtime_js_value) (b : runtime_js_value) =
  match (a, b) with
  | String {value = Known a_value}, String {value = Known b_value} ->
    a_value = b_value
  | Number {value = Known a_value}, Number {value = Known b_value} ->
    a_value = b_value
  | BigInt {value = Known a_value}, BigInt {value = Known b_value} ->
    a_value = b_value
  | Boolean {value = Known a_value}, Boolean {value = Known b_value} ->
    a_value = b_value
  | NullLiteral, NullLiteral -> true
  | UndefinedLiteral, UndefinedLiteral -> true
  | _ -> false

let a_can_be_represented_as_b (a : runtime_js_value list)
    (b : runtime_js_value list) =
  a
  |> List.for_all (fun a_value ->
         b |> List.exists (fun b_value -> runtime_values_match a_value b_value))

let log t1 t2 env =
  Printf.sprintf "Can be coerced: %b\n\nt1 dump: %s\n\nt2 dump: %s\n"
    (a_can_be_represented_as_b
       (to_runtime_representation t1 env)
       (to_runtime_representation t2 env))
    (to_runtime_representation t1 env
    |> List.map debug_print_runtime_value
    |> String.concat " | ")
    (to_runtime_representation t2 env
    |> List.map debug_print_runtime_value
    |> String.concat " | ")
