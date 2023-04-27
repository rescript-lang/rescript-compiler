type untaggedError = OnlyOneUnknown | AtMostOneObject | AtMostOneArray | AtMostOneString | AtMostOneNumber | DuplicateLiteral of string
type error =
  | InvalidVariantAsAnnotation
  | Duplicated_bs_as
  | InvalidVariantTagAnnotation
  | InvalidUntaggedVariantDefinition of untaggedError
exception Error of Location.t * error

let report_error ppf =
  let open Format in
  function
  | InvalidVariantAsAnnotation ->
    fprintf ppf "A variant case annotation @as(...) must be a string or integer, \
     boolean, null, undefined"
  | Duplicated_bs_as ->
    fprintf ppf "duplicate @as "
  | InvalidVariantTagAnnotation ->
    fprintf ppf "A variant tag annotation @tag(...) must be a string"
  | InvalidUntaggedVariantDefinition untaggedVariant ->
    fprintf ppf "This untagged variant definition is invalid: %s"
    (match untaggedVariant with
    | OnlyOneUnknown -> "An unknown case must be the only case with payloads."
    | AtMostOneObject -> "At most one case can be an object type."
    | AtMostOneArray -> "At most one case can be an array type."
    | AtMostOneString -> "At most one case can be a string type."
    | AtMostOneNumber -> "At most one case can be a number type (int or float)."
    | DuplicateLiteral s -> "Duplicate literal " ^ s ^ "."
    )

(* Type of the runtime representation of an untagged block (case with payoad) *)
type block_type =
  | IntType | StringType | FloatType | ArrayType | ObjectType | UnknownType

(*
  Type of the runtime representation of a tag.
  Can be a literal (case with no payload), or a block (case with payload).
  In the case of block it can be tagged or untagged.
*)
type tag_type =
  | String of string | Int of int | Float of string | Bool of bool | Null | Undefined (* literal or tagged block *)
  | Untagged of block_type (* untagged block *)
type tag = {name: string; tag_type: tag_type option}
type block = {tag: tag; tag_name: string option; block_type: block_type option}
type switch_names = {consts: tag array; blocks: block array}

let untagged = "unboxed"

let has_untagged (attrs: Parsetree.attributes) =
  Ext_list.exists attrs (function ({txt}, _) -> txt = untagged)

let process_untagged (attrs : Parsetree.attributes) =
  let st = ref false in
  Ext_list.iter attrs (fun ({txt}, _) ->
      match txt with
      | "unboxed" -> st := true
      | _ -> ());
  !st

let process_tag_type (attrs : Parsetree.attributes) =
  let st : tag_type option ref = ref None in
  Ext_list.iter attrs (fun (({txt; loc}, payload)) ->
      match txt with
      | "bs.as" | "as" ->
        if !st = None then (
          (match Ast_payload.is_single_string payload with
          | None -> ()
          | Some (s, _dec) ->
            st := Some (String s));
          (match Ast_payload.is_single_int payload with
          | None -> ()
          | Some i ->
            st := Some (Int i));
          (match Ast_payload.is_single_float payload with
          | None -> ()
          | Some f ->
            st := Some (Float f));
          (match Ast_payload.is_single_bool payload with
          | None -> ()
          | Some b ->
            st := Some (Bool b));
          (match Ast_payload.is_single_ident payload with
          | None -> ()
          | Some (Lident "null") ->
            st := Some Null
          | Some (Lident "undefined") ->
            st := Some Undefined
          | Some _ -> raise (Error (loc, InvalidVariantAsAnnotation)));
          if !st = None then raise (Error (loc, InvalidVariantAsAnnotation)))
        else raise (Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
        Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )

let get_block_type ~env (cstr: Types.constructor_declaration) : block_type option =
  match process_untagged cstr.cd_attributes, cstr.cd_args with
  | false, _ -> None
  | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when Path.same path Predef.path_string ->
      Some StringType
  | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when Path.same path Predef.path_int ->
      Some IntType
  | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when Path.same path Predef.path_float ->
      Some FloatType
  | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when Path.same path Predef.path_array ->
      Some ArrayType
  | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when Path. same path Predef.path_string ->
      Some StringType
  | true, Cstr_tuple [{desc = Tconstr (path, _, _)}] when
    let name = Path.name path in
    name = "Js.Dict.t" || name = "Js_dict.t" ->
      Some ObjectType
  | true, Cstr_tuple [ty] ->
    let default = Some UnknownType in
    (match Ctype.extract_concrete_typedecl env ty with
    | (_, _, {type_kind = Type_record (_, Record_unboxed _)}) -> default
    | (_, _, {type_kind = Type_record (_, _)}) -> Some ObjectType
      | _ -> default
      | exception _ -> default
    )
| true, Cstr_tuple (_ :: _ :: _) ->
      (* C(_, _) with at least 2 args is an object *)
      Some ObjectType
  | true, Cstr_record _ ->
      (* inline record is an object *)
      Some ObjectType
  | true, _ -> None (* TODO: add restrictions here *)

let process_tag_name (attrs : Parsetree.attributes) =
  let st = ref None in
  Ext_list.iter attrs (fun (({txt; loc}, payload)) ->
      match txt with
      | "tag" ->
        if !st = None then (
          (match Ast_payload.is_single_string payload with
          | None -> ()
          | Some (s, _dec) ->
            st := Some s);
          if !st = None then raise (Error(loc, InvalidVariantTagAnnotation)))
        else raise (Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st
    
let get_tag_name (cstr: Types.constructor_declaration) =
  process_tag_name cstr.cd_attributes

let is_nullary_variant (x : Types.constructor_arguments) =
  match x with Types.Cstr_tuple [] -> true | _ -> false

let checkInvariant ~isUntaggedDef ~(consts : (Location.t * tag) list) ~(blocks : (Location.t * block) list) =
  let module StringSet = Set.Make(String) in
  let string_literals = ref StringSet.empty in
  let nonstring_literals = ref StringSet.empty in
  let arrayTypes = ref 0 in
  let objectTypes = ref 0 in
  let stringTypes = ref 0 in
  let numberTypes = ref 0 in
  let unknownTypes = ref 0 in
  let addStringLiteral ~loc s = 
    if StringSet.mem s !string_literals then
      raise (Error (loc, InvalidUntaggedVariantDefinition (DuplicateLiteral s)));
    string_literals := StringSet.add s !string_literals in
  let addNonstringLiteral ~loc s = 
    if StringSet.mem s !nonstring_literals then
      raise (Error (loc, InvalidUntaggedVariantDefinition (DuplicateLiteral s)));
      nonstring_literals := StringSet.add s !nonstring_literals in
  let invariant loc =
    if !unknownTypes <> 0 && (List.length blocks <> 1)
      then raise (Error (loc, InvalidUntaggedVariantDefinition OnlyOneUnknown));
    if !objectTypes > 1
      then raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneObject));
    if !arrayTypes > 1
      then raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneArray));
    if !stringTypes > 1
      then raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneString));
    if !numberTypes > 1
      then raise (Error (loc, InvalidUntaggedVariantDefinition AtMostOneNumber));
    () in
  Ext_list.rev_iter consts (fun (loc, literal) -> match literal.tag_type with
    | Some (String s) ->
      addStringLiteral ~loc s
    | Some (Int i) ->
      addNonstringLiteral ~loc (string_of_int i)
    | Some (Float f) ->
      addNonstringLiteral ~loc f
    | Some Null ->
      addNonstringLiteral ~loc "null"
    | Some Undefined ->
      addNonstringLiteral ~loc "undefined"
    | Some (Bool b) ->
      addNonstringLiteral ~loc (if b then "true" else "false")
    | Some (Untagged _) -> ()
    | None ->
      addStringLiteral ~loc literal.name
    );
  if isUntaggedDef then
    Ext_list.rev_iter blocks (fun (loc, block) -> match block.block_type with
    | Some UnknownType ->
      incr unknownTypes;
      invariant loc
    | Some ObjectType ->
      incr objectTypes;
      invariant loc
    | Some ArrayType ->
      incr arrayTypes;
      invariant loc
    | Some (IntType | FloatType) ->
      incr numberTypes;
      invariant loc
    | Some StringType ->
      incr stringTypes;
      invariant loc      
    | None -> ())

let names_from_type_variant ?(isUntaggedDef=false) ~env (cstrs : Types.constructor_declaration list) =
  let get_cstr_name (cstr: Types.constructor_declaration) =
    (cstr.cd_loc,
      { name = Ident.name cstr.cd_id;
        tag_type = process_tag_type cstr.cd_attributes }) in
  let get_block (cstr: Types.constructor_declaration) : block =
    let tag = snd (get_cstr_name cstr) in
    {tag; tag_name = get_tag_name cstr; block_type = get_block_type ~env cstr} in
  let consts, blocks =
    Ext_list.fold_left cstrs ([], []) (fun (consts, blocks) cstr ->
        if is_nullary_variant cstr.cd_args then
          (get_cstr_name cstr :: consts, blocks)
        else (consts, (cstr.cd_loc, get_block cstr) :: blocks))
  in
  checkInvariant ~isUntaggedDef ~consts ~blocks;
  let blocks = blocks |> List.map snd in
  let consts = consts |> List.map snd in
  let consts = Ext_array.reverse_of_list consts in
  let blocks = Ext_array.reverse_of_list blocks in
  Some { consts; blocks }

let check_well_formed ~env ~isUntaggedDef (cstrs: Types.constructor_declaration list) =
  ignore (names_from_type_variant ~env ~isUntaggedDef cstrs)

module DynamiChecks = struct

  type op = EqEqEq | NotEqEq | Or | And
  type 'a t = BinOp of op * 'a t * 'a t | TagType of tag_type | TypeOf of 'a t | IsArray of 'a t | Not of 'a t | Expr of 'a

  let bin op x y = BinOp(op, x, y)
  let tag_type t = TagType t
  let typeof x = TypeOf x
  let str s = String s |> tag_type
  let is_array x = IsArray x
  let not x = Not x
  let nil = Null |> tag_type
  let undefined = Undefined |> tag_type
  let object_ = Untagged ObjectType |> tag_type

  let (==) x y = bin EqEqEq x y
  let (!=) x y = bin NotEqEq x y
  let (|||) x y = bin Or x y
  let (&&&) x y = bin And x y


  let rec is_a_literal_case ~(literal_cases : tag_type list) ~block_cases (e: _ t) =
    let literals_overlaps_with_string () = 
      Ext_list.exists literal_cases (function
        | String _ -> true
        | _ -> false ) in
    let literals_overlaps_with_number () = 
      Ext_list.exists literal_cases (function
        | Int _ | Float _ -> true
        | _ -> false ) in
    let literals_overlaps_with_object () = 
      Ext_list.exists literal_cases (function
        | Null -> true
        | _ -> false ) in
    let is_literal_case (t: tag_type) : _ t =  e == (tag_type t) in
    let is_not_block_case (c: block_type) : _ t = match c with
    | StringType when literals_overlaps_with_string () = false  (* No overlap *) -> 
      (typeof e) != (str "string")
    | IntType when literals_overlaps_with_number () = false ->
      (typeof e) != (str "number")
    | FloatType when literals_overlaps_with_number () = false ->
      (typeof e) != (str "number")
    | ArrayType -> 
      not (is_array e)
    | ObjectType when literals_overlaps_with_object () = false ->
      (typeof e) != (str "object")
    | ObjectType (* overlap *) ->
      e == nil ||| (typeof e != str "object")
    | StringType (* overlap *)
    | IntType (* overlap *)
    | FloatType (* overlap *)
    | UnknownType ->
      (* We don't know the type of unknown, so we need to express:
        this is not one of the literals *)
      (match literal_cases with
        | [] ->
          (* this should not happen *)
          assert false
        | l1 :: others ->
          let is_literal_1 = is_literal_case l1 in
          Ext_list.fold_right others is_literal_1 (fun literal_n acc ->
            (is_literal_case literal_n) ||| acc
          )
      )
    in
    match block_cases with
    | [c] -> is_not_block_case c
    | c1 :: (_::_ as rest) ->
      (is_not_block_case c1) &&& (is_a_literal_case ~literal_cases ~block_cases:rest e)
    | [] -> assert false

  let is_int_tag ?(has_null_undefined_other=(false, false, false)) (e : _ t) : _ t =
    let (has_null, has_undefined, has_other) = has_null_undefined_other in
    if has_null && (has_undefined = false) && (has_other = false) then (* null *)
      bin EqEqEq e nil
    else if has_null && has_undefined && has_other=false then (* null + undefined *)
      e == nil ||| e == undefined
    else if has_null=false && has_undefined && has_other=false then (* undefined *)
       e == undefined
    else if has_null then (* (null + undefined + other) || (null + other) *)
      e == nil ||| typeof e != object_
    else (* (undefiled + other) || other *)
      typeof e != object_
end