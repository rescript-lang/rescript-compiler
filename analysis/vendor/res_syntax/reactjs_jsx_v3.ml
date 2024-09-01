open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

let nolabel = Nolabel

let labelled str = Labelled str

let optional str = Optional str

let is_optional str =
  match str with
  | Optional _ -> true
  | _ -> false

let is_labelled str =
  match str with
  | Labelled _ -> true
  | _ -> false

let get_label str =
  match str with
  | Optional str | Labelled str -> str
  | Nolabel -> ""

let option_ident = Lident "option"

let constant_string ~loc str =
  Ast_helper.Exp.constant ~loc (Pconst_string (str, None))

let safe_type_from_value value_str =
  let value_str = get_label value_str in
  if value_str = "" || (value_str.[0] [@doesNotRaise]) <> '_' then value_str
  else "T" ^ value_str

let key_type loc =
  Typ.constr ~loc {loc; txt = option_ident}
    [Typ.constr ~loc {loc; txt = Lident "string"} []]

type 'a children = ListLiteral of 'a | Exact of 'a

type component_config = {props_name: string}

(* if children is a list, convert it to an array while mapping each element. If not, just map over it, as usual *)
let transform_children_if_list_upper ~loc ~mapper the_list =
  let rec transformChildren_ the_list accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match the_list with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} -> (
      match accum with
      | [single_element] -> Exact single_element
      | accum -> ListLiteral (Exp.array ~loc (List.rev accum)))
    | {
     pexp_desc =
       Pexp_construct
         ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [v; acc]});
    } ->
      transformChildren_ acc (mapper.expr mapper v :: accum)
    | not_a_list -> Exact (mapper.expr mapper not_a_list)
  in
  transformChildren_ the_list []

let transform_children_if_list ~loc ~mapper the_list =
  let rec transformChildren_ the_list accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match the_list with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} ->
      Exp.array ~loc (List.rev accum)
    | {
     pexp_desc =
       Pexp_construct
         ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [v; acc]});
    } ->
      transformChildren_ acc (mapper.expr mapper v :: accum)
    | not_a_list -> mapper.expr mapper not_a_list
  in
  transformChildren_ the_list []

let extract_children ?(remove_last_position_unit = false) ~loc
    props_and_children =
  let rec allButLast_ lst acc =
    match lst with
    | [] -> []
    | [(Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})] ->
      acc
    | (Nolabel, {pexp_loc}) :: _rest ->
      Jsx_common.raise_error ~loc:pexp_loc
        "JSX: found non-labelled argument before the last position"
    | arg :: rest -> allButLast_ rest (arg :: acc)
  in
  let all_but_last lst = allButLast_ lst [] |> List.rev in
  match
    List.partition
      (fun (label, _) -> label = labelled "children")
      props_and_children
  with
  | [], props ->
    (* no children provided? Place a placeholder list *)
    ( Exp.construct ~loc {loc; txt = Lident "[]"} None,
      if remove_last_position_unit then all_but_last props else props )
  | [(_, children_expr)], props ->
    ( children_expr,
      if remove_last_position_unit then all_but_last props else props )
  | _ ->
    Jsx_common.raise_error ~loc
      "JSX: somehow there's more than one `children` label"

let unerasable_ignore loc =
  ( {loc; txt = "warning"},
    PStr [Str.eval (Exp.constant (Pconst_string ("-16", None)))] )

let merlin_focus = ({loc = Location.none; txt = "merlin.focus"}, PStr [])

(* Helper method to filter out any attribute that isn't [@react.component] *)
let other_attrs_pure (loc, _) = loc.txt <> "react.component"

(* Finds the name of the variable the binding is assigned to, otherwise raises Invalid_argument *)
let rec get_fn_name binding =
  match binding with
  | {ppat_desc = Ppat_var {txt}} -> txt
  | {ppat_desc = Ppat_constraint (pat, _)} -> get_fn_name pat
  | {ppat_loc} ->
    Jsx_common.raise_error ~loc:ppat_loc
      "react.component calls cannot be destructured."

let make_new_binding binding expression new_name =
  match binding with
  | {pvb_pat = {ppat_desc = Ppat_var ppat_var} as pvb_pat} ->
    {
      binding with
      pvb_pat =
        {pvb_pat with ppat_desc = Ppat_var {ppat_var with txt = new_name}};
      pvb_expr = expression;
      pvb_attributes = [merlin_focus];
    }
  | {pvb_loc} ->
    Jsx_common.raise_error ~loc:pvb_loc
      "react.component calls cannot be destructured."

(* Lookup the value of `props` otherwise raise Invalid_argument error *)
let get_props_name_value _acc (loc, exp) =
  match (loc, exp) with
  | {txt = Lident "props"}, {pexp_desc = Pexp_ident {txt = Lident str}} ->
    {props_name = str}
  | {txt; loc}, _ ->
    Jsx_common.raise_error ~loc
      "react.component only accepts props as an option, given: { %s }"
      (Longident.last txt)

(* Lookup the `props` record or string as part of [@react.component] and store the name for use when rewriting *)
let get_props_attr payload =
  let default_props = {props_name = "Props"} in
  match payload with
  | Some
      (PStr
        ({
           pstr_desc =
             Pstr_eval ({pexp_desc = Pexp_record (record_fields, None)}, _);
         }
        :: _rest)) ->
    List.fold_left get_props_name_value default_props record_fields
  | Some
      (PStr
        ({
           pstr_desc =
             Pstr_eval ({pexp_desc = Pexp_ident {txt = Lident "props"}}, _);
         }
        :: _rest)) ->
    {props_name = "props"}
  | Some (PStr ({pstr_desc = Pstr_eval (_, _); pstr_loc} :: _rest)) ->
    Jsx_common.raise_error ~loc:pstr_loc
      "react.component accepts a record config with props as an options."
  | _ -> default_props

(* Plucks the label, loc, and type_ from an AST node *)
let pluck_label_default_loc_type (label, default, _, _, loc, type_) =
  (label, default, loc, type_)

(* Lookup the filename from the location information on the AST node and turn it into a valid module identifier *)
let filename_from_loc (pstr_loc : Location.t) =
  let file_name =
    match pstr_loc.loc_start.pos_fname with
    | "" -> !Location.input_name
    | file_name -> file_name
  in
  let file_name =
    try Filename.chop_extension (Filename.basename file_name)
    with Invalid_argument _ -> file_name
  in
  let file_name = String.capitalize_ascii file_name in
  file_name

(* Build a string representation of a module name with segments separated by $ *)
let make_module_name file_name nested_modules fn_name =
  let full_module_name =
    match (file_name, nested_modules, fn_name) with
    (* TODO: is this even reachable? It seems like the fileName always exists *)
    | "", nested_modules, "make" -> nested_modules
    | "", nested_modules, fn_name -> List.rev (fn_name :: nested_modules)
    | file_name, nested_modules, "make" -> file_name :: List.rev nested_modules
    | file_name, nested_modules, fn_name ->
      file_name :: List.rev (fn_name :: nested_modules)
  in
  let full_module_name = String.concat "$" full_module_name in
  full_module_name

(*
  AST node builders
  These functions help us build AST nodes that are needed when transforming a [@react.component] into a
  constructor and a props external
*)

(* Build an AST node representing all named args for the `external` definition for a component's props *)
let rec recursively_make_named_args_for_external list args =
  match list with
  | (label, default, loc, interior_type) :: tl ->
    recursively_make_named_args_for_external tl
      (Typ.arrow ~loc label
         (match (label, interior_type, default) with
         (* ~foo=1 *)
         | label, None, Some _ ->
           {
             ptyp_desc = Ptyp_var (safe_type_from_value label);
             ptyp_loc = loc;
             ptyp_attributes = [];
           }
         (* ~foo: int=1 *)
         | _label, Some type_, Some _ -> type_
         (* ~foo: option<int>=? *)
         | ( label,
             Some {ptyp_desc = Ptyp_constr ({txt = Lident "option"}, [type_])},
             _ )
         | ( label,
             Some
               {
                 ptyp_desc =
                   Ptyp_constr
                     ({txt = Ldot (Lident "*predef*", "option")}, [type_]);
               },
             _ )
         (* ~foo: int=? - note this isnt valid. but we want to get a type error *)
         | label, Some type_, _
           when is_optional label ->
           type_
         (* ~foo=? *)
         | label, None, _ when is_optional label ->
           {
             ptyp_desc = Ptyp_var (safe_type_from_value label);
             ptyp_loc = loc;
             ptyp_attributes = [];
           }
         (* ~foo *)
         | label, None, _ ->
           {
             ptyp_desc = Ptyp_var (safe_type_from_value label);
             ptyp_loc = loc;
             ptyp_attributes = [];
           }
         | _label, Some type_, _ -> type_)
         args)
  | [] -> args

(* Build an AST node for the [@obj] representing props for a component *)
let make_props_value fn_name loc named_arg_list_with_key_and_ref props_type =
  let props_name = fn_name ^ "Props" in
  {
    pval_name = {txt = props_name; loc};
    pval_type =
      recursively_make_named_args_for_external named_arg_list_with_key_and_ref
        (Typ.arrow nolabel
           {
             ptyp_desc = Ptyp_constr ({txt = Lident "unit"; loc}, []);
             ptyp_loc = loc;
             ptyp_attributes = [];
           }
           props_type);
    pval_prim = [""];
    pval_attributes = [({txt = "obj"; loc}, PStr [])];
    pval_loc = loc;
  }

(* Build an AST node representing an `external` with the definition of the [@obj] *)
let make_props_external fn_name loc named_arg_list_with_key_and_ref props_type =
  {
    pstr_loc = loc;
    pstr_desc =
      Pstr_primitive
        (make_props_value fn_name loc named_arg_list_with_key_and_ref props_type);
  }

(* Build an AST node for the signature of the `external` definition *)
let make_props_external_sig fn_name loc named_arg_list_with_key_and_ref
    props_type =
  {
    psig_loc = loc;
    psig_desc =
      Psig_value
        (make_props_value fn_name loc named_arg_list_with_key_and_ref props_type);
  }

(* Build an AST node for the props name when converted to an object inside the function signature  *)
let make_props_name ~loc name =
  {ppat_desc = Ppat_var {txt = name; loc}; ppat_loc = loc; ppat_attributes = []}

let make_object_field loc (str, attrs, type_) =
  Otag ({loc; txt = str}, attrs, type_)

(* Build an AST node representing a "closed" object representing a component's props *)
let make_props_type ~loc named_type_list =
  Typ.mk ~loc
    (Ptyp_object (List.map (make_object_field loc) named_type_list, Closed))

(* Builds an AST node for the entire `external` definition of props *)
let make_external_decl fn_name loc named_arg_list_with_key_and_ref
    named_type_list =
  make_props_external fn_name loc
    (List.map pluck_label_default_loc_type named_arg_list_with_key_and_ref)
    (make_props_type ~loc named_type_list)

let newtype_to_var newtype type_ =
  let var_desc = Ptyp_var ("type-" ^ newtype) in
  let typ (mapper : Ast_mapper.mapper) typ =
    match typ.ptyp_desc with
    | Ptyp_constr ({txt = Lident name}, _) when name = newtype ->
      {typ with ptyp_desc = var_desc}
    | _ -> Ast_mapper.default_mapper.typ mapper typ
  in
  let mapper = {Ast_mapper.default_mapper with typ} in
  mapper.typ mapper type_

(* TODO: some line number might still be wrong *)
let jsx_mapper ~config =
  let transform_uppercase_call3 module_path mapper loc attrs _ call_arguments =
    let children, args_with_labels =
      extract_children ~loc ~remove_last_position_unit:true call_arguments
    in
    let args_for_make = args_with_labels in
    let children_expr =
      transform_children_if_list_upper ~loc ~mapper children
    in
    let recursively_transformed_args_for_make =
      args_for_make
      |> List.map (fun (label, expression) ->
             (label, mapper.expr mapper expression))
    in
    let children_arg = ref None in
    let args =
      recursively_transformed_args_for_make
      @ (match children_expr with
        | Exact children -> [(labelled "children", children)]
        | ListLiteral {pexp_desc = Pexp_array list} when list = [] -> []
        | ListLiteral expression ->
          (* this is a hack to support react components that introspect into their children *)
          children_arg := Some expression;
          [
            ( labelled "children",
              Exp.ident ~loc {loc; txt = Ldot (Lident "React", "null")} );
          ])
      @ [(nolabel, Exp.construct ~loc {loc; txt = Lident "()"} None)]
    in
    let is_cap str = String.capitalize_ascii str = str in
    let ident =
      match module_path with
      | Lident _ -> Ldot (module_path, "make")
      | Ldot (_modulePath, value) as full_path when is_cap value ->
        Ldot (full_path, "make")
      | module_path -> module_path
    in
    let props_ident =
      match ident with
      | Lident path -> Lident (path ^ "Props")
      | Ldot (ident, path) -> Ldot (ident, path ^ "Props")
      | _ ->
        Jsx_common.raise_error ~loc
          "JSX name can't be the result of function applications"
    in
    let props =
      Exp.apply ~attrs ~loc (Exp.ident ~loc {loc; txt = props_ident}) args
    in
    (* handle key, ref, children *)
    (* React.createElement(Component.make, props, ...children) *)
    match !children_arg with
    | None ->
      Exp.apply ~loc ~attrs
        (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "createElement")})
        [(nolabel, Exp.ident ~loc {txt = ident; loc}); (nolabel, props)]
    | Some children ->
      Exp.apply ~loc ~attrs
        (Exp.ident ~loc
           {loc; txt = Ldot (Lident "React", "createElementVariadic")})
        [
          (nolabel, Exp.ident ~loc {txt = ident; loc});
          (nolabel, props);
          (nolabel, children);
        ]
  in

  let transform_lowercase_call3 mapper loc attrs call_arguments id =
    let children, non_children_props = extract_children ~loc call_arguments in
    let component_name_expr = constant_string ~loc id in
    let children_expr = transform_children_if_list ~loc ~mapper children in
    let create_element_call =
      match children with
      (* [@JSX] div(~children=[a]), coming from <div> a </div> *)
      | {
       pexp_desc =
         ( Pexp_construct ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple _})
         | Pexp_construct ({txt = Lident "[]"}, None) );
      } ->
        "createDOMElementVariadic"
      (* [@JSX] div(~children= value), coming from <div> ...(value) </div> *)
      | {pexp_loc} ->
        Jsx_common.raise_error ~loc:pexp_loc
          "A spread as a DOM element's children don't make sense written \
           together. You can simply remove the spread."
    in
    let args =
      match non_children_props with
      | [_justTheUnitArgumentAtEnd] ->
        [
          (* "div" *)
          (nolabel, component_name_expr);
          (* [|moreCreateElementCallsHere|] *)
          (nolabel, children_expr);
        ]
      | non_empty_props ->
        let props_call =
          Exp.apply ~loc
            (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "domProps")})
            (non_empty_props
            |> List.map (fun (label, expression) ->
                   (label, mapper.expr mapper expression)))
        in
        [
          (* "div" *)
          (nolabel, component_name_expr);
          (* ReactDOMRe.props(~className=blabla, ~foo=bar, ()) *)
          (labelled "props", props_call);
          (* [|moreCreateElementCallsHere|] *)
          (nolabel, children_expr);
        ]
    in
    Exp.apply
      ~loc (* throw away the [@JSX] attribute and keep the others, if any *)
      ~attrs
      (* ReactDOMRe.createElement *)
      (Exp.ident ~loc
         {loc; txt = Ldot (Lident "ReactDOMRe", create_element_call)})
      args
  in

  let rec recursively_transform_named_args_for_make expr args newtypes =
    match expr.pexp_desc with
    (* TODO: make this show up with a loc. *)
    | Pexp_fun (Labelled "key", _, _, _) | Pexp_fun (Optional "key", _, _, _) ->
      Jsx_common.raise_error ~loc:expr.pexp_loc
        "Key cannot be accessed inside of a component. Don't worry - you can \
         always key a component from its parent!"
    | Pexp_fun (Labelled "ref", _, _, _) | Pexp_fun (Optional "ref", _, _, _) ->
      Jsx_common.raise_error ~loc:expr.pexp_loc
        "Ref cannot be passed as a normal prop. Either give the prop a \
         different name or use the `forwardRef` API instead."
    | Pexp_fun (arg, default, pattern, expression)
      when is_optional arg || is_labelled arg ->
      let () =
        match (is_optional arg, pattern, default) with
        | true, {ppat_desc = Ppat_constraint (_, {ptyp_desc})}, None -> (
          match ptyp_desc with
          | Ptyp_constr ({txt = Lident "option"}, [_]) -> ()
          | _ ->
            let current_type =
              match ptyp_desc with
              | Ptyp_constr ({txt}, []) ->
                String.concat "." (Longident.flatten txt)
              | Ptyp_constr ({txt}, _innerTypeArgs) ->
                String.concat "." (Longident.flatten txt) ^ "(...)"
              | _ -> "..."
            in
            Location.prerr_warning pattern.ppat_loc
              (Preprocessor
                 (Printf.sprintf
                    "React: optional argument annotations must have explicit \
                     `option`. Did you mean `option<%s>=?`?"
                    current_type)))
        | _ -> ()
      in
      let alias =
        match pattern with
        | {ppat_desc = Ppat_alias (_, {txt}) | Ppat_var {txt}} -> txt
        | {ppat_desc = Ppat_any} -> "_"
        | _ -> get_label arg
      in
      let type_ =
        match pattern with
        | {ppat_desc = Ppat_constraint (_, type_)} -> Some type_
        | _ -> None
      in

      recursively_transform_named_args_for_make expression
        ((arg, default, pattern, alias, pattern.ppat_loc, type_) :: args)
        newtypes
    | Pexp_fun
        ( Nolabel,
          _,
          {ppat_desc = Ppat_construct ({txt = Lident "()"}, _) | Ppat_any},
          _expression ) ->
      (args, newtypes, None)
    | Pexp_fun
        ( Nolabel,
          _,
          {
            ppat_desc =
              Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _);
          },
          _expression ) ->
      (args, newtypes, Some txt)
    | Pexp_fun (Nolabel, _, pattern, _expression) ->
      Location.raise_errorf ~loc:pattern.ppat_loc
        "React: react.component refs only support plain arguments and type \
         annotations."
    | Pexp_newtype (label, expression) ->
      recursively_transform_named_args_for_make expression args
        (label :: newtypes)
    | Pexp_constraint (expression, _typ) ->
      recursively_transform_named_args_for_make expression args newtypes
    | _ -> (args, newtypes, None)
  in

  let arg_to_type types (name, default, _noLabelName, _alias, loc, type_) =
    match (type_, name, default) with
    | Some {ptyp_desc = Ptyp_constr ({txt = Lident "option"}, [type_])}, name, _
      when is_optional name ->
      ( get_label name,
        [],
        {
          type_ with
          ptyp_desc =
            Ptyp_constr ({loc = type_.ptyp_loc; txt = option_ident}, [type_]);
        } )
      :: types
    | Some type_, name, Some _default ->
      ( get_label name,
        [],
        {
          ptyp_desc = Ptyp_constr ({loc; txt = option_ident}, [type_]);
          ptyp_loc = loc;
          ptyp_attributes = [];
        } )
      :: types
    | Some type_, name, _ -> (get_label name, [], type_) :: types
    | None, name, _ when is_optional name ->
      ( get_label name,
        [],
        {
          ptyp_desc =
            Ptyp_constr
              ( {loc; txt = option_ident},
                [
                  {
                    ptyp_desc = Ptyp_var (safe_type_from_value name);
                    ptyp_loc = loc;
                    ptyp_attributes = [];
                  };
                ] );
          ptyp_loc = loc;
          ptyp_attributes = [];
        } )
      :: types
    | None, name, _ when is_labelled name ->
      ( get_label name,
        [],
        {
          ptyp_desc = Ptyp_var (safe_type_from_value name);
          ptyp_loc = loc;
          ptyp_attributes = [];
        } )
      :: types
    | _ -> types
  in

  let arg_to_concrete_type types (name, loc, type_) =
    match name with
    | name when is_labelled name -> (get_label name, [], type_) :: types
    | name when is_optional name ->
      (get_label name, [], Typ.constr ~loc {loc; txt = option_ident} [type_])
      :: types
    | _ -> types
  in

  let nested_modules = ref [] in
  let transform_structure_item item =
    match item with
    (* external *)
    | {
        pstr_loc;
        pstr_desc =
          Pstr_primitive
            ({pval_name = {txt = fn_name}; pval_attributes; pval_type} as
             value_description);
      } as pstr -> (
      match List.filter Jsx_common.has_attr pval_attributes with
      | [] -> [item]
      | [_] ->
        let pval_type = Jsx_common.extract_uncurried pval_type in
        let rec get_prop_types types ({ptyp_loc; ptyp_desc} as full_type) =
          match ptyp_desc with
          | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest))
            when is_labelled name || is_optional name ->
            get_prop_types ((name, ptyp_loc, type_) :: types) rest
          | Ptyp_arrow (Nolabel, _type, rest) -> get_prop_types types rest
          | Ptyp_arrow (name, type_, return_value)
            when is_labelled name || is_optional name ->
            (return_value, (name, return_value.ptyp_loc, type_) :: types)
          | _ -> (full_type, types)
        in
        let inner_type, prop_types = get_prop_types [] pval_type in
        let named_type_list =
          List.fold_left arg_to_concrete_type [] prop_types
        in
        let pluck_label_and_loc (label, loc, type_) =
          (label, None (* default *), loc, Some type_)
        in
        let ret_props_type = make_props_type ~loc:pstr_loc named_type_list in
        let external_props_decl =
          make_props_external fn_name pstr_loc
            ((optional "key", None, pstr_loc, Some (key_type pstr_loc))
            :: List.map pluck_label_and_loc prop_types)
            ret_props_type
        in
        (* can't be an arrow because it will defensively uncurry *)
        let new_external_type =
          Ptyp_constr
            ( {loc = pstr_loc; txt = Ldot (Lident "React", "componentLike")},
              [ret_props_type; inner_type] )
        in
        let new_structure =
          {
            pstr with
            pstr_desc =
              Pstr_primitive
                {
                  value_description with
                  pval_type = {pval_type with ptyp_desc = new_external_type};
                  pval_attributes = List.filter other_attrs_pure pval_attributes;
                };
          }
        in
        [external_props_decl; new_structure]
      | _ ->
        Jsx_common.raise_error ~loc:pstr_loc
          "Only one react.component call can exist on a component at one time")
    (* let component = ... *)
    | {pstr_loc; pstr_desc = Pstr_value (rec_flag, value_bindings)} -> (
      let file_name = filename_from_loc pstr_loc in
      let empty_loc = Location.in_file file_name in
      let map_binding binding =
        if Jsx_common.has_attr_on_binding binding then
          let binding = Jsx_common.remove_arity binding in
          let binding_loc = binding.pvb_loc in
          let binding_pat_loc = binding.pvb_pat.ppat_loc in
          let binding =
            {
              binding with
              pvb_pat = {binding.pvb_pat with ppat_loc = empty_loc};
              pvb_loc = empty_loc;
            }
          in
          let fn_name = get_fn_name binding.pvb_pat in
          let internal_fn_name = fn_name ^ "$Internal" in
          let full_module_name =
            make_module_name file_name !nested_modules fn_name
          in
          let modified_binding_old binding =
            let expression = binding.pvb_expr in
            (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
            let rec spelunk_for_fun_expression expression =
              match expression with
              (* let make = (~prop) => ... *)
              | {pexp_desc = Pexp_fun _} | {pexp_desc = Pexp_newtype _} ->
                expression
              (* let make = {let foo = bar in (~prop) => ...} *)
              | {pexp_desc = Pexp_let (_recursive, _vbs, return_expression)} ->
                (* here's where we spelunk! *)
                spelunk_for_fun_expression return_expression
              (* let make = React.forwardRef((~prop) => ...) *)
              | {
               pexp_desc =
                 Pexp_apply
                   (_wrapperExpression, [(Nolabel, inner_function_expression)]);
              } ->
                spelunk_for_fun_expression inner_function_expression
              | {
               pexp_desc =
                 Pexp_sequence (_wrapperExpression, inner_function_expression);
              } ->
                spelunk_for_fun_expression inner_function_expression
              | {pexp_desc = Pexp_constraint (inner_function_expression, _typ)}
                ->
                spelunk_for_fun_expression inner_function_expression
              | {pexp_loc} ->
                Jsx_common.raise_error ~loc:pexp_loc
                  "react.component calls can only be on function definitions \
                   or component wrappers (forwardRef, memo)."
            in
            spelunk_for_fun_expression expression
          in
          let modified_binding binding =
            let has_application = ref false in
            let wrap_expression_with_binding expression_fn expression =
              Vb.mk ~loc:binding_loc
                ~attrs:(List.filter other_attrs_pure binding.pvb_attributes)
                (Pat.var ~loc:binding_pat_loc
                   {loc = binding_pat_loc; txt = fn_name})
                (expression_fn expression)
            in
            let expression = binding.pvb_expr in
            let unerasable_ignore_exp exp =
              {
                exp with
                pexp_attributes =
                  unerasable_ignore empty_loc :: exp.pexp_attributes;
              }
            in
            (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
            let rec spelunk_for_fun_expression expression =
              match expression with
              (* let make = (~prop) => ... with no final unit *)
              | {
               pexp_desc =
                 Pexp_fun
                   ( ((Labelled _ | Optional _) as label),
                     default,
                     pattern,
                     ({pexp_desc = Pexp_fun _} as internal_expression) );
              } ->
                let wrap, has_unit, exp =
                  spelunk_for_fun_expression internal_expression
                in
                ( wrap,
                  has_unit,
                  unerasable_ignore_exp
                    {
                      expression with
                      pexp_desc = Pexp_fun (label, default, pattern, exp);
                    } )
              (* let make = (()) => ... *)
              (* let make = (_) => ... *)
              | {
               pexp_desc =
                 Pexp_fun
                   ( Nolabel,
                     _default,
                     {
                       ppat_desc =
                         Ppat_construct ({txt = Lident "()"}, _) | Ppat_any;
                     },
                     _internalExpression );
              } ->
                ((fun a -> a), true, expression)
              (* let make = (~prop) => ... *)
              | {
               pexp_desc =
                 Pexp_fun
                   ( (Labelled _ | Optional _),
                     _default,
                     _pattern,
                     _internalExpression );
              } ->
                ((fun a -> a), false, unerasable_ignore_exp expression)
              (* let make = (prop) => ... *)
              | {
               pexp_desc =
                 Pexp_fun (_nolabel, _default, pattern, _internalExpression);
              } ->
                if has_application.contents then
                  ((fun a -> a), false, unerasable_ignore_exp expression)
                else
                  Location.raise_errorf ~loc:pattern.ppat_loc
                    "React: props need to be labelled arguments.\n\
                    \  If you are working with refs be sure to wrap with \
                     React.forwardRef.\n\
                    \  If your component doesn't have any props use () or _ \
                     instead of a name."
              (* let make = {let foo = bar in (~prop) => ...} *)
              | {pexp_desc = Pexp_let (recursive, vbs, internal_expression)} ->
                (* here's where we spelunk! *)
                let wrap, has_unit, exp =
                  spelunk_for_fun_expression internal_expression
                in
                ( wrap,
                  has_unit,
                  {expression with pexp_desc = Pexp_let (recursive, vbs, exp)}
                )
              (* let make = React.forwardRef((~prop) => ...) *)
              | {
               pexp_desc =
                 Pexp_apply
                   (wrapper_expression, [(Nolabel, internal_expression)]);
              } ->
                let () = has_application := true in
                let _, has_unit, exp =
                  spelunk_for_fun_expression internal_expression
                in
                ( (fun exp -> Exp.apply wrapper_expression [(nolabel, exp)]),
                  has_unit,
                  exp )
              | {
               pexp_desc =
                 Pexp_sequence (wrapper_expression, internal_expression);
              } ->
                let wrap, has_unit, exp =
                  spelunk_for_fun_expression internal_expression
                in
                ( wrap,
                  has_unit,
                  {
                    expression with
                    pexp_desc = Pexp_sequence (wrapper_expression, exp);
                  } )
              | e -> ((fun a -> a), false, e)
            in
            let wrap_expression, has_unit, expression =
              spelunk_for_fun_expression expression
            in
            (wrap_expression_with_binding wrap_expression, has_unit, expression)
          in
          let binding_wrapper, has_unit, expression =
            modified_binding binding
          in
          let react_component_attribute =
            try Some (List.find Jsx_common.has_attr binding.pvb_attributes)
            with Not_found -> None
          in
          let _attr_loc, payload =
            match react_component_attribute with
            | Some (loc, payload) -> (loc.loc, Some payload)
            | None -> (empty_loc, None)
          in
          let props = get_props_attr payload in
          (* do stuff here! *)
          let named_arg_list, newtypes, forward_ref =
            recursively_transform_named_args_for_make
              (modified_binding_old binding)
              [] []
          in
          let named_arg_list_with_key_and_ref =
            ( optional "key",
              None,
              Pat.var {txt = "key"; loc = empty_loc},
              "key",
              empty_loc,
              Some (key_type empty_loc) )
            :: named_arg_list
          in
          let named_arg_list_with_key_and_ref =
            match forward_ref with
            | Some _ ->
              ( optional "ref",
                None,
                Pat.var {txt = "key"; loc = empty_loc},
                "ref",
                empty_loc,
                None )
              :: named_arg_list_with_key_and_ref
            | None -> named_arg_list_with_key_and_ref
          in
          let named_arg_list_with_key_and_ref_for_new =
            match forward_ref with
            | Some txt ->
              named_arg_list
              @ [
                  ( nolabel,
                    None,
                    Pat.var {txt; loc = empty_loc},
                    txt,
                    empty_loc,
                    None );
                ]
            | None -> named_arg_list
          in
          let pluck_arg (label, _, _, alias, loc, _) =
            let label_string =
              match label with
              | label when is_optional label || is_labelled label ->
                get_label label
              | _ -> ""
            in
            ( label,
              match label_string with
              | "" -> Exp.ident ~loc {txt = Lident alias; loc}
              | label_string ->
                Exp.apply ~loc
                  (Exp.ident ~loc {txt = Lident "##"; loc})
                  [
                    ( nolabel,
                      Exp.ident ~loc {txt = Lident props.props_name; loc} );
                    (nolabel, Exp.ident ~loc {txt = Lident label_string; loc});
                  ] )
          in
          let named_type_list = List.fold_left arg_to_type [] named_arg_list in
          let loc = empty_loc in
          let external_args =
            (* translate newtypes to type variables *)
            List.fold_left
              (fun args newtype ->
                List.map
                  (fun (a, b, c, d, e, maybe_typ) ->
                    match maybe_typ with
                    | Some typ ->
                      (a, b, c, d, e, Some (newtype_to_var newtype.txt typ))
                    | None -> (a, b, c, d, e, None))
                  args)
              named_arg_list_with_key_and_ref newtypes
          in
          let external_types =
            (* translate newtypes to type variables *)
            List.fold_left
              (fun args newtype ->
                List.map
                  (fun (a, b, typ) -> (a, b, newtype_to_var newtype.txt typ))
                  args)
              named_type_list newtypes
          in
          let external_decl =
            make_external_decl fn_name loc external_args external_types
          in
          let inner_expression_args =
            List.map pluck_arg named_arg_list_with_key_and_ref_for_new
            @
            if has_unit then
              [(Nolabel, Exp.construct {loc; txt = Lident "()"} None)]
            else []
          in
          let inner_expression =
            Exp.apply
              (Exp.ident
                 {
                   loc;
                   txt =
                     Lident
                       (match rec_flag with
                       | Recursive -> internal_fn_name
                       | Nonrecursive -> fn_name);
                 })
              inner_expression_args
          in
          let inner_expression_with_ref =
            match forward_ref with
            | Some txt ->
              {
                inner_expression with
                pexp_desc =
                  Pexp_fun
                    ( nolabel,
                      None,
                      {
                        ppat_desc = Ppat_var {txt; loc = empty_loc};
                        ppat_loc = empty_loc;
                        ppat_attributes = [];
                      },
                      inner_expression );
              }
            | None -> inner_expression
          in
          let full_expression =
            Exp.fun_ nolabel None
              {
                ppat_desc =
                  Ppat_constraint
                    ( make_props_name ~loc:empty_loc props.props_name,
                      make_props_type ~loc:empty_loc external_types );
                ppat_loc = empty_loc;
                ppat_attributes = [];
              }
              inner_expression_with_ref
          in
          let full_expression =
            if !Config.uncurried = Uncurried then
              full_expression
              |> Ast_uncurried.uncurried_fun ~loc:full_expression.pexp_loc
                   ~arity:1
            else full_expression
          in
          let full_expression =
            match full_module_name with
            | "" -> full_expression
            | txt ->
              Exp.let_ Nonrecursive
                [
                  Vb.mk ~loc:empty_loc
                    (Pat.var ~loc:empty_loc {loc = empty_loc; txt})
                    full_expression;
                ]
                (Exp.ident ~loc:empty_loc {loc = empty_loc; txt = Lident txt})
          in
          let bindings, new_binding =
            match rec_flag with
            | Recursive ->
              ( [
                  binding_wrapper
                    (Exp.let_ ~loc:empty_loc Recursive
                       [
                         make_new_binding binding expression internal_fn_name;
                         Vb.mk
                           (Pat.var {loc = empty_loc; txt = fn_name})
                           full_expression;
                       ]
                       (Exp.ident {loc = empty_loc; txt = Lident fn_name}));
                ],
                None )
            | Nonrecursive ->
              ( [{binding with pvb_expr = expression}],
                Some (binding_wrapper full_expression) )
          in
          (Some external_decl, bindings, new_binding)
        else (None, [binding], None)
      in
      let structures_and_binding = List.map map_binding value_bindings in
      let other_structures (extern, binding, new_binding)
          (externs, bindings, new_bindings) =
        let externs =
          match extern with
          | Some extern -> extern :: externs
          | None -> externs
        in
        let new_bindings =
          match new_binding with
          | Some new_binding -> new_binding :: new_bindings
          | None -> new_bindings
        in
        (externs, binding @ bindings, new_bindings)
      in
      let externs, bindings, new_bindings =
        List.fold_right other_structures structures_and_binding ([], [], [])
      in
      externs
      @ [{pstr_loc; pstr_desc = Pstr_value (rec_flag, bindings)}]
      @
      match new_bindings with
      | [] -> []
      | new_bindings ->
        [
          {pstr_loc = empty_loc; pstr_desc = Pstr_value (rec_flag, new_bindings)};
        ])
    | _ -> [item]
  in

  let transform_signature_item item =
    match item with
    | {
        psig_loc;
        psig_desc =
          Psig_value
            ({pval_name = {txt = fn_name}; pval_attributes; pval_type} as
             psig_desc);
      } as psig -> (
      match List.filter Jsx_common.has_attr pval_attributes with
      | [] -> [item]
      | [_] ->
        let pval_type = Jsx_common.extract_uncurried pval_type in
        let rec get_prop_types types ({ptyp_loc; ptyp_desc} as full_type) =
          match ptyp_desc with
          | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest))
            when is_optional name || is_labelled name ->
            get_prop_types ((name, ptyp_loc, type_) :: types) rest
          | Ptyp_arrow (Nolabel, _type, rest) -> get_prop_types types rest
          | Ptyp_arrow (name, type_, return_value)
            when is_optional name || is_labelled name ->
            (return_value, (name, return_value.ptyp_loc, type_) :: types)
          | _ -> (full_type, types)
        in
        let inner_type, prop_types = get_prop_types [] pval_type in
        let named_type_list =
          List.fold_left arg_to_concrete_type [] prop_types
        in
        let pluck_label_and_loc (label, loc, type_) =
          (label, None, loc, Some type_)
        in
        let ret_props_type = make_props_type ~loc:psig_loc named_type_list in
        let external_props_decl =
          make_props_external_sig fn_name psig_loc
            ((optional "key", None, psig_loc, Some (key_type psig_loc))
            :: List.map pluck_label_and_loc prop_types)
            ret_props_type
        in
        (* can't be an arrow because it will defensively uncurry *)
        let new_external_type =
          Ptyp_constr
            ( {loc = psig_loc; txt = Ldot (Lident "React", "componentLike")},
              [ret_props_type; inner_type] )
        in
        let new_structure =
          {
            psig with
            psig_desc =
              Psig_value
                {
                  psig_desc with
                  pval_type = {pval_type with ptyp_desc = new_external_type};
                  pval_attributes = List.filter other_attrs_pure pval_attributes;
                };
          }
        in
        [external_props_decl; new_structure]
      | _ ->
        Jsx_common.raise_error ~loc:psig_loc
          "Only one react.component call can exist on a component at one time")
    | _ -> [item]
  in

  let transform_jsx_call mapper call_expression call_arguments attrs =
    match call_expression.pexp_desc with
    | Pexp_ident caller -> (
      match caller with
      | {txt = Lident "createElement"; loc} ->
        Jsx_common.raise_error ~loc
          "JSX: `createElement` should be preceeded by a module name."
      (* Foo.createElement(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
      | {loc; txt = Ldot (module_path, ("createElement" | "make"))} -> (
        match config.Jsx_common.version with
        | 3 ->
          transform_uppercase_call3 module_path mapper loc attrs call_expression
            call_arguments
        | _ -> Jsx_common.raise_error ~loc "JSX: the JSX version must be  3")
      (* div(~prop1=foo, ~prop2=bar, ~children=[bla], ()) *)
      (* turn that into
         ReactDOMRe.createElement(~props=ReactDOMRe.props(~props1=foo, ~props2=bar, ()), [|bla|]) *)
      | {loc; txt = Lident id} -> (
        match config.version with
        | 3 -> transform_lowercase_call3 mapper loc attrs call_arguments id
        | _ -> Jsx_common.raise_error ~loc "JSX: the JSX version must be 3")
      | {txt = Ldot (_, anything_not_create_element_or_make); loc} ->
        Jsx_common.raise_error ~loc
          "JSX: the JSX attribute should be attached to a \
           `YourModuleName.createElement` or `YourModuleName.make` call. We \
           saw `%s` instead"
          anything_not_create_element_or_make
      | {txt = Lapply _; loc} ->
        (* don't think there's ever a case where this is reached *)
        Jsx_common.raise_error ~loc
          "JSX: encountered a weird case while processing the code. Please \
           report this!")
    | _ ->
      Jsx_common.raise_error ~loc:call_expression.pexp_loc
        "JSX: `createElement` should be preceeded by a simple, direct module \
         name."
  in

  let expr mapper expression =
    match expression with
    (* Does the function application have the @JSX attribute? *)
    | {
     pexp_desc = Pexp_apply (call_expression, call_arguments);
     pexp_attributes;
    } -> (
      let jsx_attribute, non_jsx_attributes =
        List.partition
          (fun (attribute, _) -> attribute.txt = "JSX")
          pexp_attributes
      in
      match (jsx_attribute, non_jsx_attributes) with
      (* no JSX attribute *)
      | [], _ -> default_mapper.expr mapper expression
      | _, non_jsx_attributes ->
        transform_jsx_call mapper call_expression call_arguments
          non_jsx_attributes)
    (* is it a list with jsx attribute? Reason <>foo</> desugars to [@JSX][foo]*)
    | {
        pexp_desc =
          ( Pexp_construct
              ({txt = Lident "::"; loc}, Some {pexp_desc = Pexp_tuple _})
          | Pexp_construct ({txt = Lident "[]"; loc}, None) );
        pexp_attributes;
      } as list_items -> (
      let jsx_attribute, non_jsx_attributes =
        List.partition
          (fun (attribute, _) -> attribute.txt = "JSX")
          pexp_attributes
      in
      match (jsx_attribute, non_jsx_attributes) with
      (* no JSX attribute *)
      | [], _ -> default_mapper.expr mapper expression
      | _, non_jsx_attributes ->
        let loc = {loc with loc_ghost = true} in
        let fragment =
          Exp.ident ~loc {loc; txt = Ldot (Lident "ReasonReact", "fragment")}
        in
        let children_expr =
          transform_children_if_list ~loc ~mapper list_items
        in
        let args =
          [
            (* "div" *)
            (nolabel, fragment);
            (* [|moreCreateElementCallsHere|] *)
            (nolabel, children_expr);
          ]
        in
        Exp.apply
          ~loc (* throw away the [@JSX] attribute and keep the others, if any *)
          ~attrs:non_jsx_attributes
          (* ReactDOMRe.createElement *)
          (Exp.ident ~loc
             {loc; txt = Ldot (Lident "ReactDOMRe", "createElement")})
          args)
    (* Delegate to the default mapper, a deep identity traversal *)
    | e -> default_mapper.expr mapper e
  in

  let module_binding mapper module_binding =
    let _ = nested_modules := module_binding.pmb_name.txt :: !nested_modules in
    let mapped = default_mapper.module_binding mapper module_binding in
    let () =
      match !nested_modules with
      | _ :: rest -> nested_modules := rest
      | [] -> ()
    in
    mapped
  in
  (expr, module_binding, transform_signature_item, transform_structure_item)
