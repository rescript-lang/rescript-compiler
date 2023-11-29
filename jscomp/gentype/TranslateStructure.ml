open GenTypeCommon

let rec addAnnotationsToTypes_ ~config ~(expr : Typedtree.expression)
    (argTypes : argType list) =
  match (expr.exp_desc, expr.exp_type.desc, argTypes) with
  | Texp_function {arg_label; param; cases = [{c_rhs}]}, _, {aType} :: nextTypes
    ->
    let nextTypes1 = nextTypes |> addAnnotationsToTypes_ ~config ~expr:c_rhs in
    let aName = Ident.name param in
    let _ = Printtyped.implementation in
    let aName =
      if aName = "*opt*" then
        match arg_label with
        | Optional l -> l
        | _ -> "" (* should not happen *)
      else aName
    in
    {aName; aType} :: nextTypes1
  | Texp_construct ({txt = Lident "Function$"}, _, [funExpr]), _, _ ->
    (* let uncurried1: function$<_, _> = Function$(x => x |> string_of_int, [`Has_arity1]) *)
    addAnnotationsToTypes_ ~config ~expr:funExpr argTypes
  | Texp_apply ({exp_desc = Texp_ident (path, _, _)}, [(_, Some expr1)]), _, _
    -> (
    match path |> TranslateTypeExprFromTypes.pathToList |> List.rev with
    | ["Js"; "Internal"; fn_mk]
      when (* Uncurried function definition uses Js.Internal.fn_mkX(...) *)
           String.length fn_mk >= 5
           && (String.sub fn_mk 0 5 [@doesNotRaise]) = "fn_mk" ->
      argTypes |> addAnnotationsToTypes_ ~config ~expr:expr1
    | _ -> argTypes)
  | _ -> argTypes

and addAnnotationsToTypes ~config ~(expr : Typedtree.expression)
    (argTypes : argType list) =
  let argTypes = addAnnotationsToTypes_ ~config ~expr argTypes in
  if argTypes |> List.filter (fun {aName} -> aName = "param") |> List.length > 1
  then
    (* Underscore "_" appears as "param", can occur more than once *)
    argTypes
    |> List.mapi (fun i {aName; aType} ->
           {aName = aName ^ "_" ^ string_of_int i; aType})
  else argTypes

and addAnnotationsToFields ~config (expr : Typedtree.expression)
    (fields : fields) (argTypes : argType list) =
  match (expr.exp_desc, fields, argTypes) with
  | _, [], _ -> ([], argTypes |> addAnnotationsToTypes ~config ~expr)
  | Texp_function {cases = [{c_rhs}]}, field :: nextFields, _ ->
    let nextFields1, types1 =
      addAnnotationsToFields ~config c_rhs nextFields argTypes
    in
    let name =
      TranslateTypeDeclarations.renameRecordField
        ~attributes:expr.exp_attributes ~name:field.nameJS
    in
    ({field with nameJS = name} :: nextFields1, types1)
  | _ -> (fields, argTypes)
[@@live]

(** Recover from expr the renaming annotations on named arguments. *)
let addAnnotationsToFunctionType ~config (expr : Typedtree.expression)
    (type_ : type_) =
  match type_ with
  | Function function_ ->
    let argTypes = function_.argTypes |> addAnnotationsToTypes ~config ~expr in
    Function {function_ with argTypes}
  | _ -> type_

let removeValueBindingDuplicates structureItems =
  let rec processBindings (bindings : Typedtree.value_binding list) ~seen =
    match bindings with
    | ({vb_pat = {pat_desc = Tpat_var (id, _)}} as binding) :: otherBindings ->
      let name = Ident.name id in
      if !seen |> StringSet.mem name then otherBindings |> processBindings ~seen
      else (
        seen := !seen |> StringSet.add name;
        binding :: (otherBindings |> processBindings ~seen))
    | binding :: otherBindings ->
      binding :: (otherBindings |> processBindings ~seen)
    | [] -> []
  in
  let rec processItems (items : Typedtree.structure_item list) ~acc ~seen =
    match items with
    | ({Typedtree.str_desc = Tstr_value (loc, valueBindings)} as item)
      :: otherItems ->
      let bindings = valueBindings |> processBindings ~seen in
      let item = {item with str_desc = Tstr_value (loc, bindings)} in
      otherItems |> processItems ~acc:(item :: acc) ~seen
    | item :: otherItems -> otherItems |> processItems ~acc:(item :: acc) ~seen
    | [] -> acc
  in
  structureItems |> List.rev |> processItems ~acc:[] ~seen:(ref StringSet.empty)

let translateValueBinding ~config ~outputFileRelative ~resolver ~typeEnv
    {Typedtree.vb_attributes; vb_expr; vb_pat} : Translation.t =
  match vb_pat.pat_desc with
  | Tpat_var (id, _) | Tpat_alias ({pat_desc = Tpat_any}, id, _) ->
    let name = id |> Ident.name in
    if !Debug.translation then Log_.item "Translate Value Binding %s\n" name;
    let moduleItem = Runtime.newModuleItem ~name in
    typeEnv |> TypeEnv.updateModuleItem ~moduleItem;
    if
      vb_attributes
      |> Annotation.fromAttributes ~config ~loc:vb_pat.pat_loc
      = GenType
    then
      id |> Ident.name
      |> Translation.translateValue ~attributes:vb_attributes ~config
           ~docString:(Annotation.docStringFromAttrs vb_attributes)
           ~outputFileRelative ~resolver ~typeEnv ~typeExpr:vb_pat.pat_type
           ~addAnnotationsToFunction:
             (addAnnotationsToFunctionType ~config vb_expr)
    else Translation.empty
  | _ -> Translation.empty

let rec removeDuplicateValueBindings
    (structureItems : Typedtree.structure_item list) =
  match structureItems with
  | ({Typedtree.str_desc = Tstr_value (loc, valueBindings)} as structureItem)
    :: rest ->
    let boundInRest, filteredRest = rest |> removeDuplicateValueBindings in
    let valueBindingsFiltered =
      valueBindings
      |> List.filter (fun valueBinding ->
             match valueBinding with
             | {Typedtree.vb_pat = {pat_desc = Tpat_var (id, _)}} ->
               not (boundInRest |> StringSet.mem (id |> Ident.name))
             | _ -> true)
    in
    let bound =
      valueBindings
      |> List.fold_left
           (fun bound (valueBinding : Typedtree.value_binding) ->
             match valueBinding with
             | {vb_pat = {pat_desc = Tpat_var (id, _)}} ->
               bound |> StringSet.add (id |> Ident.name)
             | _ -> bound)
           boundInRest
    in
    ( bound,
      {structureItem with str_desc = Tstr_value (loc, valueBindingsFiltered)}
      :: filteredRest )
  | structureItem :: rest ->
    let boundInRest, filteredRest = rest |> removeDuplicateValueBindings in
    (boundInRest, structureItem :: filteredRest)
  | [] -> (StringSet.empty, [])

let rec translateModuleBinding ~(config : GenTypeConfig.t) ~outputFileRelative
    ~resolver ~typeEnv
    ({mb_id; mb_expr; mb_attributes} : Typedtree.module_binding) : Translation.t
    =
  let name = mb_id |> Ident.name in
  if !Debug.translation then Log_.item "Translate Module Binding %s\n" name;
  let moduleItem = Runtime.newModuleItem ~name in
  let config = mb_attributes |> Annotation.updateConfigForModule ~config in
  typeEnv |> TypeEnv.updateModuleItem ~moduleItem;
  let typeEnv = typeEnv |> TypeEnv.newModule ~name in
  match mb_expr.mod_desc with
  | Tmod_ident (path, _) -> (
    let dep = path |> Dependencies.fromPath ~config ~typeEnv in
    let internal = dep |> Dependencies.isInternal in
    typeEnv |> TypeEnv.addModuleEquation ~dep ~internal;
    match Env.scrape_alias mb_expr.mod_env mb_expr.mod_type with
    | Mty_signature signature ->
      (* Treat module M = N as include N *)
      signature
      |> TranslateSignatureFromTypes.translateSignatureFromTypes ~config
           ~outputFileRelative ~resolver ~typeEnv
      |> Translation.combine
    | Mty_alias _ | Mty_ident _ | Mty_functor _ -> Translation.empty)
  | Tmod_structure structure ->
    let isLetPrivate =
      mb_attributes |> Annotation.hasAttribute Annotation.tagIsInternLocal
    in
    if isLetPrivate then Translation.empty
    else
      structure
      |> translateStructure ~config ~outputFileRelative ~resolver ~typeEnv
      |> Translation.combine
  | Tmod_apply _ -> (
    (* Only look at the resulting type of the module *)
    match mb_expr.mod_type with
    | Mty_signature signature ->
      signature
      |> TranslateSignatureFromTypes.translateSignatureFromTypes ~config
           ~outputFileRelative ~resolver ~typeEnv
      |> Translation.combine
    | Mty_ident _ ->
      logNotImplemented ("Mty_ident " ^ __LOC__);
      Translation.empty
    | Mty_functor _ ->
      logNotImplemented ("Mty_functor " ^ __LOC__);
      Translation.empty
    | Mty_alias _ ->
      logNotImplemented ("Mty_alias " ^ __LOC__);
      Translation.empty)
  | Tmod_unpack (_, moduleType) -> (
    match moduleType with
    | Mty_signature signature ->
      signature
      |> TranslateSignatureFromTypes.translateSignatureFromTypes ~config
           ~outputFileRelative ~resolver ~typeEnv
      |> Translation.combine
    | Mty_ident path -> (
      match typeEnv |> TypeEnv.lookupModuleTypeSignature ~path with
      | None -> Translation.empty
      | Some (signature, _) ->
        signature
        |> TranslateSignature.translateSignature ~config ~outputFileRelative
             ~resolver ~typeEnv
        |> Translation.combine)
    | Mty_functor _ ->
      logNotImplemented ("Mty_functor " ^ __LOC__);
      Translation.empty
    | Mty_alias _ ->
      logNotImplemented ("Mty_alias " ^ __LOC__);
      Translation.empty)
  | Tmod_functor _ ->
    logNotImplemented ("Tmod_functor " ^ __LOC__);
    Translation.empty
  | Tmod_constraint (_, Mty_ident path, Tmodtype_explicit _, Tcoerce_none) -> (
    match typeEnv |> TypeEnv.lookupModuleTypeSignature ~path with
    | None -> Translation.empty
    | Some (signature, _) ->
      signature
      |> TranslateSignature.translateSignature ~config ~outputFileRelative
           ~resolver ~typeEnv
      |> Translation.combine)
  | Tmod_constraint
      (_, Mty_signature signature, Tmodtype_explicit _, Tcoerce_none) ->
    signature
    |> TranslateSignatureFromTypes.translateSignatureFromTypes ~config
         ~outputFileRelative ~resolver ~typeEnv
    |> Translation.combine
  | Tmod_constraint
      ( {mod_desc = Tmod_structure structure},
        _,
        Tmodtype_implicit,
        Tcoerce_structure _ ) ->
    {
      structure with
      str_items = structure.str_items |> removeDuplicateValueBindings |> snd;
    }
    |> translateStructure ~config ~outputFileRelative ~resolver ~typeEnv
    |> Translation.combine
  | Tmod_constraint
      ( _,
        _,
        Tmodtype_explicit {mty_desc = Tmty_signature {sig_type = signature}},
        _ ) ->
    signature
    |> TranslateSignatureFromTypes.translateSignatureFromTypes ~config
         ~outputFileRelative ~resolver ~typeEnv
    |> Translation.combine
  | Tmod_constraint _ ->
    logNotImplemented ("Tmod_constraint " ^ __LOC__);
    Translation.empty

and translateStructureItem ~config ~outputFileRelative ~resolver ~typeEnv
    (structItem : Typedtree.structure_item) : Translation.t =
  match structItem with
  | {str_desc = Tstr_type (recFlag, typeDeclarations)} ->
    {
      importTypes = [];
      codeItems = [];
      typeDeclarations =
        typeDeclarations
        |> TranslateTypeDeclarations.translateTypeDeclarations ~config
             ~outputFileRelative ~recursive:(recFlag = Recursive) ~resolver
             ~typeEnv;
    }
  | {str_desc = Tstr_value (_loc, valueBindings)} ->
    valueBindings
    |> List.map
         (translateValueBinding ~config ~outputFileRelative ~resolver ~typeEnv)
    |> Translation.combine
  | {str_desc = Tstr_primitive valueDescription} ->
    (* external declaration *)
    valueDescription
    |> Translation.translatePrimitive ~config ~outputFileRelative ~resolver
         ~typeEnv
  | {str_desc = Tstr_module moduleBinding} ->
    moduleBinding
    |> translateModuleBinding ~config ~outputFileRelative ~resolver ~typeEnv
  | {str_desc = Tstr_modtype moduleTypeDeclaration} ->
    moduleTypeDeclaration
    |> TranslateSignature.translateModuleTypeDeclaration ~config
         ~outputFileRelative ~resolver ~typeEnv
  | {str_desc = Tstr_recmodule moduleBindings} ->
    moduleBindings
    |> List.map
         (translateModuleBinding ~config ~outputFileRelative ~resolver ~typeEnv)
    |> Translation.combine
  | {
   str_desc =
     (* ReScript's encoding of bs.module: include with constraint. *)
     Tstr_include
       {
         incl_mod =
           {
             mod_desc =
               Tmod_constraint
                 ( {
                     mod_desc =
                       Tmod_structure
                         {
                           str_items =
                             [({str_desc = Tstr_primitive _} as structItem1)];
                         };
                   },
                   _,
                   _,
                   _ );
           };
         _;
       };
   _;
  } ->
    structItem1
    |> translateStructureItem ~config ~outputFileRelative ~resolver ~typeEnv
  | {str_desc = Tstr_include {incl_type = signature}} ->
    signature
    |> TranslateSignatureFromTypes.translateSignatureFromTypes ~config
         ~outputFileRelative ~resolver ~typeEnv
    |> Translation.combine
  | {str_desc = Tstr_eval _} ->
    logNotImplemented ("Tstr_eval " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_typext _} ->
    logNotImplemented ("Tstr_typext " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_exception _} ->
    logNotImplemented ("Tstr_exception " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_open _} ->
    logNotImplemented ("Tstr_open " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_class _} ->
    logNotImplemented ("Tstr_class " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_class_type _} ->
    logNotImplemented ("Tstr_class_type " ^ __LOC__);
    Translation.empty
  | {str_desc = Tstr_attribute _} ->
    logNotImplemented ("Tstr_attribute " ^ __LOC__);
    Translation.empty

and translateStructure ~config ~outputFileRelative ~resolver ~typeEnv structure
    : Translation.t list =
  if !Debug.translation then Log_.item "Translate Structure\n";
  structure.Typedtree.str_items |> removeValueBindingDuplicates
  |> List.map (fun structItem ->
         structItem
         |> translateStructureItem ~config ~outputFileRelative ~resolver
              ~typeEnv)
