open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

let nolabel = Nolabel

let labelled str = Labelled str

let isOptional str =
  match str with
  | Optional _ -> true
  | _ -> false

let isLabelled str =
  match str with
  | Labelled _ -> true
  | _ -> false

let isForwardRef = function
  | {pexp_desc = Pexp_ident {txt = Ldot (Lident "React", "forwardRef")}} -> true
  | _ -> false

let getLabel str =
  match str with
  | Optional str | Labelled str -> str
  | Nolabel -> ""

let optionalAttr = ({txt = "ns.optional"; loc = Location.none}, PStr [])
let optionalAttrs = [optionalAttr]

let constantString ~loc str =
  Ast_helper.Exp.constant ~loc (Pconst_string (str, None))

(* {} empty record *)
let emptyRecord ~loc = Exp.record ~loc [] None

let unitExpr ~loc = Exp.construct ~loc (Location.mkloc (Lident "()") loc) None

let safeTypeFromValue valueStr =
  let valueStr = getLabel valueStr in
  if valueStr = "" || (valueStr.[0] [@doesNotRaise]) <> '_' then valueStr
  else "T" ^ valueStr

let refType loc =
  Typ.constr ~loc
    {loc; txt = Ldot (Ldot (Lident "ReactDOM", "Ref"), "currentDomRef")}
    []

type 'a children = ListLiteral of 'a | Exact of 'a

(* if children is a list, convert it to an array while mapping each element. If not, just map over it, as usual *)
let transformChildrenIfListUpper ~mapper theList =
  let rec transformChildren_ theList accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match theList with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} -> (
      match accum with
      | [singleElement] -> Exact singleElement
      | accum -> ListLiteral (Exp.array (List.rev accum)))
    | {
     pexp_desc =
       Pexp_construct
         ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [v; acc]});
    } ->
      transformChildren_ acc (mapper.expr mapper v :: accum)
    | notAList -> Exact (mapper.expr mapper notAList)
  in
  transformChildren_ theList []

let transformChildrenIfList ~mapper theList =
  let rec transformChildren_ theList accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match theList with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} ->
      Exp.array (List.rev accum)
    | {
     pexp_desc =
       Pexp_construct
         ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple [v; acc]});
    } ->
      transformChildren_ acc (mapper.expr mapper v :: accum)
    | notAList -> mapper.expr mapper notAList
  in
  transformChildren_ theList []

let extractChildren ?(removeLastPositionUnit = false) ~loc propsAndChildren =
  let rec allButLast_ lst acc =
    match lst with
    | [] -> []
    | [(Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})] ->
      acc
    | (Nolabel, {pexp_loc}) :: _rest ->
      React_jsx_common.raiseError ~loc:pexp_loc
        "JSX: found non-labelled argument before the last position"
    | arg :: rest -> allButLast_ rest (arg :: acc)
  in
  let allButLast lst = allButLast_ lst [] |> List.rev in
  match
    List.partition
      (fun (label, _) -> label = labelled "children")
      propsAndChildren
  with
  | [], props ->
    (* no children provided? Place a placeholder list *)
    ( Exp.construct {loc = Location.none; txt = Lident "[]"} None,
      if removeLastPositionUnit then allButLast props else props )
  | [(_, childrenExpr)], props ->
    (childrenExpr, if removeLastPositionUnit then allButLast props else props)
  | _ ->
    React_jsx_common.raiseError ~loc
      "JSX: somehow there's more than one `children` label"

let merlinFocus = ({loc = Location.none; txt = "merlin.focus"}, PStr [])

(* Helper method to filter out any attribute that isn't [@react.component] *)
let otherAttrsPure (loc, _) = loc.txt <> "react.component"

(* Finds the name of the variable the binding is assigned to, otherwise raises Invalid_argument *)
let rec getFnName binding =
  match binding with
  | {ppat_desc = Ppat_var {txt}} -> txt
  | {ppat_desc = Ppat_constraint (pat, _)} -> getFnName pat
  | {ppat_loc} ->
    React_jsx_common.raiseError ~loc:ppat_loc
      "react.component calls cannot be destructured."

let makeNewBinding binding expression newName =
  match binding with
  | {pvb_pat = {ppat_desc = Ppat_var ppat_var} as pvb_pat} ->
    {
      binding with
      pvb_pat =
        {pvb_pat with ppat_desc = Ppat_var {ppat_var with txt = newName}};
      pvb_expr = expression;
      pvb_attributes = [merlinFocus];
    }
  | {pvb_loc} ->
    React_jsx_common.raiseError ~loc:pvb_loc
      "react.component calls cannot be destructured."

(* Lookup the filename from the location information on the AST node and turn it into a valid module identifier *)
let filenameFromLoc (pstr_loc : Location.t) =
  let fileName =
    match pstr_loc.loc_start.pos_fname with
    | "" -> !Location.input_name
    | fileName -> fileName
  in
  let fileName =
    try Filename.chop_extension (Filename.basename fileName)
    with Invalid_argument _ -> fileName
  in
  let fileName = String.capitalize_ascii fileName in
  fileName

(* Build a string representation of a module name with segments separated by $ *)
let makeModuleName fileName nestedModules fnName =
  let fullModuleName =
    match (fileName, nestedModules, fnName) with
    (* TODO: is this even reachable? It seems like the fileName always exists *)
    | "", nestedModules, "make" -> nestedModules
    | "", nestedModules, fnName -> List.rev (fnName :: nestedModules)
    | fileName, nestedModules, "make" -> fileName :: List.rev nestedModules
    | fileName, nestedModules, fnName ->
      fileName :: List.rev (fnName :: nestedModules)
  in
  let fullModuleName = String.concat "$" fullModuleName in
  fullModuleName

(*
  AST node builders
  These functions help us build AST nodes that are needed when transforming a [@react.component] into a
  constructor and a props external
  *)

(* make record from props and spread props if exists *)
let recordFromProps ~loc ~removeKey callArguments =
  let spreadPropsLabel = "_spreadProps" in
  let rec removeLastPositionUnitAux props acc =
    match props with
    | [] -> acc
    | [(Nolabel, {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})] ->
      acc
    | (Nolabel, {pexp_loc}) :: _rest ->
      React_jsx_common.raiseError ~loc:pexp_loc
        "JSX: found non-labelled argument before the last position"
    | ((Labelled txt, {pexp_loc}) as prop) :: rest
    | ((Optional txt, {pexp_loc}) as prop) :: rest ->
      if txt = spreadPropsLabel then
        match acc with
        | [] -> removeLastPositionUnitAux rest (prop :: acc)
        | _ ->
          React_jsx_common.raiseError ~loc:pexp_loc
            "JSX: use {...p} {x: v} not {x: v} {...p} \n\
            \     multiple spreads {...p} {...p} not allowed."
      else removeLastPositionUnitAux rest (prop :: acc)
  in
  let props, propsToSpread =
    removeLastPositionUnitAux callArguments []
    |> List.rev
    |> List.partition (fun (label, _) -> label <> labelled "_spreadProps")
  in
  let props =
    if removeKey then
      props |> List.filter (fun (arg_label, _) -> "key" <> getLabel arg_label)
    else props
  in

  let processProp (arg_label, ({pexp_loc} as pexpr)) =
    (* In case filed label is "key" only then change expression to option *)
    let id = getLabel arg_label in
    if isOptional arg_label then
      ( {txt = Lident id; loc = pexp_loc},
        {pexpr with pexp_attributes = optionalAttrs} )
    else ({txt = Lident id; loc = pexp_loc}, pexpr)
  in
  let fields = props |> List.map processProp in
  let spreadFields =
    propsToSpread |> List.map (fun (_, expression) -> expression)
  in
  match (fields, spreadFields) with
  | [], [spreadProps] | [], spreadProps :: _ -> spreadProps
  | _, [] ->
    {
      pexp_desc = Pexp_record (fields, None);
      pexp_loc = loc;
      pexp_attributes = [];
    }
  | _, [spreadProps]
  (* take the first spreadProps only *)
  | _, spreadProps :: _ ->
    {
      pexp_desc = Pexp_record (fields, Some spreadProps);
      pexp_loc = loc;
      pexp_attributes = [];
    }

(* make type params for make fn arguments *)
(* let make = ({id, name, children}: props<'id, 'name, 'children>) *)
let makePropsTypeParamsTvar namedTypeList =
  namedTypeList
  |> List.filter_map (fun (_isOptional, label, _, _interiorType) ->
         if label = "key" then None
         else Some (Typ.var @@ safeTypeFromValue (Labelled label)))

let stripOption coreType =
  match coreType with
  | {ptyp_desc = Ptyp_constr ({txt = Lident "option"}, coreTypes)} ->
    List.nth_opt coreTypes 0 [@doesNotRaise]
  | _ -> Some coreType

let stripJsNullable coreType =
  match coreType with
  | {
   ptyp_desc =
     Ptyp_constr ({txt = Ldot (Ldot (Lident "Js", "Nullable"), "t")}, coreTypes);
  } ->
    List.nth_opt coreTypes 0 [@doesNotRaise]
  | _ -> Some coreType

(* Make type params of the props type *)
(* (Sig) let make: React.componentLike<props<string>, React.element> *)
(* (Str) let make = ({x, _}: props<'x>) => body *)
(* (Str) external make: React.componentLike<props< .. >, React.element> = "default" *)
let makePropsTypeParams ?(stripExplicitOption = false)
    ?(stripExplicitJsNullableOfRef = false) namedTypeList =
  namedTypeList
  |> List.filter_map (fun (isOptional, label, _, interiorType) ->
         if label = "key" then None
           (* TODO: Worth thinking how about "ref_" or "_ref" usages *)
         else if label = "ref" then
           (*
                If ref has a type annotation then use it, else `ReactDOM.Ref.currentDomRef.
                For example, if JSX ppx is used for React Native, type would be different.
             *)
           match interiorType with
           | {ptyp_desc = Ptyp_var "ref"} -> Some (refType Location.none)
           | _ ->
             (* Strip explicit Js.Nullable.t in case of forwardRef *)
             if stripExplicitJsNullableOfRef then stripJsNullable interiorType
             else Some interiorType
           (* Strip the explicit option type in implementation *)
           (* let make = (~x: option<string>=?) => ... *)
         else if isOptional && stripExplicitOption then stripOption interiorType
         else Some interiorType)

let makeLabelDecls ~loc namedTypeList =
  namedTypeList
  |> List.map (fun (isOptional, label, _, interiorType) ->
         if label = "key" then
           Type.field ~loc ~attrs:optionalAttrs {txt = label; loc} interiorType
         else if isOptional then
           Type.field ~loc ~attrs:optionalAttrs {txt = label; loc}
             (Typ.var @@ safeTypeFromValue @@ Labelled label)
         else
           Type.field ~loc {txt = label; loc}
             (Typ.var @@ safeTypeFromValue @@ Labelled label))

let makeTypeDecls propsName loc namedTypeList =
  let labelDeclList = makeLabelDecls ~loc namedTypeList in
  (* 'id, 'className, ... *)
  let params =
    makePropsTypeParamsTvar namedTypeList
    |> List.map (fun coreType -> (coreType, Invariant))
  in
  [
    Type.mk ~loc ~params {txt = propsName; loc}
      ~kind:(Ptype_record labelDeclList);
  ]

let makeTypeDeclsWithCoreType propsName loc coreType typVars =
  [
    Type.mk ~loc {txt = propsName; loc} ~kind:Ptype_abstract
      ~params:(typVars |> List.map (fun v -> (v, Invariant)))
      ~manifest:coreType;
  ]

(* type props<'x, 'y, ...> = { x: 'x, y?: 'y, ... } *)
let makePropsRecordType ~coreTypeOfAttr ~typVarsOfCoreType propsName loc
    namedTypeList =
  Str.type_ Nonrecursive
    (match coreTypeOfAttr with
    | None -> makeTypeDecls propsName loc namedTypeList
    | Some coreType ->
      makeTypeDeclsWithCoreType propsName loc coreType typVarsOfCoreType)

(* type props<'x, 'y, ...> = { x: 'x, y?: 'y, ... } *)
let makePropsRecordTypeSig ~coreTypeOfAttr ~typVarsOfCoreType propsName loc
    namedTypeList =
  Sig.type_ Nonrecursive
    (match coreTypeOfAttr with
    | None -> makeTypeDecls propsName loc namedTypeList
    | Some coreType ->
      makeTypeDeclsWithCoreType propsName loc coreType typVarsOfCoreType)

let transformUppercaseCall3 ~config modulePath mapper jsxExprLoc callExprLoc
    attrs callArguments =
  let children, argsWithLabels =
    extractChildren ~removeLastPositionUnit:true ~loc:jsxExprLoc callArguments
  in
  let argsForMake = argsWithLabels in
  let childrenExpr = transformChildrenIfListUpper ~mapper children in
  let recursivelyTransformedArgsForMake =
    argsForMake
    |> List.map (fun (label, expression) ->
           (label, mapper.expr mapper expression))
  in
  let childrenArg = ref None in
  let args =
    recursivelyTransformedArgsForMake
    @
    match childrenExpr with
    | Exact children -> [(labelled "children", children)]
    | ListLiteral {pexp_desc = Pexp_array list} when list = [] -> []
    | ListLiteral expression -> (
      (* this is a hack to support react components that introspect into their children *)
      childrenArg := Some expression;
      match config.React_jsx_common.mode with
      | "automatic" ->
        [
          ( labelled "children",
            Exp.apply
              (Exp.ident
                 {txt = Ldot (Lident "React", "array"); loc = Location.none})
              [(Nolabel, expression)] );
        ]
      | _ ->
        [
          ( labelled "children",
            Exp.ident {loc = Location.none; txt = Ldot (Lident "React", "null")}
          );
        ])
  in

  let isCap str = String.capitalize_ascii str = str in
  let ident ~suffix =
    match modulePath with
    | Lident _ -> Ldot (modulePath, suffix)
    | Ldot (_modulePath, value) as fullPath when isCap value ->
      Ldot (fullPath, suffix)
    | modulePath -> modulePath
  in
  let isEmptyRecord {pexp_desc} =
    match pexp_desc with
    | Pexp_record (labelDecls, _) when List.length labelDecls = 0 -> true
    | _ -> false
  in

  (* handle key, ref, children *)
  (* React.createElement(Component.make, props, ...children) *)
  let record = recordFromProps ~loc:jsxExprLoc ~removeKey:true args in
  let props =
    if isEmptyRecord record then emptyRecord ~loc:jsxExprLoc else record
  in
  let keyProp =
    args |> List.filter (fun (arg_label, _) -> "key" = getLabel arg_label)
  in
  let makeID =
    Exp.ident ~loc:callExprLoc {txt = ident ~suffix:"make"; loc = callExprLoc}
  in
  match config.mode with
  (* The new jsx transform *)
  | "automatic" ->
    let jsxExpr, keyAndUnit =
      match (!childrenArg, keyProp) with
      | None, key :: _ ->
        ( Exp.ident
            {loc = Location.none; txt = Ldot (Lident "React", "jsxKeyed")},
          [key; (nolabel, unitExpr ~loc:Location.none)] )
      | None, [] ->
        (Exp.ident {loc = Location.none; txt = Ldot (Lident "React", "jsx")}, [])
      | Some _, key :: _ ->
        ( Exp.ident
            {loc = Location.none; txt = Ldot (Lident "React", "jsxsKeyed")},
          [key; (nolabel, unitExpr ~loc:Location.none)] )
      | Some _, [] ->
        ( Exp.ident {loc = Location.none; txt = Ldot (Lident "React", "jsxs")},
          [] )
    in
    Exp.apply ~attrs jsxExpr ([(nolabel, makeID); (nolabel, props)] @ keyAndUnit)
  | _ -> (
    match (!childrenArg, keyProp) with
    | None, key :: _ ->
      Exp.apply ~attrs
        (Exp.ident
           {
             loc = Location.none;
             txt = Ldot (Lident "JsxPPXReactSupport", "createElementWithKey");
           })
        [key; (nolabel, makeID); (nolabel, props)]
    | None, [] ->
      Exp.apply ~attrs
        (Exp.ident
           {loc = Location.none; txt = Ldot (Lident "React", "createElement")})
        [(nolabel, makeID); (nolabel, props)]
    | Some children, key :: _ ->
      Exp.apply ~attrs
        (Exp.ident
           {
             loc = Location.none;
             txt =
               Ldot (Lident "JsxPPXReactSupport", "createElementVariadicWithKey");
           })
        [key; (nolabel, makeID); (nolabel, props); (nolabel, children)]
    | Some children, [] ->
      Exp.apply ~attrs
        (Exp.ident
           {
             loc = Location.none;
             txt = Ldot (Lident "React", "createElementVariadic");
           })
        [(nolabel, makeID); (nolabel, props); (nolabel, children)])

let transformLowercaseCall3 ~config mapper jsxExprLoc callExprLoc attrs
    callArguments id =
  let componentNameExpr = constantString ~loc:callExprLoc id in
  match config.React_jsx_common.mode with
  (* the new jsx transform *)
  | "automatic" ->
    let children, nonChildrenProps =
      extractChildren ~removeLastPositionUnit:true ~loc:jsxExprLoc callArguments
    in
    let argsForMake = nonChildrenProps in
    let childrenExpr = transformChildrenIfListUpper ~mapper children in
    let recursivelyTransformedArgsForMake =
      argsForMake
      |> List.map (fun (label, expression) ->
             (label, mapper.expr mapper expression))
    in
    let childrenArg = ref None in
    let args =
      recursivelyTransformedArgsForMake
      @
      match childrenExpr with
      | Exact children ->
        [
          ( labelled "children",
            Exp.apply ~attrs:optionalAttrs
              (Exp.ident
                 {
                   txt = Ldot (Lident "ReactDOM", "someElement");
                   loc = Location.none;
                 })
              [(Nolabel, children)] );
        ]
      | ListLiteral {pexp_desc = Pexp_array list} when list = [] -> []
      | ListLiteral expression ->
        (* this is a hack to support react components that introspect into their children *)
        childrenArg := Some expression;
        [
          ( labelled "children",
            Exp.apply
              (Exp.ident
                 {txt = Ldot (Lident "React", "array"); loc = Location.none})
              [(Nolabel, expression)] );
        ]
    in
    let isEmptyRecord {pexp_desc} =
      match pexp_desc with
      | Pexp_record (labelDecls, _) when List.length labelDecls = 0 -> true
      | _ -> false
    in
    let record = recordFromProps ~loc:jsxExprLoc ~removeKey:true args in
    let props =
      if isEmptyRecord record then emptyRecord ~loc:jsxExprLoc else record
    in
    let keyProp =
      args |> List.filter (fun (arg_label, _) -> "key" = getLabel arg_label)
    in
    let jsxExpr, keyAndUnit =
      match (!childrenArg, keyProp) with
      | None, key :: _ ->
        ( Exp.ident
            {loc = Location.none; txt = Ldot (Lident "ReactDOM", "jsxKeyed")},
          [key; (nolabel, unitExpr ~loc:Location.none)] )
      | None, [] ->
        ( Exp.ident {loc = Location.none; txt = Ldot (Lident "ReactDOM", "jsx")},
          [] )
      | Some _, key :: _ ->
        ( Exp.ident
            {loc = Location.none; txt = Ldot (Lident "ReactDOM", "jsxsKeyed")},
          [key; (nolabel, unitExpr ~loc:Location.none)] )
      | Some _, [] ->
        ( Exp.ident {loc = Location.none; txt = Ldot (Lident "ReactDOM", "jsxs")},
          [] )
    in
    Exp.apply ~attrs jsxExpr
      ([(nolabel, componentNameExpr); (nolabel, props)] @ keyAndUnit)
  | _ ->
    let children, nonChildrenProps =
      extractChildren ~loc:jsxExprLoc callArguments
    in
    let childrenExpr = transformChildrenIfList ~mapper children in
    let createElementCall =
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
        React_jsx_common.raiseError ~loc:pexp_loc
          "A spread as a DOM element's children don't make sense written \
           together. You can simply remove the spread."
    in
    let args =
      match nonChildrenProps with
      | [_justTheUnitArgumentAtEnd] ->
        [
          (* "div" *)
          (nolabel, componentNameExpr);
          (* [|moreCreateElementCallsHere|] *)
          (nolabel, childrenExpr);
        ]
      | nonEmptyProps ->
        let propsRecord =
          recordFromProps ~loc:Location.none ~removeKey:false nonEmptyProps
        in
        [
          (* "div" *)
          (nolabel, componentNameExpr);
          (* ReactDOM.domProps(~className=blabla, ~foo=bar, ()) *)
          (labelled "props", propsRecord);
          (* [|moreCreateElementCallsHere|] *)
          (nolabel, childrenExpr);
        ]
    in
    Exp.apply ~loc:jsxExprLoc ~attrs
      (* ReactDOM.createElement *)
      (Exp.ident
         {
           loc = Location.none;
           txt = Ldot (Lident "ReactDOM", createElementCall);
         })
      args

let rec recursivelyTransformNamedArgsForMake mapper expr args newtypes coreType
    =
  let expr = mapper.expr mapper expr in
  match expr.pexp_desc with
  (* TODO: make this show up with a loc. *)
  | Pexp_fun (Labelled "key", _, _, _) | Pexp_fun (Optional "key", _, _, _) ->
    React_jsx_common.raiseError ~loc:expr.pexp_loc
      "Key cannot be accessed inside of a component. Don't worry - you can \
       always key a component from its parent!"
  | Pexp_fun (Labelled "ref", _, _, _) | Pexp_fun (Optional "ref", _, _, _) ->
    React_jsx_common.raiseError ~loc:expr.pexp_loc
      "Ref cannot be passed as a normal prop. Please use `forwardRef` API \
       instead."
  | Pexp_fun (arg, default, pattern, expression)
    when isOptional arg || isLabelled arg ->
    let () =
      match (isOptional arg, pattern, default) with
      | true, {ppat_desc = Ppat_constraint (_, {ptyp_desc})}, None -> (
        match ptyp_desc with
        | Ptyp_constr ({txt = Lident "option"}, [_]) -> ()
        | _ ->
          let currentType =
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
                   `option`. Did you mean `option(%s)=?`?"
                  currentType)))
      | _ -> ()
    in
    let alias =
      match pattern with
      | {ppat_desc = Ppat_alias (_, {txt}) | Ppat_var {txt}} -> txt
      | {ppat_desc = Ppat_any} -> "_"
      | _ -> getLabel arg
    in
    let type_ =
      match pattern with
      | {ppat_desc = Ppat_constraint (_, type_)} -> Some type_
      | _ -> None
    in

    recursivelyTransformNamedArgsForMake mapper expression
      ((arg, default, pattern, alias, pattern.ppat_loc, type_) :: args)
      newtypes coreType
  | Pexp_fun
      ( Nolabel,
        _,
        {ppat_desc = Ppat_construct ({txt = Lident "()"}, _) | Ppat_any},
        _expression ) ->
    (args, newtypes, coreType)
  | Pexp_fun
      ( Nolabel,
        _,
        ({
           ppat_desc =
             Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _);
         } as pattern),
        _expression ) ->
    if txt = "ref" then
      let type_ =
        match pattern with
        | {ppat_desc = Ppat_constraint (_, type_)} -> Some type_
        | _ -> None
      in
      (* The ref arguement of forwardRef should be optional *)
      ( (Optional "ref", None, pattern, txt, pattern.ppat_loc, type_) :: args,
        newtypes,
        coreType )
    else (args, newtypes, coreType)
  | Pexp_fun (Nolabel, _, pattern, _expression) ->
    Location.raise_errorf ~loc:pattern.ppat_loc
      "React: react.component refs only support plain arguments and type \
       annotations."
  | Pexp_newtype (label, expression) ->
    recursivelyTransformNamedArgsForMake mapper expression args
      (label :: newtypes) coreType
  | Pexp_constraint (expression, coreType) ->
    recursivelyTransformNamedArgsForMake mapper expression args newtypes
      (Some coreType)
  | _ -> (args, newtypes, coreType)

let newtypeToVar newtype type_ =
  let var_desc = Ptyp_var ("type-" ^ newtype) in
  let typ (mapper : Ast_mapper.mapper) typ =
    match typ.ptyp_desc with
    | Ptyp_constr ({txt = Lident name}, _) when name = newtype ->
      {typ with ptyp_desc = var_desc}
    | _ -> Ast_mapper.default_mapper.typ mapper typ
  in
  let mapper = {Ast_mapper.default_mapper with typ} in
  mapper.typ mapper type_

let argToType ~newtypes ~(typeConstraints : core_type option) types
    (name, default, _noLabelName, _alias, loc, type_) =
  let rec getType name coreType =
    match coreType with
    | {ptyp_desc = Ptyp_arrow (arg, c1, c2)} ->
      if name = arg then Some c1 else getType name c2
    | _ -> None
  in
  let typeConst = Option.bind typeConstraints (getType name) in
  let type_ =
    List.fold_left
      (fun type_ newtype ->
        match (type_, typeConst) with
        | _, Some typ | Some typ, None -> Some (newtypeToVar newtype.txt typ)
        | _ -> None)
      type_ newtypes
  in
  match (type_, name, default) with
  | Some type_, name, _ when isOptional name ->
    (true, getLabel name, [], {type_ with ptyp_attributes = optionalAttrs})
    :: types
  | Some type_, name, _ -> (false, getLabel name, [], type_) :: types
  | None, name, _ when isOptional name ->
    ( true,
      getLabel name,
      [],
      Typ.var ~loc ~attrs:optionalAttrs (safeTypeFromValue name) )
    :: types
  | None, name, _ when isLabelled name ->
    (false, getLabel name, [], Typ.var ~loc (safeTypeFromValue name)) :: types
  | _ -> types

let argWithDefaultValue (name, default, _, _, _, _) =
  match default with
  | Some default when isOptional name -> Some (getLabel name, default)
  | _ -> None

let argToConcreteType types (name, _loc, type_) =
  match name with
  | name when isLabelled name -> (false, getLabel name, [], type_) :: types
  | name when isOptional name -> (true, getLabel name, [], type_) :: types
  | _ -> types

let check_string_int_attribute_iter =
  let attribute _ ({txt; loc}, _) =
    if txt = "string" || txt = "int" then
      React_jsx_common.raiseError ~loc
        "@string and @int attributes not supported. See \
         https://github.com/rescript-lang/rescript-compiler/issues/5724"
  in

  {Ast_iterator.default_iterator with attribute}

let transformStructureItem ~config mapper item =
  match item with
  (* external *)
  | {
      pstr_loc;
      pstr_desc =
        Pstr_primitive ({pval_attributes; pval_type} as value_description);
    } as pstr -> (
    match List.filter React_jsx_common.hasAttr pval_attributes with
    | [] -> [item]
    | [_] ->
      (* If there is another @react.component, throw error *)
      if config.React_jsx_common.hasReactComponent then
        React_jsx_common.raiseErrorMultipleReactComponent ~loc:pstr_loc
      else (
        config.hasReactComponent <- true;
        check_string_int_attribute_iter.structure_item
          check_string_int_attribute_iter item;
        let coreTypeOfAttr = React_jsx_common.coreTypeOfAttrs pval_attributes in
        let typVarsOfCoreType =
          coreTypeOfAttr
          |> Option.map React_jsx_common.typVarsOfCoreType
          |> Option.value ~default:[]
        in
        let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
          match ptyp_desc with
          | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest))
            when isLabelled name || isOptional name ->
            getPropTypes ((name, ptyp_loc, type_) :: types) rest
          | Ptyp_arrow (Nolabel, _type, rest) -> getPropTypes types rest
          | Ptyp_arrow (name, type_, returnValue)
            when isLabelled name || isOptional name ->
            (returnValue, (name, returnValue.ptyp_loc, type_) :: types)
          | _ -> (fullType, types)
        in
        let innerType, propTypes = getPropTypes [] pval_type in
        let namedTypeList = List.fold_left argToConcreteType [] propTypes in
        let retPropsType =
          Typ.constr ~loc:pstr_loc
            (Location.mkloc (Lident "props") pstr_loc)
            (match coreTypeOfAttr with
            | None -> makePropsTypeParams namedTypeList
            | Some _ -> (
              match typVarsOfCoreType with
              | [] -> []
              | _ -> [Typ.any ()]))
        in
        (* type props<'x, 'y> = { x: 'x, y?: 'y, ... } *)
        let propsRecordType =
          makePropsRecordType ~coreTypeOfAttr ~typVarsOfCoreType "props"
            pstr_loc namedTypeList
        in
        (* can't be an arrow because it will defensively uncurry *)
        let newExternalType =
          Ptyp_constr
            ( {loc = pstr_loc; txt = Ldot (Lident "React", "componentLike")},
              [retPropsType; innerType] )
        in
        let newStructure =
          {
            pstr with
            pstr_desc =
              Pstr_primitive
                {
                  value_description with
                  pval_type = {pval_type with ptyp_desc = newExternalType};
                  pval_attributes = List.filter otherAttrsPure pval_attributes;
                };
          }
        in
        [propsRecordType; newStructure])
    | _ ->
      React_jsx_common.raiseError ~loc:pstr_loc
        "Only one react.component call can exist on a component at one time")
  (* let component = ... *)
  | {pstr_loc; pstr_desc = Pstr_value (recFlag, valueBindings)} -> (
    let fileName = filenameFromLoc pstr_loc in
    let emptyLoc = Location.in_file fileName in
    let mapBinding binding =
      if React_jsx_common.hasAttrOnBinding binding then
        if config.hasReactComponent then
          React_jsx_common.raiseErrorMultipleReactComponent ~loc:pstr_loc
        else (
          config.hasReactComponent <- true;
          let coreTypeOfAttr =
            React_jsx_common.coreTypeOfAttrs binding.pvb_attributes
          in
          let typVarsOfCoreType =
            coreTypeOfAttr
            |> Option.map React_jsx_common.typVarsOfCoreType
            |> Option.value ~default:[]
          in
          let bindingLoc = binding.pvb_loc in
          let bindingPatLoc = binding.pvb_pat.ppat_loc in
          let binding =
            {
              binding with
              pvb_pat = {binding.pvb_pat with ppat_loc = emptyLoc};
              pvb_loc = emptyLoc;
            }
          in
          let fnName = getFnName binding.pvb_pat in
          let internalFnName = fnName ^ "$Internal" in
          let fullModuleName =
            makeModuleName fileName config.nestedModules fnName
          in
          let modifiedBindingOld binding =
            let expression = binding.pvb_expr in
            (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
            let rec spelunkForFunExpression expression =
              match expression with
              (* let make = (~prop) => ... *)
              | {pexp_desc = Pexp_fun _} | {pexp_desc = Pexp_newtype _} ->
                expression
              (* let make = {let foo = bar in (~prop) => ...} *)
              | {pexp_desc = Pexp_let (_recursive, _vbs, returnExpression)} ->
                (* here's where we spelunk! *)
                spelunkForFunExpression returnExpression
              (* let make = React.forwardRef((~prop) => ...) *)
              | {
               pexp_desc =
                 Pexp_apply
                   (_wrapperExpression, [(Nolabel, innerFunctionExpression)]);
              } ->
                spelunkForFunExpression innerFunctionExpression
              | {
               pexp_desc =
                 Pexp_sequence (_wrapperExpression, innerFunctionExpression);
              } ->
                spelunkForFunExpression innerFunctionExpression
              | {pexp_desc = Pexp_constraint (innerFunctionExpression, _typ)} ->
                spelunkForFunExpression innerFunctionExpression
              | {pexp_loc} ->
                React_jsx_common.raiseError ~loc:pexp_loc
                  "react.component calls can only be on function definitions \
                   or component wrappers (forwardRef, memo)."
            in
            spelunkForFunExpression expression
          in
          let modifiedBinding binding =
            let hasApplication = ref false in
            let wrapExpressionWithBinding expressionFn expression =
              Vb.mk ~loc:bindingLoc
                ~attrs:(List.filter otherAttrsPure binding.pvb_attributes)
                (Pat.var ~loc:bindingPatLoc {loc = bindingPatLoc; txt = fnName})
                (expressionFn expression)
            in
            let expression = binding.pvb_expr in
            (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
            let rec spelunkForFunExpression expression =
              match expression with
              (* let make = (~prop) => ... with no final unit *)
              | {
               pexp_desc =
                 Pexp_fun
                   ( ((Labelled _ | Optional _) as label),
                     default,
                     pattern,
                     ({pexp_desc = Pexp_fun _} as internalExpression) );
              } ->
                let wrap, hasForwardRef, exp =
                  spelunkForFunExpression internalExpression
                in
                ( wrap,
                  hasForwardRef,
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
                ((fun a -> a), false, expression)
              (* let make = (~prop) => ... *)
              | {
               pexp_desc =
                 Pexp_fun
                   ( (Labelled _ | Optional _),
                     _default,
                     _pattern,
                     _internalExpression );
              } ->
                ((fun a -> a), false, expression)
              (* let make = (prop) => ... *)
              | {
               pexp_desc =
                 Pexp_fun (_nolabel, _default, pattern, _internalExpression);
              } ->
                if !hasApplication then ((fun a -> a), false, expression)
                else
                  Location.raise_errorf ~loc:pattern.ppat_loc
                    "React: props need to be labelled arguments.\n\
                    \  If you are working with refs be sure to wrap with \
                     React.forwardRef.\n\
                    \  If your component doesn't have any props use () or _ \
                     instead of a name."
              (* let make = {let foo = bar in (~prop) => ...} *)
              | {pexp_desc = Pexp_let (recursive, vbs, internalExpression)} ->
                (* here's where we spelunk! *)
                let wrap, hasForwardRef, exp =
                  spelunkForFunExpression internalExpression
                in
                ( wrap,
                  hasForwardRef,
                  {expression with pexp_desc = Pexp_let (recursive, vbs, exp)}
                )
              (* let make = React.forwardRef((~prop) => ...) *)
              | {
               pexp_desc =
                 Pexp_apply (wrapperExpression, [(Nolabel, internalExpression)]);
              } ->
                let () = hasApplication := true in
                let _, _, exp = spelunkForFunExpression internalExpression in
                let hasForwardRef = isForwardRef wrapperExpression in
                ( (fun exp -> Exp.apply wrapperExpression [(nolabel, exp)]),
                  hasForwardRef,
                  exp )
              | {
               pexp_desc = Pexp_sequence (wrapperExpression, internalExpression);
              } ->
                let wrap, hasForwardRef, exp =
                  spelunkForFunExpression internalExpression
                in
                ( wrap,
                  hasForwardRef,
                  {
                    expression with
                    pexp_desc = Pexp_sequence (wrapperExpression, exp);
                  } )
              | e -> ((fun a -> a), false, e)
            in
            let wrapExpression, hasForwardRef, expression =
              spelunkForFunExpression expression
            in
            (wrapExpressionWithBinding wrapExpression, hasForwardRef, expression)
          in
          let bindingWrapper, hasForwardRef, expression =
            modifiedBinding binding
          in
          (* do stuff here! *)
          let namedArgList, newtypes, typeConstraints =
            recursivelyTransformNamedArgsForMake mapper
              (modifiedBindingOld binding)
              [] [] None
          in
          let namedTypeList =
            List.fold_left
              (argToType ~newtypes ~typeConstraints)
              [] namedArgList
          in
          let namedArgWithDefaultValueList =
            List.filter_map argWithDefaultValue namedArgList
          in
          let vbMatch (label, default) =
            Vb.mk
              (Pat.var (Location.mknoloc label))
              (Exp.match_
                 (Exp.ident {txt = Lident label; loc = Location.none})
                 [
                   Exp.case
                     (Pat.construct
                        (Location.mknoloc @@ Lident "Some")
                        (Some (Pat.var (Location.mknoloc label))))
                     (Exp.ident (Location.mknoloc @@ Lident label));
                   Exp.case
                     (Pat.construct (Location.mknoloc @@ Lident "None") None)
                     default;
                 ])
          in
          let vbMatchList = List.map vbMatch namedArgWithDefaultValueList in
          (* type props = { ... } *)
          let propsRecordType =
            makePropsRecordType ~coreTypeOfAttr ~typVarsOfCoreType "props"
              pstr_loc namedTypeList
          in
          let innerExpression =
            Exp.apply
              (Exp.ident (Location.mknoloc @@ Lident fnName))
              ([(Nolabel, Exp.ident (Location.mknoloc @@ Lident "props"))]
              @
              match hasForwardRef with
              | true ->
                [(Nolabel, Exp.ident (Location.mknoloc @@ Lident "ref"))]
              | false -> [])
          in
          let makePropsPattern = function
            | [] -> Pat.var @@ Location.mknoloc "props"
            | _ ->
              Pat.constraint_
                (Pat.var @@ Location.mknoloc "props")
                (Typ.constr (Location.mknoloc @@ Lident "props") [Typ.any ()])
          in
          let fullExpression =
            (* React component name should start with uppercase letter *)
            (* let make = { let \"App" = props => make(props); \"App" } *)
            (* let make = React.forwardRef({
                 let \"App" = (props, ref) => make({...props, ref: @optional (Js.Nullabel.toOption(ref))})
               })*)
            Exp.fun_ nolabel None
              (match coreTypeOfAttr with
              | None -> makePropsPattern namedTypeList
              | Some _ -> makePropsPattern typVarsOfCoreType)
              (if hasForwardRef then
               Exp.fun_ nolabel None
                 (Pat.var @@ Location.mknoloc "ref")
                 innerExpression
              else innerExpression)
          in
          let fullExpression =
            match fullModuleName with
            | "" -> fullExpression
            | txt ->
              Exp.let_ Nonrecursive
                [
                  Vb.mk ~loc:emptyLoc
                    (Pat.var ~loc:emptyLoc {loc = emptyLoc; txt})
                    fullExpression;
                ]
                (Exp.ident ~loc:pstr_loc {loc = emptyLoc; txt = Lident txt})
          in
          let rec stripConstraintUnpack ~label pattern =
            match pattern with
            | {ppat_desc = Ppat_constraint (pattern, _)} ->
              stripConstraintUnpack ~label pattern
            | {ppat_desc = Ppat_unpack _; ppat_loc} ->
              (* remove unpack e.g. model: module(T) *)
              Pat.var ~loc:ppat_loc {txt = label; loc = ppat_loc}
            | _ -> pattern
          in
          let rec returnedExpression patternsWithLabel patternsWithNolabel
              ({pexp_desc} as expr) =
            match pexp_desc with
            | Pexp_newtype (_, expr) ->
              returnedExpression patternsWithLabel patternsWithNolabel expr
            | Pexp_constraint (expr, _) ->
              returnedExpression patternsWithLabel patternsWithNolabel expr
            | Pexp_fun
                ( _arg_label,
                  _default,
                  {
                    ppat_desc =
                      Ppat_construct ({txt = Lident "()"}, _) | Ppat_any;
                  },
                  expr ) ->
              (patternsWithLabel, patternsWithNolabel, expr)
            | Pexp_fun
                (arg_label, _default, ({ppat_loc; ppat_desc} as pattern), expr)
              -> (
              let patternWithoutConstraint =
                stripConstraintUnpack ~label:(getLabel arg_label) pattern
              in
              if isLabelled arg_label || isOptional arg_label then
                returnedExpression
                  (( {loc = ppat_loc; txt = Lident (getLabel arg_label)},
                     {
                       patternWithoutConstraint with
                       ppat_attributes =
                         (if isOptional arg_label then optionalAttrs else [])
                         @ pattern.ppat_attributes;
                     } )
                  :: patternsWithLabel)
                  patternsWithNolabel expr
              else
                (* Special case of nolabel arg "ref" in forwardRef fn *)
                (* let make = React.forwardRef(ref => body) *)
                match ppat_desc with
                | Ppat_var {txt}
                | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) ->
                  returnedExpression patternsWithLabel
                    (( {loc = ppat_loc; txt = Lident txt},
                       {
                         pattern with
                         ppat_attributes =
                           optionalAttrs @ pattern.ppat_attributes;
                       } )
                    :: patternsWithNolabel)
                    expr
                | _ ->
                  returnedExpression patternsWithLabel patternsWithNolabel expr)
            | _ -> (patternsWithLabel, patternsWithNolabel, expr)
          in
          let patternsWithLabel, patternsWithNolabel, expression =
            returnedExpression [] [] expression
          in
          (* add pattern matching for optional prop value *)
          let expression =
            if List.length vbMatchList = 0 then expression
            else Exp.let_ Nonrecursive vbMatchList expression
          in
          (* (ref) => expr *)
          let expression =
            List.fold_left
              (fun expr (_, pattern) -> Exp.fun_ Nolabel None pattern expr)
              expression patternsWithNolabel
          in
          let recordPattern =
            match patternsWithLabel with
            | [] -> Pat.any ()
            | _ -> Pat.record (List.rev patternsWithLabel) Open
          in
          let expression =
            Exp.fun_ Nolabel None
              (Pat.constraint_ recordPattern
                 (Typ.constr ~loc:emptyLoc
                    {txt = Lident "props"; loc = emptyLoc}
                    (match coreTypeOfAttr with
                    | None ->
                      makePropsTypeParams ~stripExplicitOption:true
                        ~stripExplicitJsNullableOfRef:hasForwardRef
                        namedTypeList
                    | Some _ -> (
                      match typVarsOfCoreType with
                      | [] -> []
                      | _ -> [Typ.any ()]))))
              expression
          in
          (* let make = ({id, name, ...}: props<'id, 'name, ...>) => { ... } *)
          let bindings, newBinding =
            match recFlag with
            | Recursive ->
              ( [
                  bindingWrapper
                    (Exp.let_ ~loc:emptyLoc Recursive
                       [
                         makeNewBinding binding expression internalFnName;
                         Vb.mk
                           (Pat.var {loc = emptyLoc; txt = fnName})
                           fullExpression;
                       ]
                       (Exp.ident {loc = emptyLoc; txt = Lident fnName}));
                ],
                None )
            | Nonrecursive ->
              ( [
                  {
                    binding with
                    pvb_expr = expression;
                    pvb_pat = Pat.var {txt = fnName; loc = Location.none};
                  };
                ],
                Some (bindingWrapper fullExpression) )
          in
          (Some propsRecordType, bindings, newBinding))
      else (None, [binding], None)
    in
    (* END of mapBinding fn *)
    let structuresAndBinding = List.map mapBinding valueBindings in
    let otherStructures (type_, binding, newBinding)
        (types, bindings, newBindings) =
      let types =
        match type_ with
        | Some type_ -> type_ :: types
        | None -> types
      in
      let newBindings =
        match newBinding with
        | Some newBinding -> newBinding :: newBindings
        | None -> newBindings
      in
      (types, binding @ bindings, newBindings)
    in
    let types, bindings, newBindings =
      List.fold_right otherStructures structuresAndBinding ([], [], [])
    in
    types
    @ [{pstr_loc; pstr_desc = Pstr_value (recFlag, bindings)}]
    @
    match newBindings with
    | [] -> []
    | newBindings ->
      [{pstr_loc = emptyLoc; pstr_desc = Pstr_value (recFlag, newBindings)}])
  | _ -> [item]

let transformSignatureItem ~config _mapper item =
  match item with
  | {
      psig_loc;
      psig_desc = Psig_value ({pval_attributes; pval_type} as psig_desc);
    } as psig -> (
    match List.filter React_jsx_common.hasAttr pval_attributes with
    | [] -> [item]
    | [_] ->
      (* If there is another @react.component, throw error *)
      if config.React_jsx_common.hasReactComponent then
        React_jsx_common.raiseErrorMultipleReactComponent ~loc:psig_loc
      else config.hasReactComponent <- true;
      check_string_int_attribute_iter.signature_item
        check_string_int_attribute_iter item;
      let hasForwardRef = ref false in
      let coreTypeOfAttr = React_jsx_common.coreTypeOfAttrs pval_attributes in
      let typVarsOfCoreType =
        coreTypeOfAttr
        |> Option.map React_jsx_common.typVarsOfCoreType
        |> Option.value ~default:[]
      in
      let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
        match ptyp_desc with
        | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest))
          when isOptional name || isLabelled name ->
          getPropTypes ((name, ptyp_loc, type_) :: types) rest
        | Ptyp_arrow
            (Nolabel, {ptyp_desc = Ptyp_constr ({txt = Lident "unit"}, _)}, rest)
          ->
          getPropTypes types rest
        | Ptyp_arrow (Nolabel, _type, rest) ->
          hasForwardRef := true;
          getPropTypes types rest
        | Ptyp_arrow (name, type_, returnValue)
          when isOptional name || isLabelled name ->
          (returnValue, (name, returnValue.ptyp_loc, type_) :: types)
        | _ -> (fullType, types)
      in
      let innerType, propTypes = getPropTypes [] pval_type in
      let namedTypeList = List.fold_left argToConcreteType [] propTypes in
      let retPropsType =
        Typ.constr
          (Location.mkloc (Lident "props") psig_loc)
          (match coreTypeOfAttr with
          | None -> makePropsTypeParams namedTypeList
          | Some _ -> (
            match typVarsOfCoreType with
            | [] -> []
            | _ -> [Typ.any ()]))
      in
      let propsRecordType =
        makePropsRecordTypeSig ~coreTypeOfAttr ~typVarsOfCoreType "props"
          psig_loc
          ((* If there is Nolabel arg, regard the type as ref in forwardRef *)
           (if !hasForwardRef then [(true, "ref", [], refType Location.none)]
           else [])
          @ namedTypeList)
      in
      (* can't be an arrow because it will defensively uncurry *)
      let newExternalType =
        Ptyp_constr
          ( {loc = psig_loc; txt = Ldot (Lident "React", "componentLike")},
            [retPropsType; innerType] )
      in
      let newStructure =
        {
          psig with
          psig_desc =
            Psig_value
              {
                psig_desc with
                pval_type = {pval_type with ptyp_desc = newExternalType};
                pval_attributes = List.filter otherAttrsPure pval_attributes;
              };
        }
      in
      [propsRecordType; newStructure]
    | _ ->
      React_jsx_common.raiseError ~loc:psig_loc
        "Only one react.component call can exist on a component at one time")
  | _ -> [item]

let transformJsxCall ~config mapper callExpression callArguments jsxExprLoc
    attrs =
  match callExpression.pexp_desc with
  | Pexp_ident caller -> (
    match caller with
    | {txt = Lident "createElement"; loc} ->
      React_jsx_common.raiseError ~loc
        "JSX: `createElement` should be preceeded by a module name."
    (* Foo.createElement(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
    | {loc; txt = Ldot (modulePath, ("createElement" | "make"))} ->
      transformUppercaseCall3 ~config modulePath mapper jsxExprLoc loc attrs
        callArguments
    (* div(~prop1=foo, ~prop2=bar, ~children=[bla], ()) *)
    (* turn that into
       ReactDOM.createElement(~props=ReactDOM.props(~props1=foo, ~props2=bar, ()), [|bla|]) *)
    | {loc; txt = Lident id} ->
      transformLowercaseCall3 ~config mapper jsxExprLoc loc attrs callArguments
        id
    | {txt = Ldot (_, anythingNotCreateElementOrMake); loc} ->
      React_jsx_common.raiseError ~loc
        "JSX: the JSX attribute should be attached to a \
         `YourModuleName.createElement` or `YourModuleName.make` call. We saw \
         `%s` instead"
        anythingNotCreateElementOrMake
    | {txt = Lapply _; loc} ->
      (* don't think there's ever a case where this is reached *)
      React_jsx_common.raiseError ~loc
        "JSX: encountered a weird case while processing the code. Please \
         report this!")
  | _ ->
    React_jsx_common.raiseError ~loc:callExpression.pexp_loc
      "JSX: `createElement` should be preceeded by a simple, direct module \
       name."

let expr ~config mapper expression =
  match expression with
  (* Does the function application have the @JSX attribute? *)
  | {
   pexp_desc = Pexp_apply (callExpression, callArguments);
   pexp_attributes;
   pexp_loc;
  } -> (
    let jsxAttribute, nonJSXAttributes =
      List.partition
        (fun (attribute, _) -> attribute.txt = "JSX")
        pexp_attributes
    in
    match (jsxAttribute, nonJSXAttributes) with
    (* no JSX attribute *)
    | [], _ -> default_mapper.expr mapper expression
    | _, nonJSXAttributes ->
      transformJsxCall ~config mapper callExpression callArguments pexp_loc
        nonJSXAttributes)
  (* is it a list with jsx attribute? Reason <>foo</> desugars to [@JSX][foo]*)
  | {
      pexp_desc =
        ( Pexp_construct
            ({txt = Lident "::"; loc}, Some {pexp_desc = Pexp_tuple _})
        | Pexp_construct ({txt = Lident "[]"; loc}, None) );
      pexp_attributes;
    } as listItems -> (
    let jsxAttribute, nonJSXAttributes =
      List.partition
        (fun (attribute, _) -> attribute.txt = "JSX")
        pexp_attributes
    in
    match (jsxAttribute, nonJSXAttributes) with
    (* no JSX attribute *)
    | [], _ -> default_mapper.expr mapper expression
    | _, nonJSXAttributes ->
      let loc = {loc with loc_ghost = true} in
      let fragment =
        match config.mode with
        | "automatic" ->
          Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsxFragment")}
        | "classic" | _ ->
          Exp.ident ~loc {loc; txt = Ldot (Lident "React", "fragment")}
      in
      let childrenExpr = transformChildrenIfList ~mapper listItems in
      let recordOfChildren children =
        Exp.record [(Location.mknoloc (Lident "children"), children)] None
      in
      let args =
        [
          (nolabel, fragment);
          (match config.mode with
          | "automatic" -> (
            ( nolabel,
              match childrenExpr with
              | {pexp_desc = Pexp_array children} -> (
                match children with
                | [] -> emptyRecord ~loc:Location.none
                | [child] -> recordOfChildren child
                | _ -> recordOfChildren childrenExpr)
              | _ -> recordOfChildren childrenExpr ))
          | "classic" | _ -> (nolabel, childrenExpr));
        ]
      in
      let countOfChildren = function
        | {pexp_desc = Pexp_array children} -> List.length children
        | _ -> 0
      in
      Exp.apply
        ~loc (* throw away the [@JSX] attribute and keep the others, if any *)
        ~attrs:nonJSXAttributes
        (* ReactDOM.createElement *)
        (match config.mode with
        | "automatic" ->
          if countOfChildren childrenExpr > 1 then
            Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsxs")}
          else Exp.ident ~loc {loc; txt = Ldot (Lident "React", "jsx")}
        | "classic" | _ ->
          Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOM", "createElement")})
        args)
  (* Delegate to the default mapper, a deep identity traversal *)
  | e -> default_mapper.expr mapper e

let module_binding ~(config : React_jsx_common.jsxConfig) mapper module_binding
    =
  config.nestedModules <- module_binding.pmb_name.txt :: config.nestedModules;
  let mapped = default_mapper.module_binding mapper module_binding in
  let () =
    match config.nestedModules with
    | _ :: rest -> config.nestedModules <- rest
    | [] -> ()
  in
  mapped

(* TODO: some line number might still be wrong *)
let jsxMapper ~config =
  let expr = expr ~config in
  let module_binding = module_binding ~config in
  let transformStructureItem = transformStructureItem ~config in
  let transformSignatureItem = transformSignatureItem ~config in
  (expr, module_binding, transformSignatureItem, transformStructureItem)
