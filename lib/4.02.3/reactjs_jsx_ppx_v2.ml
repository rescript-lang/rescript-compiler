(*
  This is the file that handles turning Reason JSX' agnostic function call into
  a ReasonReact-specific function call. Aka, this is a macro, using OCaml's ppx
  facilities; https://whitequark.org/blog/2014/04/16/a-guide-to-extension-
  points-in-ocaml/

  You wouldn't use this file directly; it's used by BuckleScript's
  bsconfig.json. Specifically, there's a field called `react-jsx` inside the
  field `reason`, which enables this ppx through some internal call in bsb
*)

(*
  There are two different transforms that can be selected in this file (v2 and v3):

  v2:
  transform `[@JSX] div(~props1=a, ~props2=b, ~children=[foo, bar], ())` into
  `ReactDOMRe.createElement("div", ~props={"props1": 1, "props2": b}, [|foo,
  bar|])`.

  transform `[@JSX] div(~props1=a, ~props2=b, ~children=foo, ())` into
  `ReactDOMRe.createElementVariadic("div", ~props={"props1": 1, "props2": b}, foo)`.

  transform the upper-cased case
  `[@JSX] Foo.createElement(~key=a, ~ref=b, ~foo=bar, ~children=[], ())` into
  `ReasonReact.element(~key=a, ~ref=b, Foo.make(~foo=bar, [||]))`

  transform `[@JSX] [foo]` into
  `ReactDOMRe.createElement(ReasonReact.fragment, [|foo|])`

  v3:
  transform `[@JSX] div(~props1=a, ~props2=b, ~children=[foo, bar], ())` into
  `ReactDOMRe.createDOMElementVariadic("div", ReactDOMRe.domProps(~props1=1, ~props2=b), [|foo, bar|])`.

  transform the upper-cased case
  `[@JSX] Foo.createElement(~key=a, ~ref=b, ~foo=bar, ~children=[], ())` into
  `React.createElement(Foo.make, Foo.makeProps(~key=a, ~ref=b, ~foo=bar, ()))`

  transform the upper-cased case
  `[@JSX] Foo.createElement(~foo=bar, ~children=[foo, bar], ())` into
  `React.createElementVariadic(Foo.make, Foo.makeProps(~foo=bar, ~children=React.null, ()), [|foo, bar|])`

  transform `[@JSX] [foo]` into
  `ReactDOMRe.createElement(ReasonReact.fragment, [|foo|])`
*)

open Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

let rec find_opt p = function
  | [] -> None
  | x :: l -> if p x then Some x else find_opt p l
let nolabel = ""
let labelled str = str
let optional str = "?" ^ str
let isOptional str = str <> "" && str.[0] = '?'
let isLabelled str = str <> "" && not (isOptional str)
let getLabel str = if (isOptional str) then (String.sub str 1 ((String.length str) - 1)) else str
let optionIdent = Ldot (Lident "*predef*","option")

let argIsKeyRef = function
  | (("key" | "ref"), _) | (("?key" | "?ref"), _) -> true
  | _ -> false
let constantString ~loc str = Ast_helper.Exp.constant ~loc (Asttypes.Const_string (str, None))
let safeTypeFromValue valueStr =
let valueStr = getLabel valueStr in
match String.sub valueStr 0 1 with
| "_" -> "T" ^ valueStr
| _ -> valueStr

type 'a children = | ListLiteral of 'a | Exact of 'a
type componentConfig = {
  propsName: string;
}

(* if children is a list, convert it to an array while mapping each element. If not, just map over it, as usual *)
let transformChildrenIfListUpper ~loc ~mapper theList =
  let rec transformChildren_ theList accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match theList with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} -> begin
      match accum with
      | [singleElement] -> Exact singleElement
      | accum -> ListLiteral (List.rev accum |> Exp.array ~loc)
      end
    | {pexp_desc = Pexp_construct (
        {txt = Lident "::"},
        Some {pexp_desc = Pexp_tuple (v::acc::[])}
      )} ->
      transformChildren_ acc ((mapper.expr mapper v)::accum)
    | notAList -> Exact (mapper.expr mapper notAList)
  in
  transformChildren_ theList []

let transformChildrenIfList ~loc ~mapper theList =
  let rec transformChildren_ theList accum =
    (* not in the sense of converting a list to an array; convert the AST
       reprensentation of a list to the AST reprensentation of an array *)
    match theList with
    | {pexp_desc = Pexp_construct ({txt = Lident "[]"}, None)} ->
      List.rev accum |> Exp.array ~loc
    | {pexp_desc = Pexp_construct (
        {txt = Lident "::"},
        Some {pexp_desc = Pexp_tuple (v::acc::[])}
      )} ->
      transformChildren_ acc ((mapper.expr mapper v)::accum)
    | notAList -> mapper.expr mapper notAList
  in
  transformChildren_ theList []

let extractChildren ?(removeLastPositionUnit=false) ~loc propsAndChildren =
  let rec allButLast_ lst acc = match lst with
    | [] -> []
    | ("", {pexp_desc = Pexp_construct ({txt = Lident "()"}, None)})::[] -> acc
    | ("", _)::rest -> raise (Invalid_argument "JSX: found non-labelled argument before the last position")
    | arg::rest -> allButLast_ rest (arg::acc)
  in
  let allButLast lst = allButLast_ lst [] |> List.rev in
  match (List.partition (fun (label, _) -> label = labelled "children") propsAndChildren) with
  | ([], props) ->
    (* no children provided? Place a placeholder list *)
    (Exp.construct ~loc {loc; txt = Lident "[]"} None, if removeLastPositionUnit then allButLast props else props)
  | ([(_, childrenExpr)], props) ->
    (childrenExpr, if removeLastPositionUnit then allButLast props else props)
  | _ -> raise (Invalid_argument "JSX: somehow there's more than one `children` label")

(* Helper method to look up the [@react.component] attribute *)
let hasAttr (loc, _) =
  loc.txt = "react.component"

(* Helper method to filter out any attribute that isn't [@react.component] *)
let otherAttrsPure (loc, _) =
  loc.txt <> "react.component"

(* Iterate over the attributes and try to find the [@react.component] attribute *)
let hasAttrOnBinding {pvb_attributes} = find_opt hasAttr pvb_attributes <> None

(* Filter the [@react.component] attribute and immutably replace them on the binding *)
let filterAttrOnBinding binding = {binding with pvb_attributes = List.filter otherAttrsPure binding.pvb_attributes}

(* Finds the name of the variable the binding is assigned to, otherwise raises Invalid_argument *)
let getFnName binding =
  match binding with
  | {pvb_pat = {
      ppat_desc = Ppat_var {txt}
    }} -> txt
  | _ -> raise (Invalid_argument "react.component calls cannot be destructured.")

(* Lookup the value of `props` otherwise raise Invalid_argument error *)
let getPropsNameValue acc (loc, exp) =
    match (loc, exp) with
    | ({ txt = Lident "props" }, { pexp_desc = Pexp_ident {txt = Lident str} }) -> { propsName = str }
    | ({ txt }, _) -> raise (Invalid_argument ("react.component only accepts props as an option, given: " ^ Longident.last txt))

(* Lookup the `props` record or string as part of [@react.component] and store the name for use when rewriting *)
let getPropsAttr payload =
  let defaultProps = {propsName = "Props"} in
  match payload with
  | Some(PStr(
    {pstr_desc = Pstr_eval ({
      pexp_desc = Pexp_record (recordFields, None)
      }, _)}::_rest
      )) ->
      List.fold_left getPropsNameValue defaultProps recordFields
  | Some(PStr({pstr_desc = Pstr_eval ({pexp_desc = Pexp_ident {txt = Lident "props"}}, _)}::_rest)) -> {propsName = "props"}
  | Some(PStr({pstr_desc = Pstr_eval (_, _)}::_rest)) -> raise (Invalid_argument ("react.component accepts a record config with props as an options."))
  | _ -> defaultProps

(* Plucks the label, loc, and type_ from an AST node *)
let pluckLabelLocType (label, _, _, _, loc, type_) = (label, loc, type_)

(* Lookup the filename from the location information on the AST node and turn it into a valid module identifier *)
let filenameFromLoc (pstr_loc: Location.t) =
  let fileName = match pstr_loc.loc_start.pos_fname with
  | "" -> !Location.input_name
  | fileName -> fileName
  in
  let fileName = try
      Filename.chop_extension (Filename.basename fileName)
    with | Invalid_argument _-> fileName in
  let fileName = String.capitalize fileName in
  fileName

(* Build a string representation of a module name with segments separated by $ *)
let makeModuleName fileName nestedModules fnName =
  let fullModuleName = match (fileName, nestedModules, fnName) with
  (* TODO: is this even reachable? It seems like the fileName always exists *)
  | ("", nestedModules, "make") -> nestedModules
  | ("", nestedModules, fnName) -> List.rev (fnName :: nestedModules)
  | (fileName, nestedModules, "make") -> fileName :: (List.rev nestedModules)
  | (fileName, nestedModules, fnName) -> fileName :: (List.rev (fnName :: nestedModules))
  in
  let fullModuleName = String.concat "$" fullModuleName in
  fullModuleName

(*
  AST node builders

  These functions help us build AST nodes that are needed when transforming a [@react.component] into a
  constructor and a props external
*)

(* Build an AST node representing all named args for the `external` definition for a component's props *)
let rec recursivelyMakeNamedArgsForExternal list args =
  match list with
  | (label, loc, type_)::tl ->
    recursivelyMakeNamedArgsForExternal tl (Typ.arrow
    ~loc
    label
    (match (label, type_) with
    | (label, None) when isOptional label -> {
        ptyp_loc = loc;
        ptyp_attributes = [];
        ptyp_desc = Ptyp_constr ({loc; txt=optionIdent}, [{
          ptyp_desc = Ptyp_var (safeTypeFromValue label);
          ptyp_loc = loc;
          ptyp_attributes = [];
        }]);
      }
    | (label, None) when isLabelled label -> {
      ptyp_desc = Ptyp_var (safeTypeFromValue label);
      ptyp_loc = loc;
      ptyp_attributes = [];
    }
    | (label, Some ({ptyp_desc = Ptyp_constr ({txt=optionIdent}, _)} as type_)) when isOptional label ->
      type_
    | (label, Some ({ptyp_desc = Ptyp_constr ({txt=(Lident "option")}, [type_])})) when isOptional label -> {
      type_ with
      ptyp_desc = Ptyp_constr ({loc=type_.ptyp_loc; txt=optionIdent}, [type_]);
    }
    | (label, Some (type_)) when isOptional label -> {
      type_ with
      ptyp_desc = Ptyp_constr ({loc=type_.ptyp_loc; txt=optionIdent}, [type_]);
    }
    | (_, Some type_) -> type_
    | (_, None) -> raise (Invalid_argument "This should never happen..")
    )
    args)
  | [] -> args

(* Build an AST node for the [@bs.obj] representing props for a component *)
let makePropsValue fnName loc namedArgListWithKeyAndRef propsType =
  let propsName = fnName ^ "Props" in {
  pval_name = {txt = propsName; loc};
  pval_type =
      recursivelyMakeNamedArgsForExternal
        namedArgListWithKeyAndRef
        (Typ.arrow
          nolabel
          {
            ptyp_desc = Ptyp_constr ({txt= Lident("unit"); loc}, []);
            ptyp_loc = loc;
            ptyp_attributes = [];
          }
          propsType
        );
  pval_prim = [""];
  pval_attributes = [({txt = "bs.obj"; loc = loc}, PStr [])];
  pval_loc = loc;
}

(* Build an AST node representing an `external` with the definition of the [@bs.obj] *)
let makePropsExternal fnName loc namedArgListWithKeyAndRef propsType =
  {
    pstr_loc = loc;
    pstr_desc = Pstr_primitive (makePropsValue fnName loc namedArgListWithKeyAndRef propsType)
  }

(* Build an AST node for the signature of the `external` definition *)
let makePropsExternalSig fnName loc namedArgListWithKeyAndRef propsType =
  {
    psig_loc = loc;
    psig_desc = Psig_value (makePropsValue fnName loc namedArgListWithKeyAndRef propsType)
  }

(* Build an AST node for the props name when converted to a Js.t inside the function signature  *)
let makePropsName ~loc name =
  {
    ppat_desc = Ppat_var {txt = name; loc};
    ppat_loc = loc;
    ppat_attributes = [];
  }

(* Build an AST node representing a "closed" Js.t object representing a component's props *)
let makePropsType ~loc namedTypeList =
  Typ.mk ~loc (
    Ptyp_constr({txt= Ldot (Lident("Js"), "t"); loc}, [{
        ptyp_desc = Ptyp_object(namedTypeList, Closed);
        ptyp_loc = loc;
        ptyp_attributes = [];
      }])
    )

(* Builds an AST node for the entire `external` definition of props *)
let makeExternalDecl fnName loc namedArgListWithKeyAndRef namedTypeList =
  makePropsExternal
    fnName
    loc
    (List.map pluckLabelLocType namedArgListWithKeyAndRef)
    (makePropsType ~loc namedTypeList)

(* TODO: some line number might still be wrong *)
let jsxMapper () =

  let jsxVersion = ref None in

  let transformUppercaseCall3 modulePath mapper loc attrs _ callArguments =
    let (children, argsWithLabels) = extractChildren ~loc ~removeLastPositionUnit:true callArguments in
    let argsForMake = argsWithLabels in
    let childrenExpr = transformChildrenIfListUpper ~loc ~mapper children in
    let recursivelyTransformedArgsForMake = argsForMake |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)) in
    let childrenArg = ref None in
    let args = recursivelyTransformedArgsForMake
      @ (match childrenExpr with
        | Exact children -> [(labelled "children", children)]
        | ListLiteral ({ pexp_desc = Pexp_array list }) when list = [] -> []
        | ListLiteral expression ->
        (* this is a hack to support react components that introspect into their children *)
        (childrenArg := Some expression;
        [(labelled "children", Exp.ident ~loc {loc; txt = Ldot (Lident "React", "null")})]))
      @ [(nolabel, Exp.construct ~loc {loc; txt = Lident "()"} None)] in
    let isCap str = let first = String.sub str 0 1 in let capped = String.uppercase first in first = capped in
    let ident = match modulePath with
    | Lident _ -> Ldot (modulePath, "make")
    | (Ldot (modulePath, value) as fullPath) when isCap value -> Ldot (fullPath, "make")
    | modulePath -> modulePath in
    let propsIdent = match ident with
    | Lident path -> Lident (path ^ "Props")
    | Ldot(ident, path) -> Ldot (ident, path ^ "Props")
    | _ -> raise (Invalid_argument "JSX name can't be the result of function applications") in
    let props =
    Exp.apply ~attrs ~loc (Exp.ident ~loc {loc; txt = propsIdent}) args in
    (* handle key, ref, children *)
      (* React.createElement(Component.make, props, ...children) *)
    match (!childrenArg) with
    | None ->
      (Exp.apply
        ~loc
        ~attrs
        (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "createElement")})
        ([
          (nolabel, Exp.ident ~loc {txt = ident; loc});
          (nolabel, props)
        ]))
     | Some children ->
       (Exp.apply
         ~loc
         ~attrs
         (Exp.ident ~loc {loc; txt = Ldot (Lident "React", "createElementVariadic")})
         ([
           (nolabel, Exp.ident ~loc {txt = ident; loc});
           (nolabel, props);
           (nolabel, children)
         ]))
     in

    let transformLowercaseCall3 mapper loc attrs callArguments id =
      let (children, nonChildrenProps) = extractChildren ~loc callArguments in
      let componentNameExpr = constantString ~loc id in
      let childrenExpr = transformChildrenIfList ~loc ~mapper children in
      let createElementCall = match children with
        (* [@JSX] div(~children=[a]), coming from <div> a </div> *)
        | {
            pexp_desc =
             Pexp_construct ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple _ })
             | Pexp_construct ({txt = Lident "[]"}, None)
          } -> "createDOMElementVariadic"
        (* [@JSX] div(~children= value), coming from <div> ...(value) </div> *)
        | _ -> raise (Invalid_argument "A spread as a DOM element's \
          children don't make sense written together. You can simply remove the spread.")
      in
      let args = match nonChildrenProps with
        | [_justTheUnitArgumentAtEnd] ->
          [
            (* "div" *)
            (nolabel, componentNameExpr);
            (* [|moreCreateElementCallsHere|] *)
            (nolabel, childrenExpr)
          ]
        | nonEmptyProps ->
          let propsCall =
            Exp.apply
              ~loc
              (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "domProps")})
              (nonEmptyProps |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)))
          in
          [
            (* "div" *)
            (nolabel, componentNameExpr);
            (* ReactDOMRe.props(~className=blabla, ~foo=bar, ()) *)
            (labelled "props", propsCall);
            (* [|moreCreateElementCallsHere|] *)
            (nolabel, childrenExpr)
          ] in
      Exp.apply
        ~loc
        (* throw away the [@JSX] attribute and keep the others, if any *)
        ~attrs
        (* ReactDOMRe.createElement *)
        (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", createElementCall)})
        args
    in

  let transformUppercaseCall modulePath mapper loc attrs _ callArguments =
    let (children, argsWithLabels) = extractChildren ~loc ~removeLastPositionUnit:true callArguments in
    let (argsKeyRef, argsForMake) = List.partition argIsKeyRef argsWithLabels in
    let childrenExpr = transformChildrenIfList ~loc ~mapper children in
    let recursivelyTransformedArgsForMake = argsForMake |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)) in
    let args = recursivelyTransformedArgsForMake @ [ (nolabel, childrenExpr) ] in
    let wrapWithReasonReactElement e = (* ReasonReact.element(~key, ~ref, ...) *)
      Exp.apply
        ~loc
        (Exp.ident ~loc {loc; txt = Ldot (Lident "ReasonReact", "element")})
        (argsKeyRef @ [(nolabel, e)]) in
    Exp.apply
      ~loc
      ~attrs
      (* Foo.make *)
      (Exp.ident ~loc {loc; txt = Ldot (modulePath, "make")})
      args
    |> wrapWithReasonReactElement in

  let transformLowercaseCall mapper loc attrs callArguments id =
    let (children, nonChildrenProps) = extractChildren ~loc callArguments in
    let componentNameExpr = constantString ~loc id in
    let childrenExpr = transformChildrenIfList ~loc ~mapper children in
    let createElementCall = match children with
      (* [@JSX] div(~children=[a]), coming from <div> a </div> *)
      | {
          pexp_desc =
           Pexp_construct ({txt = Lident "::"}, Some {pexp_desc = Pexp_tuple _ })
           | Pexp_construct ({txt = Lident "[]"}, None)
        } -> "createElement"
      (* [@JSX] div(~children=[|a|]), coming from <div> ...[|a|] </div> *)
      | { pexp_desc = (Pexp_array _) } ->
        raise (Invalid_argument "A spread + an array literal as a DOM element's \
          children would cancel each other out, and thus don't make sense written \
          together. You can simply remove the spread and the array literal.")
      (* [@JSX] div(~children= <div />), coming from <div> ...<div/> </div> *)
      | {
          pexp_attributes
        } when pexp_attributes |> List.exists (fun (attribute, _) -> attribute.txt = "JSX") ->
        raise (Invalid_argument "A spread + a JSX literal as a DOM element's \
          children don't make sense written together. You can simply remove the spread.")
      | _ -> "createElementVariadic"
    in
    let args = match nonChildrenProps with
      | [_justTheUnitArgumentAtEnd] ->
        [
          (* "div" *)
          (nolabel, componentNameExpr);
          (* [|moreCreateElementCallsHere|] *)
          (nolabel, childrenExpr)
        ]
      | nonEmptyProps ->
        let propsCall =
          Exp.apply
            ~loc
            (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "props")})
            (nonEmptyProps |> List.map (fun (label, expression) -> (label, mapper.expr mapper expression)))
        in
        [
          (* "div" *)
          (nolabel, componentNameExpr);
          (* ReactDOMRe.props(~className=blabla, ~foo=bar, ()) *)
          (labelled "props", propsCall);
          (* [|moreCreateElementCallsHere|] *)
          (nolabel, childrenExpr)
        ] in
    Exp.apply
      ~loc
      (* throw away the [@JSX] attribute and keep the others, if any *)
      ~attrs
      (* ReactDOMRe.createElement *)
      (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", createElementCall)})
      args
  in

  let rec recursivelyTransformNamedArgsForMake mapper expr list =
    let expr = mapper.expr mapper expr in
    match expr.pexp_desc with
    (* TODO: make this show up with a loc. *)
    | Pexp_fun ("key", _, _, _)
    | Pexp_fun ("?key", _, _, _) -> raise (Invalid_argument "Key cannot be accessed inside of a component. Don't worry - you can always key a component from its parent!")
    | Pexp_fun ("ref", _, _, _)
    | Pexp_fun ("?ref", _, _, _) -> raise (Invalid_argument "Ref cannot be passed as a normal prop. Please use `forwardRef` API instead.")
    | Pexp_fun (arg, default, pattern, expression) when isOptional arg || isLabelled arg ->
      let alias = (match pattern with
      | {ppat_desc = Ppat_alias (_, {txt}) | Ppat_var {txt}} -> txt
      | {ppat_desc = Ppat_any} -> "_"
      | _ -> getLabel arg) in
      let type_ = (match pattern with
      | {ppat_desc = Ppat_constraint (_, type_)} -> Some type_
      | _ -> None) in
      recursivelyTransformNamedArgsForMake mapper expression ((arg, default, None, alias, pattern.ppat_loc, type_) :: list)
    | Pexp_fun (nolabel, _, { ppat_desc = (Ppat_construct ({txt = Lident "()"}, _) | Ppat_any)}, expression) ->
        (expression.pexp_desc, list, None)
    | Pexp_fun (nolabel, _, { ppat_desc = Ppat_var ({txt})}, expression) ->
        (expression.pexp_desc, list, Some txt)
    | innerExpression -> (innerExpression, list, None)
  in


  let argToType types (name, _default, _noLabelName, _alias, loc, type_) = match (type_, name) with
    | (Some type_, name) when isLabelled name || isOptional name ->
      (getLabel name, [], type_) :: types
    | (None, name) when isOptional name ->
      (getLabel name, [], {
        ptyp_desc = Ptyp_constr ({loc; txt=optionIdent}, [{
          ptyp_desc = Ptyp_var (safeTypeFromValue name);
          ptyp_loc = loc;
          ptyp_attributes = [];
        }]);
        ptyp_loc = loc;
        ptyp_attributes = [];
        }) :: types
    | (None, name) when isLabelled name ->
      (getLabel name, [], {
        ptyp_desc = Ptyp_var (safeTypeFromValue name);
        ptyp_loc = loc;
        ptyp_attributes = [];
        }) :: types
    | _ -> types
  in

  let argToConcreteType types (name, loc, type_) = match name with
    | name when isLabelled name || isOptional name ->
    (getLabel name, [], type_) :: types
    (* return value *)
    | _ -> types
  in

  let nestedModules = ref([]) in
  let transformComponentDefinition mapper structure returnStructures = match structure with
  (* external *)
  | ({
      pstr_loc;
      pstr_desc = Pstr_primitive ({
        pval_name = { txt = fnName };
        pval_attributes;
        pval_type;
      } as pstr_desc)
    } as pstr) ->
    (match List.filter hasAttr pval_attributes with
    | [] -> structure :: returnStructures
    | [_] ->
    let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
      (match ptyp_desc with
      | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest)) when isLabelled name || isOptional name ->
        getPropTypes ((name, ptyp_loc, type_)::types) rest
      | Ptyp_arrow ("", _type, rest) ->
        getPropTypes types rest
      | Ptyp_arrow (name, type_, returnValue) when isLabelled name || isOptional name ->
        (returnValue, (name, returnValue.ptyp_loc, type_)::types)
      | _ -> (fullType, types))
    in
    let (innerType, propTypes) = getPropTypes [] pval_type in
    let namedTypeList = List.fold_left argToConcreteType [] propTypes in
    let pluckLabelAndLoc (label, loc, type_) = (label, loc, Some type_) in
    let retPropsType = makePropsType ~loc:pstr_loc namedTypeList in
    let externalPropsDecl = makePropsExternal fnName pstr_loc ((
      optional "key",
      pstr_loc,
      None
    ) :: List.map pluckLabelAndLoc propTypes) retPropsType in
    (* can't be an arrow because it will defensively uncurry *)
    let newExternalType = Ptyp_constr (
      {loc = pstr_loc; txt = Ldot ((Lident "React"), "componentLike")},
      [retPropsType; innerType]
    ) in
    let newStructure = {
      pstr with pstr_desc = Pstr_primitive {
        pstr_desc with pval_type = {
          pval_type with ptyp_desc = newExternalType;
        };
        pval_attributes = List.filter otherAttrsPure pval_attributes;
      }
    } in
    externalPropsDecl :: newStructure :: returnStructures
    | _ -> raise (Invalid_argument "Only one react.component call can exist on a component at one time"))
  (* let component = ... *)
  | {
      pstr_loc;
      pstr_desc = Pstr_value (
        recFlag,
        valueBindings
      )
    } ->
      let mapBinding binding = if (hasAttrOnBinding binding) then
        let fnName = getFnName binding in
        let modifiedBinding binding =
          let expression = binding.pvb_expr in
          let wrapExpressionWithBinding expressionFn expression = {(filterAttrOnBinding binding) with pvb_expr = expressionFn expression} in
          let rec spelunkForFunExpression expression = (match expression with
          (* let make = (~prop) => ... *)
          | {
            pexp_desc = Pexp_fun _
          } -> ((fun expressionDesc -> {expression with pexp_desc = expressionDesc}), expression)
          (* let make = {let foo = bar in (~prop) => ...} *)
          | {
              pexp_desc = Pexp_let (recursive, vbs, returnExpression)
            } ->
            (* here's where we spelunk! *)
            let (wrapExpression, realReturnExpression) = spelunkForFunExpression returnExpression in
            ((fun expressionDesc -> {expression with pexp_desc = Pexp_let (recursive, vbs, wrapExpression expressionDesc)}), realReturnExpression)
          (* let make = React.forwardRef((~prop) => ...) *)
          | {
              pexp_desc = Pexp_apply (wrapperExpression, [(nolabel, innerFunctionExpression)])
            } ->
            let (wrapExpression, realReturnExpression) = spelunkForFunExpression innerFunctionExpression in
            ((fun expressionDesc -> {
              expression with pexp_desc =
                Pexp_apply (wrapperExpression, [(nolabel, wrapExpression expressionDesc)])
              }),
              realReturnExpression
            )
          | _ -> raise (Invalid_argument "react.component calls can only be on function definitions or component wrappers (forwardRef, memo).")
          ) in
          let (wrapExpression, expression) = spelunkForFunExpression expression in
          (wrapExpressionWithBinding wrapExpression, expression)
        in
        let (bindingWrapper, expression) = modifiedBinding binding in
        let reactComponentAttribute = try
          Some(List.find hasAttr binding.pvb_attributes)
        with | Not_found -> None in
        let payload = match reactComponentAttribute with
        (* TODO: in some cases this is a better loc than pstr_loc *)
        | Some (_loc, payload) -> Some payload
        | None -> None in
        let props = getPropsAttr payload in
        (* do stuff here! *)
        let (innerFunctionExpression, namedArgList, forwardRef) = recursivelyTransformNamedArgsForMake mapper expression [] in
        let namedArgListWithKeyAndRef = (optional("key"), None, None, "key", pstr_loc, None) :: namedArgList in
        let namedArgListWithKeyAndRef = match forwardRef with
        | Some(_) ->  (optional("ref"), None, None, "ref", pstr_loc, None) :: namedArgListWithKeyAndRef
        | None -> namedArgListWithKeyAndRef
        in
        let namedTypeList = List.fold_left argToType [] namedArgList in
        let externalDecl = makeExternalDecl fnName pstr_loc namedArgListWithKeyAndRef namedTypeList in
        let makeLet innerExpression (label, default, _, alias, loc, _type) =
          let labelString = (match label with | label when isOptional label || isLabelled label -> getLabel label | _ -> raise (Invalid_argument "This should never happen")) in
          let expression = (Exp.apply ~loc
            (Exp.ident ~loc {txt = (Lident "##"); loc })
            [
              (nolabel, Exp.ident ~loc {txt = (Lident props.propsName); loc });
              (nolabel, Exp.ident ~loc {
                txt = (Lident labelString);
                loc
              })
            ]
          ) in
          let expression = match (label, default) with
          | (label, Some default) when isOptional label -> Exp.match_ expression [
            Exp.case
              (Pat.construct {loc; txt=Lident "Some"} (Some (Pat.var ~loc {txt = labelString; loc})))
              (Exp.ident ~loc {txt = (Lident labelString); loc});
            Exp.case
              (Pat.construct {loc; txt=Lident "None"} None)
              default
          ]
          | _ -> expression in
          let letExpression = Vb.mk
            (Pat.var ~loc {txt = alias; loc})
             expression in
          Exp.let_ ~loc Nonrecursive [letExpression] innerExpression in
        let innerExpression = List.fold_left makeLet (Exp.mk innerFunctionExpression) namedArgList in
        let innerExpressionWithRef = match (forwardRef) with
        | Some txt ->
          {innerExpression with pexp_desc = Pexp_fun (nolabel, None, {
            ppat_desc = Ppat_var { txt; loc = pstr_loc };
            ppat_loc = pstr_loc;
            ppat_attributes = [];
          }, innerExpression)}
        | None -> innerExpression
        in
        let fullExpression = (Pexp_fun (
          nolabel,
          None,
          {
            ppat_desc = Ppat_constraint (
              makePropsName ~loc:pstr_loc props.propsName,
              makePropsType ~loc:pstr_loc namedTypeList
            );
            ppat_loc = pstr_loc;
            ppat_attributes = [];
          },
          innerExpressionWithRef
        )) in
        let fileName = filenameFromLoc pstr_loc in
        let fullModuleName = makeModuleName fileName !nestedModules fnName in
        let fullExpression = match (fullModuleName) with
        | ("") -> fullExpression
        | (txt) -> Pexp_let (
            Nonrecursive,
            [Vb.mk
              ~loc:pstr_loc
              (Pat.var ~loc:pstr_loc {loc = pstr_loc; txt})
              (Exp.mk ~loc:pstr_loc fullExpression)
            ],
            (Exp.ident ~loc:pstr_loc {loc = pstr_loc; txt = Lident txt})
          )
        in
        let newBinding = bindingWrapper fullExpression in
        (Some externalDecl, newBinding)
      else
        (None, binding)
      in
      let structuresAndBinding = List.map mapBinding valueBindings in
      let otherStructures (extern, binding) (externs, bindings) =
        let externs = match extern with
        | Some extern -> extern :: externs
        | None -> externs in
        (externs, binding :: bindings)
      in
      let (externs, bindings) = List.fold_right otherStructures structuresAndBinding ([], []) in
      externs @ {
        pstr_loc;
        pstr_desc = Pstr_value (
          recFlag,
          bindings
        )
      } :: returnStructures
    | structure -> structure :: returnStructures in

  let reactComponentTransform mapper structures =
  List.fold_right (transformComponentDefinition mapper) structures [] in

  let transformComponentSignature mapper signature returnSignatures = match signature with
  | ({
      psig_loc;
      psig_desc = Psig_value ({
        pval_name = { txt = fnName };
        pval_attributes;
        pval_type;
      } as psig_desc)
    } as psig) ->
    (match List.filter hasAttr pval_attributes with
    | [] -> signature :: returnSignatures
    | [_] ->
    let rec getPropTypes types ({ptyp_loc; ptyp_desc} as fullType) =
      (match ptyp_desc with
      | Ptyp_arrow (name, type_, ({ptyp_desc = Ptyp_arrow _} as rest)) when isOptional name || isLabelled name ->
        getPropTypes ((name, ptyp_loc, type_)::types) rest
      | Ptyp_arrow ("", _type, rest) ->
        getPropTypes types rest
      | Ptyp_arrow (name, type_, returnValue) when isOptional name || isLabelled name ->
        (returnValue, (name, returnValue.ptyp_loc, type_)::types)
      | _ -> (fullType, types))
    in
    let (innerType, propTypes) = getPropTypes [] pval_type in
    let namedTypeList = List.fold_left argToConcreteType [] propTypes in
    let pluckLabelAndLoc (label, loc, type_) = (label, loc, Some type_) in
    let retPropsType = makePropsType ~loc:psig_loc namedTypeList in
    let externalPropsDecl = makePropsExternalSig fnName psig_loc ((
      optional "key",
      psig_loc,
      None
    ) :: List.map pluckLabelAndLoc propTypes) retPropsType in
        (* can't be an arrow because it will defensively uncurry *)
    let newExternalType = Ptyp_constr (
      {loc = psig_loc; txt = Ldot ((Lident "React"), "componentLike")},
      [retPropsType; innerType]
    ) in
    let newStructure = {
      psig with psig_desc = Psig_value {
        psig_desc with pval_type = {
          pval_type with ptyp_desc = newExternalType;
        };
        pval_attributes = List.filter otherAttrsPure pval_attributes;
      }
    } in
    externalPropsDecl :: newStructure :: returnSignatures
    | _ -> raise (Invalid_argument "Only one react.component call can exist on a component at one time"))
  | signature -> signature :: returnSignatures in

  let reactComponentSignatureTransform mapper signatures =
  List.fold_right (transformComponentSignature mapper) signatures [] in


  let transformJsxCall mapper callExpression callArguments attrs =
    (match callExpression.pexp_desc with
     | Pexp_ident caller ->
       (match caller with
        | {txt = Lident "createElement"} ->
          raise (Invalid_argument "JSX: `createElement` should be preceeded by a module name.")

        (* Foo.createElement(~prop1=foo, ~prop2=bar, ~children=[], ()) *)
        | {loc; txt = Ldot (modulePath, ("createElement" | "make"))} ->
          (match !jsxVersion with
          | None
          | Some 2 -> transformUppercaseCall modulePath mapper loc attrs callExpression callArguments
          | Some 3 -> transformUppercaseCall3 modulePath mapper loc attrs callExpression callArguments
          | Some _ -> raise (Invalid_argument "JSX: the JSX version must be 2 or 3"))

        (* div(~prop1=foo, ~prop2=bar, ~children=[bla], ()) *)
        (* turn that into
          ReactDOMRe.createElement(~props=ReactDOMRe.props(~props1=foo, ~props2=bar, ()), [|bla|]) *)
        | {loc; txt = Lident id} ->
          (match !jsxVersion with
          | None
          | Some 2 -> transformLowercaseCall mapper loc attrs callArguments id
          | Some 3 -> transformLowercaseCall3 mapper loc attrs callArguments id
          | Some _ -> raise (Invalid_argument "JSX: the JSX version must be 2 or 3"))

        | {txt = Ldot (_, anythingNotCreateElementOrMake)} ->
          raise (
            Invalid_argument
              ("JSX: the JSX attribute should be attached to a `YourModuleName.createElement` or `YourModuleName.make` call. We saw `"
               ^ anythingNotCreateElementOrMake
               ^ "` instead"
              )
          )

        | {txt = Lapply _} ->
          (* don't think there's ever a case where this is reached *)
          raise (
            Invalid_argument "JSX: encountered a weird case while processing the code. Please report this!"
          )
       )
     | _ ->
       raise (
         Invalid_argument "JSX: `createElement` should be preceeded by a simple, direct module name."
       )
    ) in

  let signature =
    (fun mapper signature -> default_mapper.signature mapper @@ reactComponentSignatureTransform mapper signature) in

  let structure =
    (fun mapper structure -> match structure with
      (*
        match against [@bs.config {foo, jsx: ...}] at the file-level. This
        indicates which version of JSX we're using. This code stays here because
        we used to have 2 versions of JSX PPX (and likely will again in the
        future when JSX PPX changes). So the architecture for switching between
        JSX behavior stayed here. To create a new JSX ppx, copy paste this
        entire file and change the relevant parts.

        Description of architecture: in bucklescript's bsconfig.json, you can
        specify a project-wide JSX version. You can also specify a file-level
        JSX version. This degree of freedom allows a person to convert a project
        one file at time onto the new JSX, when it was released. It also enabled
        a project to depend on a third-party which is still using an old version
        of JSX
      *)
      | {
          pstr_loc;
          pstr_desc = Pstr_attribute (
            ({txt = "bs.config"} as bsConfigLabel),
            PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_record (recordFields, b)} as innerConfigRecord, a)} as configRecord]
          )
        }::restOfStructure -> begin
          let (jsxField, recordFieldsWithoutJsx) = recordFields |> List.partition (fun ({txt}, _) -> txt = Lident "jsx") in
          match (jsxField, recordFieldsWithoutJsx) with
          (* no file-level jsx config found *)
          | ([], _) -> default_mapper.structure mapper structure
          (* {jsx: 2} *)
          | ((_, {pexp_desc = Pexp_constant (Const_int version)})::rest, recordFieldsWithoutJsx) -> begin
              (match version with
              | 2 -> jsxVersion := Some 2
              | 3 -> jsxVersion := Some 3
              | _ -> raise (Invalid_argument "JSX: the file-level bs.config's jsx version must be 2 or 3"));
              match recordFieldsWithoutJsx with
              (* record empty now, remove the whole bs.config attribute *)
              | [] -> default_mapper.structure mapper @@ reactComponentTransform mapper restOfStructure
              | fields -> default_mapper.structure mapper ({
                pstr_loc;
                pstr_desc = Pstr_attribute (
                  bsConfigLabel,
                  PStr [{configRecord with pstr_desc = Pstr_eval ({innerConfigRecord with pexp_desc = Pexp_record (fields, b)}, a)}]
                )
              }::(reactComponentTransform mapper restOfStructure))
            end
        | _ -> raise (Invalid_argument "JSX: the file-level bs.config's {jsx: ...} config accepts only a version number")
      end
      | structures -> begin
        default_mapper.structure mapper @@ reactComponentTransform mapper structures
      end
    ) in

  let expr =
    (fun mapper expression -> match expression with
       (* Does the function application have the @JSX attribute? *)
       | {
           pexp_desc = Pexp_apply (callExpression, callArguments);
           pexp_attributes
         } ->
         let (jsxAttribute, nonJSXAttributes) = List.partition (fun (attribute, _) -> attribute.txt = "JSX") pexp_attributes in
         (match (jsxAttribute, nonJSXAttributes) with
         (* no JSX attribute *)
         | ([], _) -> default_mapper.expr mapper expression
         | (_, nonJSXAttributes) -> transformJsxCall mapper callExpression callArguments nonJSXAttributes)

       (* is it a list with jsx attribute? Reason <>foo</> desugars to [@JSX][foo]*)
       | {
           pexp_desc =
            Pexp_construct ({txt = Lident "::"; loc}, Some {pexp_desc = Pexp_tuple _})
            | Pexp_construct ({txt = Lident "[]"; loc}, None);
           pexp_attributes
         } as listItems ->
          let (jsxAttribute, nonJSXAttributes) = List.partition (fun (attribute, _) -> attribute.txt = "JSX") pexp_attributes in
          (match (jsxAttribute, nonJSXAttributes) with
          (* no JSX attribute *)
          | ([], _) -> default_mapper.expr mapper expression
          | (_, nonJSXAttributes) ->
            let fragment = Exp.ident ~loc {loc; txt = Ldot (Lident "ReasonReact", "fragment")} in
            let childrenExpr = transformChildrenIfList ~loc ~mapper listItems in
            let args = [
              (* "div" *)
              (nolabel, fragment);
              (* [|moreCreateElementCallsHere|] *)
              (nolabel, childrenExpr)
            ] in
            Exp.apply
              ~loc
              (* throw away the [@JSX] attribute and keep the others, if any *)
              ~attrs:nonJSXAttributes
              (* ReactDOMRe.createElement *)
              (Exp.ident ~loc {loc; txt = Ldot (Lident "ReactDOMRe", "createElement")})
              args
         )
       (* Delegate to the default mapper, a deep identity traversal *)
       | e -> default_mapper.expr mapper e) in

  let module_binding =
    (fun mapper module_binding ->
      let _ = nestedModules := module_binding.pmb_name.txt :: !nestedModules in
      let mapped = default_mapper.module_binding mapper module_binding in
      let _ = nestedModules := List.tl !nestedModules in
      mapped
    ) in

  { default_mapper with structure; expr; signature; module_binding; }


(* #if BS_COMPILER_IN_BROWSER then

module Js = struct
  module Unsafe = struct
    type any
    external inject : 'a -> any = "%identity"
    external get : 'a -> 'b -> 'c = "caml_js_get"
    external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
    external pure_js_expr : string -> 'a = "caml_pure_js_expr"
    let global = pure_js_expr "joo_global_object"
    external obj : (string * any) array -> 'a = "caml_js_object"
  end
  type (-'a, +'b) meth_callback
  type 'a callback = (unit, 'a) meth_callback
  external wrap_meth_callback : ('a -> 'b) -> ('a, 'b) meth_callback = "caml_js_wrap_meth_callback"
  type + 'a t
  type js_string
  external string : string -> js_string t = "caml_js_from_string"
  external to_string : js_string t -> string = "caml_js_to_string"
end

(* keep in sync with jscomp/core/jsoo_main.ml `let implementation` *)
let rewrite code =
  let mapper = jsxMapper () in
  Location.input_name := "//toplevel//";
  try
    let lexer = Lexing.from_string code in
    let pstr = Parse.implementation lexer in
    let pstr = mapper.structure mapper pstr in
    let buffer = Buffer.create 1000 in
    Pprintast.structure Format.str_formatter pstr;
    let ocaml_code = Format.flush_str_formatter () in
    Js.Unsafe.(obj [| "ocaml_code", inject @@ Js.string ocaml_code |])
  with e ->
    match Location.error_of_exn e with
    | Some error ->
        Location.report_error Format.err_formatter error;
        let (file, line, startchar) = Location.get_pos_info error.loc.loc_start in
        let (file, endline, endchar) = Location.get_pos_info error.loc.loc_end in
        Js.Unsafe.(obj
          [|
            "ppx_error_msg", inject @@ Js.string (Printf.sprintf "Line %d, %d: %s" line startchar error.msg);
            "row", inject (line - 1);
            "column", inject startchar;
            "endRow", inject (endline - 1);
            "endColumn", inject endchar;
            "text", inject @@ Js.string error.msg;
            "type", inject @@ Js.string "error";
          |]
        )
    | None ->
        Js.Unsafe.(obj [|
          "js_error_msg" , inject @@ Js.string (Printexc.to_string e)
        |])

let export (field : string) v =
  Js.Unsafe.set (Js.Unsafe.global) field v

let make_ppx name =
  export name
    (Js.Unsafe.(obj
                  [|"rewrite",
                    inject @@
                    Js.wrap_meth_callback
                      (fun _ code -> rewrite (Js.to_string code));
                  |]))

let () = make_ppx "jsxv2" *)

(* #else *)
let () = Ast_mapper.register "JSX" (fun _argv -> jsxMapper ())
(* #end *)