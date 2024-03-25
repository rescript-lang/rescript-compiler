open Asttypes
open Parsetree

type jsxConfig = {
  mutable version: int;
  mutable module_: string;
  mutable mode: string;
  mutable nestedModules: string list;
  mutable hasComponent: bool;
}

(* Helper method to look up the [@react.component] attribute *)
let hasAttr (loc, _) =
  match loc.txt with
  | "react.component" | "jsx.component" -> true
  | _ -> false

(* Iterate over the attributes and try to find the [@react.component] attribute *)
let hasAttrOnBinding {pvb_attributes} =
  List.find_opt hasAttr pvb_attributes <> None

let coreTypeOfAttrs attributes =
  List.find_map
    (fun ({txt}, payload) ->
      match (txt, payload) with
      | ("react.component" | "jsx.component"), PTyp coreType -> Some coreType
      | _ -> None)
    attributes

let typVarsOfCoreType {ptyp_desc} =
  match ptyp_desc with
  | Ptyp_constr (_, coreTypes) ->
    List.filter
      (fun {ptyp_desc} ->
        match ptyp_desc with
        | Ptyp_var _ -> true
        | _ -> false)
      coreTypes
  | _ -> []

let raiseError ~loc msg = Location.raise_errorf ~loc msg

let raiseErrorMultipleComponent ~loc =
  raiseError ~loc
    "Only one component definition is allowed for each module. Move to a \
     submodule or other file if necessary."

let optionalAttr = ({txt = "res.optional"; loc = Location.none}, PStr [])

let extractUncurried typ =
  if Ast_uncurried.coreTypeIsUncurriedFun typ then
    let _arity, t = Ast_uncurried.coreTypeExtractUncurriedFun typ in
    t
  else typ

let removeArity binding =
  let rec removeArityRecord expr =
    match expr.pexp_desc with
    | _ when Ast_uncurried.exprIsUncurriedFun expr ->
      Ast_uncurried.exprExtractUncurriedFun expr
    | Pexp_newtype (label, e) ->
      {expr with pexp_desc = Pexp_newtype (label, removeArityRecord e)}
    | Pexp_apply (forwardRef, [(label, e)]) ->
      {
        expr with
        pexp_desc = Pexp_apply (forwardRef, [(label, removeArityRecord e)]);
      }
    | _ -> expr
  in
  {binding with pvb_expr = removeArityRecord binding.pvb_expr}

let async_component ~async expr =
  if async then
    let open Ast_helper in
    Exp.apply
      (Exp.ident
         {
           loc = Location.none;
           txt = Ldot (Lident "JsxPPXReactSupport", "asyncComponent");
         })
      [(Nolabel, expr)]
  else expr
