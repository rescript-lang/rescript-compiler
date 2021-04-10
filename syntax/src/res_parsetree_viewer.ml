open Parsetree

let arrowType ct =
  let rec process attrsBefore acc typ = match typ with
  | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = []} ->
    let arg = ([], lbl, typ1) in
    process attrsBefore (arg::acc) typ2
  | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = [({txt ="bs"}, _) ] as attrs} ->
    let arg = (attrs, lbl, typ1) in
    process attrsBefore (arg::acc) typ2
  | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = _attrs} as returnType ->
    let args = List.rev acc in
    (attrsBefore, args, returnType)
  | {ptyp_desc = Ptyp_arrow ((Labelled _ | Optional _) as lbl, typ1, typ2); ptyp_attributes = attrs} ->
    let arg = (attrs, lbl, typ1) in
    process attrsBefore (arg::acc) typ2
  | typ ->
    (attrsBefore, List.rev acc, typ)
  in
  begin match ct with
  | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = attrs} as typ ->
    process attrs [] {typ with ptyp_attributes = []}
  | typ -> process [] [] typ
  end

let functorType modtype =
  let rec process acc modtype = match modtype with
  | {pmty_desc = Pmty_functor (lbl, argType, returnType); pmty_attributes = attrs} ->
    let arg = (attrs, lbl, argType) in
    process (arg::acc) returnType
  | modType ->
    (List.rev acc, modType)
  in
  process [] modtype

let processUncurriedAttribute attrs =
  let rec process uncurriedSpotted acc attrs =
    match attrs with
    | [] -> (uncurriedSpotted, List.rev acc)
    | ({Location.txt = "bs"}, _)::rest -> process true acc rest
    | attr::rest -> process uncurriedSpotted (attr::acc) rest
  in
  process false [] attrs

let collectListExpressions expr =
  let rec collect acc expr = match expr.pexp_desc with
  | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
    (List.rev acc, None)
  | Pexp_construct (
      {txt = Longident.Lident "::"},
      Some {pexp_desc = Pexp_tuple (hd::[tail])}
    ) ->
      collect (hd::acc) tail
  | _ ->
    (List.rev acc, Some expr)
  in
  collect [] expr

(* (__x) => f(a, __x, c) -----> f(a, _, c)  *)
let rewriteUnderscoreApply expr =
  match expr.pexp_desc with
  | Pexp_fun (
      Nolabel,
      None,
      {ppat_desc = Ppat_var {txt="__x"}},
      ({pexp_desc = Pexp_apply (callExpr, args)} as e)
    ) ->
      let newArgs = List.map (fun arg ->
        match arg with
        | (
            lbl,
            ({pexp_desc = Pexp_ident ({txt = Longident.Lident "__x"} as lid)} as argExpr)
          ) ->
            (lbl, {argExpr with pexp_desc = Pexp_ident ({lid with txt = Longident.Lident "_"})})
        | arg ->  arg
      ) args in
      {e with pexp_desc = Pexp_apply (callExpr, newArgs)}
  | _ -> expr

type funParamKind =
  | Parameter of {
      attrs: Parsetree.attributes;
      lbl: Asttypes.arg_label;
      defaultExpr: Parsetree.expression option;
      pat: Parsetree.pattern;
    }
  | NewTypes of {attrs: Parsetree.attributes; locs: string Asttypes.loc list}

let funExpr expr =
  (* Turns (type t, type u, type z) into "type t u z" *)
  let rec collectNewTypes acc returnExpr =
    match returnExpr with
    | {pexp_desc = Pexp_newtype (stringLoc, returnExpr); pexp_attributes = []} ->
      collectNewTypes (stringLoc::acc) returnExpr
    | returnExpr ->
      (List.rev acc, returnExpr)
  in
  let rec collect attrsBefore acc expr = match expr with
  | {pexp_desc = Pexp_fun (
      Nolabel,
      None,
      {ppat_desc = Ppat_var {txt="__x"}},
      {pexp_desc = Pexp_apply _}
    )} ->
    (attrsBefore, List.rev acc, rewriteUnderscoreApply expr)
  | {pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr); pexp_attributes = []} ->
    let parameter = Parameter {
      attrs = [];
      lbl = lbl;
      defaultExpr = defaultExpr;
      pat = pattern;
    } in
    collect attrsBefore (parameter::acc) returnExpr
  | {pexp_desc = Pexp_newtype (stringLoc, rest); pexp_attributes = attrs} ->
    let (stringLocs, returnExpr) = collectNewTypes [stringLoc] rest in
    let param = NewTypes {attrs; locs = stringLocs} in
    collect attrsBefore (param::acc) returnExpr
  | {pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr); pexp_attributes = [({txt = "bs"}, _)] as attrs} ->
    let parameter = Parameter {
      attrs = attrs;
      lbl = lbl;
      defaultExpr = defaultExpr;
      pat = pattern;
    } in
    collect attrsBefore (parameter::acc) returnExpr
  | {
      pexp_desc = Pexp_fun ((Labelled _ | Optional _) as lbl, defaultExpr, pattern, returnExpr);
      pexp_attributes = attrs
    } ->
    let parameter = Parameter {
      attrs = attrs;
      lbl = lbl;
      defaultExpr = defaultExpr;
      pat = pattern;
    } in
    collect attrsBefore (parameter::acc) returnExpr
  | expr ->
    (attrsBefore, List.rev acc, expr)
  in
  begin match expr with
  | {pexp_desc = Pexp_fun (Nolabel, _defaultExpr, _pattern, _returnExpr); pexp_attributes = attrs} as expr ->
    collect attrs [] {expr with pexp_attributes = []}
  | expr -> collect [] [] expr
  end

let processBracesAttr expr =
  match expr.pexp_attributes with
  | (({txt = "ns.braces"}, _) as attr)::attrs ->
    (Some attr, {expr with pexp_attributes = attrs})
  | _ ->
    (None, expr)

let filterParsingAttrs attrs =
  List.filter (fun attr ->
    match attr with
    | ({Location.txt = ("ns.ternary" | "ns.braces" | "bs" | "ns.iflet" | "ns.namedArgLoc")}, _) -> false
    | _ -> true
  ) attrs

let isBlockExpr expr =
  match expr.pexp_desc with
  | Pexp_letmodule _
  | Pexp_letexception _
  | Pexp_let _
  | Pexp_open _
  | Pexp_sequence _ -> true
  | _ -> false

let isBracedExpr expr =
  match processBracesAttr expr with
  | (Some _, _) -> true
  | _ -> false

let isMultilineText txt =
  let len = String.length txt in
  let rec check i=
    if i >= len then false
    else
      let c = String.unsafe_get txt i in
      match c with
      | '\010' | '\013' -> true
      | '\\' ->
        if (i + 2) = len then false
        else
          check (i + 2)
      | _ -> check (i + 1)
  in
  check 0

let isHuggableExpression expr =
  match expr.pexp_desc with
  | Pexp_array _
  | Pexp_tuple _
  | Pexp_constant (Pconst_string (_, Some _))
  | Pexp_construct ({txt = Longident.Lident ("::" | "[]")}, _)
  | Pexp_extension ({txt = "bs.obj" | "obj"}, _)
  | Pexp_record _ -> true
  | _ when isBlockExpr expr -> true
  | _ when isBracedExpr expr -> true
  | Pexp_constant (Pconst_string (txt, None)) when isMultilineText txt -> true
  | _ -> false

let isHuggableRhs expr =
  match expr.pexp_desc with
  | Pexp_array _
  | Pexp_tuple _
  | Pexp_construct ({txt = Longident.Lident ("::" | "[]")}, _)
  | Pexp_extension ({txt = "bs.obj" | "obj"}, _)
  | Pexp_record _ -> true
  | _ when isBracedExpr expr -> true
  | _ -> false

let isHuggablePattern pattern =
  match pattern.ppat_desc with
  | Ppat_array _
  | Ppat_tuple _
  | Ppat_record _
  | Ppat_variant _
  | Ppat_construct _ -> true
  | _ -> false

let operatorPrecedence operator = match operator with
  | ":=" -> 1
  | "||" -> 2
  | "&&" -> 3
  | "=" | "==" | "<" | ">" | "!=" | "<>" | "!==" | "<=" | ">=" | "|>" -> 4
  | "+" | "+." | "-" | "-." | "^" -> 5
  | "*" | "*." | "/" | "/." -> 6
  | "**" -> 7
  | "#" | "##" | "|." -> 8
  | _ -> 0

let isUnaryOperator operator = match operator with
  | "~+" | "~+." | "~-" | "~-." | "not" -> true
  | _ -> false

let isUnaryExpression expr = match expr.pexp_desc with
  | Pexp_apply(
      {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
      [Nolabel, _arg]
    ) when isUnaryOperator operator -> true
  | _ -> false

(* TODO: tweak this to check for ghost ^ as template literal *)
let isBinaryOperator operator = match operator with
  | ":="
  | "||"
  | "&&"
  | "=" | "==" | "<" | ">" | "!=" | "!==" | "<=" | ">=" | "|>"
  | "+" | "+." | "-" | "-." | "^"
  | "*" | "*." | "/" | "/."
  | "**"
  | "|." | "<>" -> true
  | _ -> false

let isBinaryExpression expr = match expr.pexp_desc with
  | Pexp_apply(
      {pexp_desc = Pexp_ident {txt = Longident.Lident operator; loc = operatorLoc}},
      [(Nolabel, _operand1); (Nolabel, _operand2)]
    ) when isBinaryOperator operator &&
        not (operatorLoc.loc_ghost && operator = "^") (* template literal *)
    -> true
  | _ -> false

let isEqualityOperator operator = match operator with
  | "=" | "==" | "<>" | "!=" -> true
  | _ -> false

let flattenableOperators parentOperator childOperator =
  let precParent = operatorPrecedence parentOperator in
  let precChild =  operatorPrecedence childOperator in
  if precParent == precChild then
    not (
      isEqualityOperator parentOperator &&
      isEqualityOperator childOperator
    )
  else
    false

let rec hasIfLetAttribute attrs =
  match attrs with
  | [] -> false
  | ({Location.txt="ns.iflet"},_)::_ -> true
  | _::attrs -> hasIfLetAttribute attrs

let isIfLetExpr expr = match expr with
  | {
      pexp_attributes = attrs;
      pexp_desc = Pexp_match _
    } when hasIfLetAttribute attrs -> true
  | _ -> false

let hasAttributes attrs =
  List.exists (fun attr -> match attr with
    | ({Location.txt = "bs" | "ns.ternary" | "ns.braces" | "ns.iflet"}, _) -> false
    (* Remove the fragile pattern warning for iflet expressions *)
    | ({Location.txt="warning"}, PStr [{
      pstr_desc = Pstr_eval ({
        pexp_desc = Pexp_constant (
          Pconst_string ("-4", None)
        )
      }, _)
    }]) -> not (hasIfLetAttribute attrs)
    | _ -> true
  ) attrs

let isArrayAccess expr = match expr.pexp_desc with
  | Pexp_apply (
      {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")}},
      [Nolabel, _parentExpr; Nolabel, _memberExpr]
    ) -> true
  | _ -> false


type ifConditionKind =
| If of Parsetree.expression
| IfLet of Parsetree.pattern * Parsetree.expression

let collectIfExpressions expr =
  let rec collect acc expr = match expr.pexp_desc with
  | Pexp_ifthenelse (ifExpr, thenExpr, Some elseExpr) ->
    collect ((If(ifExpr), thenExpr)::acc) elseExpr
  | Pexp_ifthenelse (ifExpr, thenExpr, (None as elseExpr)) ->
    let ifs = List.rev ((If(ifExpr), thenExpr)::acc) in
    (ifs, elseExpr)
      | Pexp_match (condition, [{
    pc_lhs = pattern;
    pc_guard = None;
    pc_rhs = thenExpr;
  }; {
    pc_rhs = {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}
  }]) when isIfLetExpr expr ->
    let ifs = List.rev ((IfLet(pattern, condition), thenExpr)::acc) in
    (ifs, None)
  | Pexp_match (condition, [{
    pc_lhs = pattern;
    pc_guard = None;
    pc_rhs = thenExpr;
  }; {
    pc_rhs = elseExpr;
  }]) when isIfLetExpr expr ->
    collect ((IfLet(pattern, condition), thenExpr)::acc) elseExpr
  | _ ->
    (List.rev acc, Some expr)
  in
  collect [] expr

let rec hasTernaryAttribute attrs =
  match attrs with
  | [] -> false
  | ({Location.txt="ns.ternary"},_)::_ -> true
  | _::attrs -> hasTernaryAttribute attrs

let isTernaryExpr expr = match expr with
  | {
      pexp_attributes = attrs;
      pexp_desc = Pexp_ifthenelse _
    } when hasTernaryAttribute attrs -> true
  | _ -> false

let collectTernaryParts expr =
  let rec collect acc expr = match expr with
  | {
      pexp_attributes = attrs;
      pexp_desc = Pexp_ifthenelse (condition, consequent, Some(alternate))
    } when hasTernaryAttribute attrs -> collect ((condition, consequent)::acc) alternate
  | alternate -> (List.rev acc, alternate)
  in
  collect [] expr

let parametersShouldHug parameters = match parameters with
  | [Parameter {
      attrs = [];
      lbl = Asttypes.Nolabel;
      defaultExpr = None;
      pat = pat
    }] when isHuggablePattern pat -> true
  | _ -> false

let filterTernaryAttributes attrs =
  List.filter (fun attr -> match attr with
    |({Location.txt="ns.ternary"},_) -> false
    | _ -> true
  ) attrs

let filterFragileMatchAttributes attrs =
  List.filter (fun attr -> match attr with
    | ({Location.txt="warning"}, PStr [{
      pstr_desc = Pstr_eval ({
        pexp_desc = Pexp_constant (
          Pconst_string ("-4", _)
        )
      }, _)
    }]) -> false
    | _ -> true
  ) attrs

let isJsxExpression expr =
  let rec loop attrs =
    match attrs with
    | [] -> false
    | ({Location.txt = "JSX"}, _)::_ -> true
    | _::attrs -> loop attrs
  in
  match expr.pexp_desc with
  | Pexp_apply _ ->
    loop expr.Parsetree.pexp_attributes
  | _ -> false

let hasJsxAttribute attributes =
  let rec loop attrs =
    match attrs with
    | [] -> false
    | ({Location.txt = "JSX"}, _)::_ -> true
    | _::attrs -> loop attrs
  in
  loop attributes

let shouldIndentBinaryExpr expr =
  let samePrecedenceSubExpression operator subExpression =
    match subExpression with
    | {pexp_desc = Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident subOperator}},
        [Nolabel, _lhs; Nolabel, _rhs]
      )} when isBinaryOperator subOperator ->
      flattenableOperators operator subOperator
    | _ -> true
  in
  match expr with
  | {pexp_desc = Pexp_apply (
      {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
      [Nolabel, lhs; Nolabel, _rhs]
    )} when isBinaryOperator operator ->
    isEqualityOperator operator ||
    not (samePrecedenceSubExpression operator lhs) ||
    operator = ":="
  | _ -> false

let shouldInlineRhsBinaryExpr rhs = match rhs.pexp_desc with
  | Parsetree.Pexp_constant _
  | Pexp_let _
  | Pexp_letmodule _
  | Pexp_letexception _
  | Pexp_sequence _
  | Pexp_open _
  | Pexp_ifthenelse _
  | Pexp_for _
  | Pexp_while _
  | Pexp_try _
  | Pexp_array _
  | Pexp_record _ -> true
  | _ -> false

let filterPrinteableAttributes attrs =
  List.filter (fun attr -> match attr with
    | ({Location.txt="bs" | "ns.ternary" | "ns.iflet" | "JSX"}, _) -> false
    | _ -> true
  ) attrs

let partitionPrinteableAttributes attrs =
  List.partition (fun attr -> match attr with
    | ({Location.txt="bs" | "ns.ternary" | "ns.iflet" | "JSX"}, _) -> false
    | _ -> true
  ) attrs

let requiresSpecialCallbackPrintingLastArg args =
  let rec loop args = match args with
  | [] -> false
  | [(_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})] -> true
  | (_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})::_ -> false
  | _::rest -> loop rest
  in
  loop args

let requiresSpecialCallbackPrintingFirstArg args =
  let rec loop args = match args with
    | [] -> true
    | (_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})::_ -> false
    | _::rest -> loop rest
  in
  match args with
  | [(_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})] -> false
  | (_, {pexp_desc = Pexp_fun _ | Pexp_newtype _})::rest -> loop rest
  | _ -> false

let modExprApply modExpr =
  let rec loop acc modExpr = match modExpr with
  | {pmod_desc = Pmod_apply (next, arg)} ->
    loop (arg::acc) next
  | _ -> (acc, modExpr)
  in
  loop [] modExpr

let modExprFunctor modExpr =
  let rec loop acc modExpr = match modExpr with
  | {pmod_desc = Pmod_functor (lbl, modType, returnModExpr); pmod_attributes = attrs} ->
    let param = (attrs, lbl, modType) in
    loop (param::acc) returnModExpr
  | returnModExpr ->
    (List.rev acc, returnModExpr)
  in
  loop [] modExpr

let rec collectPatternsFromListConstruct acc pattern =
  let open Parsetree in
  match pattern.ppat_desc with
  | Ppat_construct(
      {txt = Longident.Lident "::"},
      Some {ppat_desc=Ppat_tuple (pat::rest::[])}
    ) ->
    collectPatternsFromListConstruct (pat::acc) rest
  | _ -> List.rev acc, pattern

(* Simple heuristic to detect template literal sugar:
 *  `${user.name} lastName` parses internally as user.name ++ ` lastName`.
 *  The thing is: the ++ operator (parsed as `^`)  will always have a ghost loc.
 *  A ghost loc is only produced by our parser.
 *  Hence, if we have that ghost operator, we know for sure it's a template literal. *)
let isTemplateLiteral expr =
  match expr.pexp_desc with
  | Pexp_apply (
      {pexp_desc = Pexp_ident {txt = Longident.Lident "^"; loc}},
      [Nolabel, _; Nolabel, _]
    ) when loc.loc_ghost -> true
  | _ -> false

(* Blue | Red | Green -> [Blue; Red; Green] *)
let collectOrPatternChain pat =
  let rec loop pattern chain =
    match pattern.ppat_desc with
    | Ppat_or (left, right) -> loop left (right::chain)
    | _ -> pattern::chain
  in
  loop pat []

let isSinglePipeExpr expr =
  (* handles:
   *   x
   *   ->Js.Dict.get("wm-property")
   *   ->Option.flatMap(Js.Json.decodeString)
   *   ->Option.flatMap(x =>
   *     switch x {
   *     | "like-of" => Some(#like)
   *     | "repost-of" => Some(#repost)
   *     | _ => None
   *     }
   *   )
   *)
  let isPipeExpr expr = match expr.pexp_desc with
  | Pexp_apply(
      {pexp_desc = Pexp_ident {txt = Longident.Lident ("|." | "|>") }},
      [(Nolabel, _operand1); (Nolabel, _operand2)]
    )  -> true
  | _ -> false
  in
  match expr.pexp_desc with
  | Pexp_apply(
      {pexp_desc = Pexp_ident {txt = Longident.Lident ("|." | "|>") }},
      [(Nolabel, operand1); (Nolabel, _operand2)]
    ) when not (isPipeExpr operand1) -> true
  | _ -> false

let isUnderscoreApplySugar expr =
  match expr.pexp_desc with
  | Pexp_fun (
      Nolabel,
      None,
      {ppat_desc = Ppat_var {txt="__x"}},
      {pexp_desc = Pexp_apply _}
    ) -> true
  | _ -> false
