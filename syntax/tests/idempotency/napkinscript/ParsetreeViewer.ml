(* Collection of utilities to view the ast in a more a convenient form,
 * allowing for easier processing.
 * Example: given a ptyp_arrow type, what are its arguments and what is the
 * returnType? *)
module ParsetreeViewer : sig
  (* Restructures a nested tree of arrow types into its args & returnType
   * The parsetree contains: a => b => c => d, for printing purposes
   * we restructure the tree into (a, b, c) and its returnType d *)
  val arrowType: Parsetree.core_type ->
      Parsetree.attributes *
      (Parsetree.attributes * Asttypes.arg_label * Parsetree.core_type) list *
      Parsetree.core_type

  val functorType: Parsetree.module_type ->
    (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option) list *
    Parsetree.module_type

  (* filters @bs out of the provided attributes *)
  val processUncurriedAttribute: Parsetree.attributes -> bool * Parsetree.attributes

  (* if ... else if ... else ... is represented as nested expressions: if ... else { if ... }
   * The purpose of this function is to flatten nested ifs into one sequence.
   * Basically compute: ([if, else if, else if, else if], else) *)
  val collectIfExpressions:
    Parsetree.expression ->
      (Parsetree.expression * Parsetree.expression) list * Parsetree.expression option

  val collectListExpressions:
    Parsetree.expression -> (Parsetree.expression list * Parsetree.expression option)

  val funExpr:
    Parsetree.expression ->
      Parsetree.attributes *
      (Parsetree.attributes * Asttypes.arg_label * Parsetree.expression option * Parsetree.pattern) list *
      Parsetree.expression

  (* example:
   *  `makeCoordinate({
   *    x: 1,
   *    y: 2,
   *  })`
   *  Notice howe `({` and `})` "hug" or stick to each other *)
  val isHuggableExpression: Parsetree.expression -> bool

  val isHuggablePattern: Parsetree.pattern -> bool

  (* For better type errors `Js.log("test")` gets parsed as `let () = Js.log("test")`
   * This function determines if `let ()` is written by the user or inserted by
   * the parser *)
  val isGhostUnitBinding: int -> Parsetree.value_binding -> bool

  val operatorPrecedence: string -> int

  val isUnaryExpression: Parsetree.expression -> bool
  val isBinaryOperator: string -> bool
  val isBinaryExpression: Parsetree.expression -> bool

  val isMultiplicativeOperator: string -> bool
  val isEqualityOperator: string -> bool
  val flattenableOperators: string -> string -> bool

  val hasAttributes: Parsetree.attributes -> bool

  val isArrayAccess: Parsetree.expression -> bool
  val isTernaryExpr: Parsetree.expression -> bool

  val collectTernaryParts: Parsetree.expression -> ((Parsetree.expression * Parsetree.expression) list * Parsetree.expression)

  val parametersShouldHug:
    (Parsetree.attributes * Asttypes.arg_label * Parsetree.expression option * Parsetree.pattern ) list -> bool

  val filterTernaryAttributes: Parsetree.attributes -> Parsetree.attributes

  val isJsxExpression: Parsetree.expression -> bool
  val hasJsxAttribute: Parsetree.attributes -> bool

  val shouldIndentBinaryExpr: Parsetree.expression -> bool
  val shouldInlineRhsBinaryExpr: Parsetree.expression -> bool
  val filterPrinteableAttributes: Parsetree.attributes -> Parsetree.attributes
  val partitionPrinteableAttributes: Parsetree.attributes -> (Parsetree.attributes * Parsetree.attributes)

  val requiresSpecialCallbackPrintingLastArg: (Asttypes.arg_label * Parsetree.expression) list -> bool
  val requiresSpecialCallbackPrintingFirstArg: (Asttypes.arg_label * Parsetree.expression) list -> bool

  val modExprApply : Parsetree.module_expr -> (
    Parsetree.module_expr list * Parsetree.module_expr
  )

  val modExprFunctor : Parsetree.module_expr -> (
    (Parsetree.attributes * string Asttypes.loc * Parsetree.module_type option) list *
    Parsetree.module_expr
  )

  val splitGenTypeAttr : Parsetree.attributes -> (bool * Parsetree.attributes)

  val collectPatternsFromListConstruct:
    Parsetree.pattern list -> Parsetree.pattern ->
      (Parsetree.pattern list * Parsetree.pattern)

  val isBlockExpr : Parsetree.expression -> bool

  val isTemplateLiteral: Parsetree.expression -> bool

  val collectOrPatternChain:
    Parsetree.pattern -> Parsetree.pattern list

  val processBracesAttr : Parsetree.expression -> (Parsetree.attribute option * Parsetree.expression)

  val filterParsingAttrs : Parsetree.attributes -> Parsetree.attributes

  val isBracedExpr : Parsetree.expression -> bool

  val isPipeExpr : Parsetree.expression -> bool

  val unwrapMangledValueBinding: Parsetree.value_binding -> Parsetree.value_binding
end = struct
  open Parsetree

  let arrowType ct =
    let rec process attrsBefore acc typ = match typ with
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = []} ->
      let arg = ([], lbl, typ1) in
      process attrsBefore (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel as lbl, typ1, typ2); ptyp_attributes = [({txt ="bs"}, _) ] as attrs} ->
      let arg = (attrs, lbl, typ1) in
      process attrsBefore (arg::acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel, typ1, typ2); ptyp_attributes = attrs} as returnType ->
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

  let collectIfExpressions expr =
    let rec collect acc expr = match expr.pexp_desc with
    | Pexp_ifthenelse (ifExpr, thenExpr, Some elseExpr) ->
      collect ((ifExpr, thenExpr)::acc) elseExpr
    | Pexp_ifthenelse (ifExpr, thenExpr, (None as elseExpr)) ->
      let ifs = List.rev ((ifExpr, thenExpr)::acc) in
      (ifs, elseExpr)
    | _ ->
      (List.rev acc, Some expr)
    in
    collect [] expr

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

  let funExpr expr =
    (* Turns (type t, type u, type z) into "type t u z" *)
    let rec collectNewTypes acc returnExpr =
      match returnExpr with
      | {pexp_desc = Pexp_newtype (stringLoc, returnExpr); pexp_attributes = []} ->
        collectNewTypes (stringLoc::acc) returnExpr
      | returnExpr ->
        let loc = match (acc, List.rev acc) with
        | (startLoc::_, endLoc::_) -> { startLoc.loc with loc_end = endLoc.loc.loc_end }
        | _ -> Location.none
        in
        let txt = List.fold_right (fun curr acc -> acc ^ " " ^ curr.Location.txt) acc "type" in
        (Location.mkloc txt loc, returnExpr)
    in
    (* For simplicity reason Pexp_newtype gets converted to a Nolabel parameter,
     * otherwise this function would need to return a variant:
     * | NormalParamater(...)
     * | NewType(...)
     * This complicates printing with an extra variant/boxing/allocation for a code-path
     * that is not often used. Lets just keep it simple for now *)
    let rec collect attrsBefore acc expr = match expr with
    | {pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr); pexp_attributes = []} ->
      let parameter = ([], lbl, defaultExpr, pattern) in
      collect attrsBefore (parameter::acc) returnExpr
    | {pexp_desc = Pexp_newtype (stringLoc, rest); pexp_attributes = attrs} ->
      let (var, returnExpr) = collectNewTypes [stringLoc] rest in
      let parameter = (
        attrs,
        Asttypes.Nolabel,
        None,
        Ast_helper.Pat.var ~loc:stringLoc.loc var
      ) in
      collect attrsBefore (parameter::acc) returnExpr
    | {pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr); pexp_attributes = [({txt = "bs"}, _)] as attrs} ->
      let parameter = (attrs, lbl, defaultExpr, pattern) in
      collect attrsBefore (parameter::acc) returnExpr
    | {
        pexp_desc = Pexp_fun ((Labelled _ | Optional _) as lbl, defaultExpr, pattern, returnExpr);
        pexp_attributes = attrs
      } ->
      let parameter = (attrs, lbl, defaultExpr, pattern) in
      collect attrsBefore (parameter::acc) returnExpr
    | expr ->
      (attrsBefore, List.rev acc, expr)
    in
    begin match expr with
    | {pexp_desc = Pexp_fun (Nolabel, defaultExpr, pattern, returnExpr); pexp_attributes = attrs} as expr ->
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
      | ({Location.txt = ("ns.ternary" | "ns.braces" | "bs")}, _) -> false
      | _ -> true
    ) attrs

  let isBracedExpr expr =
    match processBracesAttr expr with
    | (Some _, _) -> true
    | _ -> false

  let isHuggableExpression expr =
    match expr.pexp_desc with
    | Pexp_array _
    | Pexp_tuple _
    | Pexp_construct ({txt = Longident.Lident ("::" | "[]")}, _)
    | Pexp_extension ({txt = "bs.obj"}, _)
    | Pexp_record _ -> true
    | _ when isBracedExpr expr -> true
    | _ -> false

  let isHuggablePattern pattern =
    match pattern.ppat_desc with
    | Ppat_array _
    | Ppat_tuple _
    | Ppat_record _
    | Ppat_construct _ -> true
    | _ -> false

  let isGhostUnitBinding i vb = match vb.pvb_pat with
    | {
        ppat_loc=loc;
        ppat_desc = Ppat_construct({txt = Longident.Lident "()"}, None)
      } when loc.loc_ghost && i == 0 -> true
    | _ -> false

  let operatorPrecedence operator = match operator with
    | ":=" -> 1
    | "||" -> 2
    | "&&" -> 3
    | "=" | "==" | "<" | ">" | "!=" | "!==" | "<=" | ">=" | "|>" -> 4
    | "+" | "+." | "-" | "-." | "^" -> 5
    | "*" | "*." | "/" | "/." -> 6
    | "**" -> 7
    | "#" | "##" | "|." -> 8
    | _ -> 0

  let isUnaryOperator operator = match operator with
    | "~+" | "~+."
    | "~-" | "~-."
    | "not" | "!" -> true
    | _ -> false

  let isUnaryExpression expr = match expr.pexp_desc with
    | Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, _arg]
      ) when isUnaryOperator operator -> true
    | _ -> false

  let isBinaryOperator operator = match operator with
    | ":="
    | "||"
    | "&&"
    | "=" | "==" | "<" | ">" | "!=" | "!==" | "<=" | ">=" | "|>"
    | "+" | "+." | "-" | "-." | "++" | "^"
    | "*" | "*." | "/" | "/."
    | "**"
    | "|." | "<>" -> true
    | _ -> false

  let isBinaryExpression expr = match expr.pexp_desc with
    | Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [(Nolabel, _operand1); (Nolabel, _operand2)]
      ) when isBinaryOperator operator -> true
    | _ -> false

  let isMultiplicativeOperator operator = match operator with
    | "*" | "*." | "/" | "/." -> true
    | _ -> false

  let isEqualityOperator operator = match operator with
    | "=" | "==" | "<>" | "!=" -> true
    | _ -> false

  let flattenableOperators parentOperator childOperator =
    let precParent = operatorPrecedence parentOperator in
    let precChild =  operatorPrecedence childOperator in
    if precParent == precChild then
      not (
        isMultiplicativeOperator parentOperator &&
        isMultiplicativeOperator childOperator &&
        parentOperator <> childOperator
      ) && not (
        isEqualityOperator parentOperator &&
        isEqualityOperator childOperator
      )
    else
      false

  let hasAttributes attrs =
    let attrs = List.filter (fun attr -> match attr with
      | ({Location.txt = "bs" | "ns.ternary" | "ns.braces"}, _) -> false
      | _ -> true
    ) attrs in
    match attrs with
    | [] -> false
    | _ -> true

  let isArrayAccess expr = match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")}},
        [Nolabel, parentExpr; Nolabel, memberExpr]
      ) -> true
    | _ -> false

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
    | [([], Asttypes.Nolabel, None, pat)] when isHuggablePattern pat -> true
    | _ -> false

  let filterTernaryAttributes attrs =
    List.filter (fun attr -> match attr with
      |({Location.txt="ns.ternary"},_) -> false
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

  let hasJsxAttribute attributes = match attributes with
    | ({Location.txt = "JSX"},_)::_ -> true
    | _ -> false

  let shouldIndentBinaryExpr expr =
    let samePrecedenceSubExpression operator subExpression =
      match subExpression with
      | {pexp_desc = Pexp_apply (
          {pexp_desc = Pexp_ident {txt = Longident.Lident subOperator}},
          [Nolabel, lhs; Nolabel, rhs]
        )} when isBinaryOperator subOperator ->
        flattenableOperators operator subOperator
      | _ -> true
    in
    match expr with
    | {pexp_desc = Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, lhs; Nolabel, rhs]
      )} when isBinaryOperator operator ->
      isEqualityOperator operator ||
      not (samePrecedenceSubExpression operator lhs)
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
      | ({Location.txt="bs" | "ns.ternary"}, _) -> false
      | _ -> true
    ) attrs

  let partitionPrinteableAttributes attrs =
    List.partition (fun attr -> match attr with
      | ({Location.txt="bs" | "ns.ternary"}, _) -> false
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

  let splitGenTypeAttr attrs =
    match attrs with
    | ({Location.txt = "genType"}, _)::attrs -> (true, attrs)
    | attrs -> (false, attrs)

  let rec collectPatternsFromListConstruct acc pattern =
    let open Parsetree in
    match pattern.ppat_desc with
    | Ppat_construct(
        {txt = Longident.Lident "::"},
        Some {ppat_desc=Ppat_tuple (pat::rest::[])}
      ) ->
      collectPatternsFromListConstruct (pat::acc) rest
    | _ -> List.rev acc, pattern

  let isBlockExpr expr =
    match expr.pexp_desc with
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_let _
    | Pexp_open _
    | Pexp_sequence _ -> true
    | _ -> false

  let rec isTemplateLiteral expr =
    let isPexpConstantString expr = match expr.pexp_desc with
    | Pexp_constant (Pconst_string (_, Some "j")) -> true
    | _ -> false
    in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "^"}},
        [Nolabel, arg1; Nolabel, arg2]
      ) when not (isPexpConstantString arg1 && isPexpConstantString arg2) ->
      isTemplateLiteral arg1 || isTemplateLiteral arg2
    | Pexp_constant (Pconst_string (_, Some _)) -> true
    | _ -> false

  (* Blue | Red | Green -> [Blue; Red; Green] *)
  let collectOrPatternChain pat =
    let rec loop pattern chain =
      match pattern.ppat_desc with
      | Ppat_or (left, right) -> loop left (right::chain)
      | _ -> pattern::chain
    in
    loop pat []

  let isPipeExpr expr = match expr.pexp_desc with
    | Pexp_apply(
        {pexp_desc = Pexp_ident {txt = Longident.Lident ("|." | "|>") }},
        [(Nolabel, _operand1); (Nolabel, operand2)]
      ) -> true
    | _ -> false

  let unwrapMangledValueBinding vb =
    let open Parsetree in
    match (vb.pvb_pat, vb.pvb_expr) with
    | {ppat_desc = Ppat_constraint (pat, {ptyp_desc = Ptyp_poly ([], t)})},
      {pexp_desc = Pexp_constraint (expr, typ)} ->
      {vb with
        pvb_pat = Ast_helper.Pat.constraint_
          ~loc:{pat.ppat_loc with loc_end = t.Parsetree.ptyp_loc.loc_end} pat t;
        pvb_expr = expr;
      }
    | _ -> vb
end
