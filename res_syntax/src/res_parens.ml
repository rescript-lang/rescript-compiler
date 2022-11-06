module ParsetreeViewer = Res_parsetree_viewer
type kind = Parenthesized | Braced of Location.t | Nothing

let expr expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | _ -> (
    match expr with
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constraint _} -> Parenthesized
    | _ -> Nothing)

let exprRecordRowRhs e =
  let kind = expr e in
  match kind with
  | Nothing when Res_parsetree_viewer.hasOptionalAttribute e.pexp_attributes
    -> (
    match e.pexp_desc with
    | Pexp_ifthenelse _ | Pexp_fun _ -> Parenthesized
    | _ -> kind)
  | _ -> kind

let callExpr expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | _ -> (
    match expr with
    | {Parsetree.pexp_attributes = attrs}
      when match ParsetreeViewer.filterParsingAttrs attrs with
           | _ :: _ -> true
           | [] -> false ->
      Parenthesized
    | _
      when ParsetreeViewer.isUnaryExpression expr
           || ParsetreeViewer.isBinaryExpression expr ->
      Parenthesized
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_fun _} when ParsetreeViewer.isUnderscoreApplySugar expr
      ->
      Nothing
    | {
     pexp_desc =
       ( Pexp_lazy _ | Pexp_assert _ | Pexp_fun _ | Pexp_newtype _
       | Pexp_function _ | Pexp_constraint _ | Pexp_setfield _ | Pexp_match _
       | Pexp_try _ | Pexp_while _ | Pexp_for _ | Pexp_ifthenelse _ );
    } ->
      Parenthesized
    | _ when ParsetreeViewer.hasAwaitAttribute expr.pexp_attributes ->
      Parenthesized
    | _ -> Nothing)

let structureExpr expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | None -> (
    match expr with
    | _
      when ParsetreeViewer.hasAttributes expr.pexp_attributes
           && not (ParsetreeViewer.isJsxExpression expr) ->
      Parenthesized
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constraint _} -> Parenthesized
    | _ -> Nothing)

let unaryExprOperand expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | None -> (
    match expr with
    | {Parsetree.pexp_attributes = attrs}
      when match ParsetreeViewer.filterParsingAttrs attrs with
           | _ :: _ -> true
           | [] -> false ->
      Parenthesized
    | expr
      when ParsetreeViewer.isUnaryExpression expr
           || ParsetreeViewer.isBinaryExpression expr ->
      Parenthesized
    | {
     pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_fun _} when ParsetreeViewer.isUnderscoreApplySugar expr
      ->
      Nothing
    | {
     pexp_desc =
       ( Pexp_lazy _ | Pexp_assert _ | Pexp_fun _ | Pexp_newtype _
       | Pexp_function _ | Pexp_constraint _ | Pexp_setfield _
       | Pexp_extension _ (* readability? maybe remove *) | Pexp_match _
       | Pexp_try _ | Pexp_while _ | Pexp_for _ | Pexp_ifthenelse _ );
    } ->
      Parenthesized
    | _ when ParsetreeViewer.hasAwaitAttribute expr.pexp_attributes ->
      Parenthesized
    | _ -> Nothing)

let binaryExprOperand ~isLhs expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | None -> (
    match expr with
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_fun _} when ParsetreeViewer.isUnderscoreApplySugar expr
      ->
      Nothing
    | {
     pexp_desc =
       Pexp_constraint _ | Pexp_fun _ | Pexp_function _ | Pexp_newtype _;
    } ->
      Parenthesized
    | expr when ParsetreeViewer.isBinaryExpression expr -> Parenthesized
    | expr when ParsetreeViewer.isTernaryExpr expr -> Parenthesized
    | {pexp_desc = Pexp_lazy _ | Pexp_assert _} when isLhs -> Parenthesized
    | _ when ParsetreeViewer.hasAwaitAttribute expr.pexp_attributes ->
      Parenthesized
    | {Parsetree.pexp_attributes = attrs} ->
      if ParsetreeViewer.hasPrintableAttributes attrs then Parenthesized
      else Nothing)

let subBinaryExprOperand parentOperator childOperator =
  let precParent = ParsetreeViewer.operatorPrecedence parentOperator in
  let precChild = ParsetreeViewer.operatorPrecedence childOperator in
  precParent > precChild
  || precParent == precChild
     && not (ParsetreeViewer.flattenableOperators parentOperator childOperator)
  || (* a && b || c, add parens to (a && b) for readability, who knows the difference by heart… *)
  (parentOperator = "||" && childOperator = "&&")

let rhsBinaryExprOperand parentOperator rhs =
  match rhs.Parsetree.pexp_desc with
  | Parsetree.Pexp_apply
      ( {
          pexp_attributes = [];
          pexp_desc =
            Pexp_ident {txt = Longident.Lident operator; loc = operatorLoc};
        },
        [(_, _left); (_, _right)] )
    when ParsetreeViewer.isBinaryOperator operator
         && not (operatorLoc.loc_ghost && operator = "^") ->
    let precParent = ParsetreeViewer.operatorPrecedence parentOperator in
    let precChild = ParsetreeViewer.operatorPrecedence operator in
    precParent == precChild
  | _ -> false

let flattenOperandRhs parentOperator rhs =
  match rhs.Parsetree.pexp_desc with
  | Parsetree.Pexp_apply
      ( {
          pexp_desc =
            Pexp_ident {txt = Longident.Lident operator; loc = operatorLoc};
        },
        [(_, _left); (_, _right)] )
    when ParsetreeViewer.isBinaryOperator operator
         && not (operatorLoc.loc_ghost && operator = "^") ->
    let precParent = ParsetreeViewer.operatorPrecedence parentOperator in
    let precChild = ParsetreeViewer.operatorPrecedence operator in
    precParent >= precChild || rhs.pexp_attributes <> []
  | Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _}) ->
    false
  | Pexp_fun _ when ParsetreeViewer.isUnderscoreApplySugar rhs -> false
  | Pexp_fun _ | Pexp_newtype _ | Pexp_setfield _ | Pexp_constraint _ -> true
  | _ when ParsetreeViewer.isTernaryExpr rhs -> true
  | _ -> false

let binaryOperatorInsideAwaitNeedsParens operator =
  ParsetreeViewer.operatorPrecedence operator
  < ParsetreeViewer.operatorPrecedence "|."

let lazyOrAssertOrAwaitExprRhs ?(inAwait = false) expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | None -> (
    match expr with
    | {Parsetree.pexp_attributes = attrs}
      when match ParsetreeViewer.filterParsingAttrs attrs with
           | _ :: _ -> true
           | [] -> false ->
      Parenthesized
    | {
     pexp_desc =
       Pexp_apply ({pexp_desc = Pexp_ident {txt = Longident.Lident operator}}, _);
    }
      when ParsetreeViewer.isBinaryExpression expr ->
      if inAwait && not (binaryOperatorInsideAwaitNeedsParens operator) then
        Nothing
      else Parenthesized
    | {
     pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_fun _} when ParsetreeViewer.isUnderscoreApplySugar expr
      ->
      Nothing
    | {
     pexp_desc =
       ( Pexp_lazy _ | Pexp_assert _ | Pexp_fun _ | Pexp_newtype _
       | Pexp_function _ | Pexp_constraint _ | Pexp_setfield _ | Pexp_match _
       | Pexp_try _ | Pexp_while _ | Pexp_for _ | Pexp_ifthenelse _ );
    } ->
      Parenthesized
    | _
      when (not inAwait)
           && ParsetreeViewer.hasAwaitAttribute expr.pexp_attributes ->
      Parenthesized
    | _ -> Nothing)

let isNegativeConstant constant =
  let isNeg txt =
    let len = String.length txt in
    len > 0 && (String.get [@doesNotRaise]) txt 0 = '-'
  in
  match constant with
  | (Parsetree.Pconst_integer (i, _) | Pconst_float (i, _)) when isNeg i -> true
  | _ -> false

let fieldExpr expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | None -> (
    match expr with
    | {Parsetree.pexp_attributes = attrs}
      when match ParsetreeViewer.filterParsingAttrs attrs with
           | _ :: _ -> true
           | [] -> false ->
      Parenthesized
    | expr
      when ParsetreeViewer.isBinaryExpression expr
           || ParsetreeViewer.isUnaryExpression expr ->
      Parenthesized
    | {
     pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constant c} when isNegativeConstant c -> Parenthesized
    | {pexp_desc = Pexp_fun _} when ParsetreeViewer.isUnderscoreApplySugar expr
      ->
      Nothing
    | {
     pexp_desc =
       ( Pexp_lazy _ | Pexp_assert _
       | Pexp_extension _ (* %extension.x vs (%extension).x *) | Pexp_fun _
       | Pexp_newtype _ | Pexp_function _ | Pexp_constraint _ | Pexp_setfield _
       | Pexp_match _ | Pexp_try _ | Pexp_while _ | Pexp_for _
       | Pexp_ifthenelse _ );
    } ->
      Parenthesized
    | _ when ParsetreeViewer.hasAwaitAttribute expr.pexp_attributes ->
      Parenthesized
    | _ -> Nothing)

let setFieldExprRhs expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | None -> (
    match expr with
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constraint _} -> Parenthesized
    | _ -> Nothing)

let ternaryOperand expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | None -> (
    match expr with
    | {
     Parsetree.pexp_desc =
       Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
    } ->
      Nothing
    | {pexp_desc = Pexp_constraint _} -> Parenthesized
    | {pexp_desc = Pexp_fun _ | Pexp_newtype _} -> (
      let _attrsOnArrow, _parameters, returnExpr =
        ParsetreeViewer.funExpr expr
      in
      match returnExpr.pexp_desc with
      | Pexp_constraint _ -> Parenthesized
      | _ -> Nothing)
    | _ -> Nothing)

let startsWithMinus txt =
  let len = String.length txt in
  if len == 0 then false
  else
    let s = (String.get [@doesNotRaise]) txt 0 in
    s = '-'

let jsxPropExpr expr =
  match expr.Parsetree.pexp_desc with
  | Parsetree.Pexp_let _ | Pexp_sequence _ | Pexp_letexception _
  | Pexp_letmodule _ | Pexp_open _ ->
    Nothing
  | _ -> (
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
    | None -> (
      match expr with
      | {
       Parsetree.pexp_desc =
         Pexp_constant (Pconst_integer (x, _) | Pconst_float (x, _));
       pexp_attributes = [];
      }
        when startsWithMinus x ->
        Parenthesized
      | _ when ParsetreeViewer.hasAwaitAttribute expr.pexp_attributes ->
        Parenthesized
      | {
       Parsetree.pexp_desc =
         ( Pexp_ident _ | Pexp_constant _ | Pexp_field _ | Pexp_construct _
         | Pexp_variant _ | Pexp_array _ | Pexp_pack _ | Pexp_record _
         | Pexp_extension _ | Pexp_letmodule _ | Pexp_letexception _
         | Pexp_open _ | Pexp_sequence _ | Pexp_let _ | Pexp_tuple _ );
       pexp_attributes = [];
      } ->
        Nothing
      | {
       Parsetree.pexp_desc =
         Pexp_constraint
           ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
       pexp_attributes = [];
      } ->
        Nothing
      | _ -> Parenthesized))

let jsxChildExpr expr =
  match expr.Parsetree.pexp_desc with
  | Parsetree.Pexp_let _ | Pexp_sequence _ | Pexp_letexception _
  | Pexp_letmodule _ | Pexp_open _ ->
    Nothing
  | _ -> (
    let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
    match optBraces with
    | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
    | _ -> (
      match expr with
      | {
       Parsetree.pexp_desc =
         Pexp_constant (Pconst_integer (x, _) | Pconst_float (x, _));
       pexp_attributes = [];
      }
        when startsWithMinus x ->
        Parenthesized
      | _ when ParsetreeViewer.hasAwaitAttribute expr.pexp_attributes ->
        Parenthesized
      | {
       Parsetree.pexp_desc =
         ( Pexp_ident _ | Pexp_constant _ | Pexp_field _ | Pexp_construct _
         | Pexp_variant _ | Pexp_array _ | Pexp_pack _ | Pexp_record _
         | Pexp_extension _ | Pexp_letmodule _ | Pexp_letexception _
         | Pexp_open _ | Pexp_sequence _ | Pexp_let _ );
       pexp_attributes = [];
      } ->
        Nothing
      | {
       Parsetree.pexp_desc =
         Pexp_constraint
           ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _});
       pexp_attributes = [];
      } ->
        Nothing
      | expr when ParsetreeViewer.isJsxExpression expr -> Nothing
      | _ -> Parenthesized))

let binaryExpr expr =
  let optBraces, _ = ParsetreeViewer.processBracesAttr expr in
  match optBraces with
  | Some ({Location.loc = bracesLoc}, _) -> Braced bracesLoc
  | None -> (
    match expr with
    | {Parsetree.pexp_attributes = _ :: _} as expr
      when ParsetreeViewer.isBinaryExpression expr ->
      Parenthesized
    | _ -> Nothing)

let modTypeFunctorReturn modType =
  match modType with
  | {Parsetree.pmty_desc = Pmty_with _} -> true
  | _ -> false

(* Add parens for readability:
     module type Functor = SetLike => Set with type t = A.t
   This is actually:
     module type Functor = (SetLike => Set) with type t = A.t
*)
let modTypeWithOperand modType =
  match modType with
  | {Parsetree.pmty_desc = Pmty_functor _ | Pmty_with _} -> true
  | _ -> false

let modExprFunctorConstraint modType =
  match modType with
  | {Parsetree.pmty_desc = Pmty_functor _ | Pmty_with _} -> true
  | _ -> false

let bracedExpr expr =
  match expr.Parsetree.pexp_desc with
  | Pexp_constraint ({pexp_desc = Pexp_pack _}, {ptyp_desc = Ptyp_package _}) ->
    false
  | Pexp_constraint _ -> true
  | _ -> false

let includeModExpr modExpr =
  match modExpr.Parsetree.pmod_desc with
  | Parsetree.Pmod_constraint _ -> true
  | _ -> false

let arrowReturnTypExpr typExpr =
  match typExpr.Parsetree.ptyp_desc with
  | Parsetree.Ptyp_arrow _ -> true
  | _ -> false

let patternRecordRowRhs (pattern : Parsetree.pattern) =
  match pattern.ppat_desc with
  | Ppat_constraint ({ppat_desc = Ppat_unpack _}, {ptyp_desc = Ptyp_package _})
    ->
    false
  | Ppat_constraint _ -> true
  | _ -> false
