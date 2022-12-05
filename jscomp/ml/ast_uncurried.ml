(* Untyped AST *)

let new_representation arity = arity = 5

let arityType ~loc arity =
  Ast_helper.Typ.variant ~loc
    [ Rtag ({ txt = string_of_int arity; loc }, [], true, []) ]
    Closed None

let uncurriedType ~loc ~arity tArg =
  if new_representation arity then
    let tArity = arityType ~loc arity in
    Ast_helper.Typ.constr ~loc
      { txt = Ldot (Lident "Js", "uncurried"); loc }
      [ tArg; tArity ]
  else
    Ast_helper.Typ.constr ~loc
      {
        txt = Ldot (Ldot (Lident "Js", "Fn"), "arity" ^ string_of_int arity);
        loc;
      }
      [ tArg ]

let uncurriedFun ~loc ~arity funExpr =
  if new_representation arity then
    let tArity = arityType ~loc arity in
    let tAny = Ast_helper.Typ.any ~loc () in
    let tUncurried =
      Ast_helper.Typ.constr ~loc
        { txt = Ldot (Lident "Js", "uncurried"); loc }
        [ tAny; tArity ]
    in
    let expr =
      Ast_helper.Exp.construct ~loc
        { txt = Ldot (Lident "Js", "Uncurried"); loc }
        (Some funExpr)
    in
    Ast_helper.Exp.constraint_ ~loc expr tUncurried
  else
    Ast_helper.Exp.record ~loc
      [
        ( {
            txt = Ldot (Ldot (Lident "Js", "Fn"), "I" ^ string_of_int arity);
            loc;
          },
          funExpr );
      ]
      None

let exprIsUncurriedFun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_record ([ ({ txt = Ldot (Ldot (Lident "Js", "Fn"), _) }, _e) ], None)
    ->
      true
  | Pexp_constraint
      ( {
          pexp_desc =
            Pexp_construct ({ txt = Ldot (Lident "Js", "Uncurried") }, Some _);
        },
        _ ) ->
      true
  | _ -> false

let exprExtractUncurriedFun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_record ([ ({ txt = Ldot (Ldot (Lident "Js", "Fn"), _) }, e) ], None) ->
      e
  | Pexp_constraint
      ( {
          pexp_desc =
            Pexp_construct ({ txt = Ldot (Lident "Js", "Uncurried") }, Some e);
        },
        _ ) ->
      e
  | _ -> assert false

(* Typed AST *)

let arity_to_type arity =
  let arity_s = string_of_int arity in
  Ctype.newty
    (Tvariant
       {
         row_fields = [ (arity_s, Rpresent None) ];
         row_more = Ctype.newty Tnil;
         row_bound = ();
         row_closed = true;
         row_fixed = false;
         row_name = None;
       })

let type_to_arity (tArity : Types.type_expr) =
  match tArity.desc with
  | Tvariant { row_fields = [ (label, _) ] } -> int_of_string label
  | _ -> assert false

let mk_js_fn ~env ~arity t =
  let typ_arity = arity_to_type arity in
  let lid : Longident.t = Ldot (Lident "Js", "uncurried") in
  let path = Env.lookup_type lid env in
  Ctype.newconstr path [ t; typ_arity ]

let uncurried_type_get_arity ~env typ =
  match (Ctype.expand_head env typ).desc with
  | Tconstr (Pdot (Pident { name = "Js" }, "uncurried", _), [ _t; tArity ], _)
    ->
      type_to_arity tArity
  | _ -> assert false
