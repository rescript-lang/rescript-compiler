(* Untyped AST *)

let new_representation arity = arity = 5

let arityType ~loc arity =
  let unit = Ast_helper.Typ.constr ~loc { txt = Lident "unit"; loc } [] in
  Ast_helper.Typ.tuple ~loc
    (Array.to_list (Array.make arity unit [@doesNotRaise]))

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

(* Typed AST *)

let arity_to_type arity =
  let type_unit =
    Ctype.newty (Tconstr (Predef.path_unit, [], ref Types.Mnil))
  in
  Ctype.newty (Ttuple (Array.make arity type_unit |> Array.to_list))

let type_to_arity (tArity : Types.type_expr) =
  match tArity.desc with Ttuple tl -> List.length tl | _ -> assert false

let mk_js_fn ~env ~arity t =
  let typ_arity = arity_to_type arity in
  let lid : Longident.t = Ldot (Lident "Js", "uncurried") in
  let path = Env.lookup_type lid env in
  Ctype.newconstr path [ t; typ_arity ]

let uncurried_type_get_arity typ =
  match (Btype.repr typ).desc with
  | Tconstr (Pdot (Pident { name = "Js" }, "uncurried", _), [ _t; tArity ], _)
    ->
      type_to_arity tArity
  | _ -> assert false
