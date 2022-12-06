(* Untyped AST *)

let new_representation arity = arity = 0 || arity = 5

let arityType ~loc arity =
  Ast_helper.Typ.variant ~loc
    [ Rtag ({ txt = string_of_int arity; loc }, [], true, []) ]
    Closed None

let uncurriedType ~loc ~arity tArg =
  if new_representation arity then
    let tArity = arityType ~loc arity in
    Ast_helper.Typ.constr ~loc
      { txt = Lident "uncurried$"; loc }
      [ tArg; tArity ]
  else
    Ast_helper.Typ.constr ~loc
      {
        txt = Ldot (Ldot (Lident "Js", "Fn"), "arity" ^ string_of_int arity);
        loc;
      }
      [ tArg ]

let arity_to_attributes arity =
  [
    ( Location.mknoloc "res.arity",
      Parsetree.PStr
        [
          Ast_helper.Str.eval
            (Ast_helper.Exp.constant
               (Pconst_integer (string_of_int arity, None)));
        ] );
  ]

let rec attributes_to_arity (attrs : Parsetree.attributes) =
  match attrs with
  | ( { txt = "res.arity" },
      PStr
        [
          {
            pstr_desc =
              Pstr_eval
                ({ pexp_desc = Pexp_constant (Pconst_integer (arity, _)) }, _);
          };
        ] )
    :: _ ->
      int_of_string arity
  | _ :: rest -> attributes_to_arity rest
  | _ -> assert false

let uncurriedFun ~loc ~arity funExpr =
  if new_representation arity then
    Ast_helper.Exp.construct ~loc
      ~attrs:(arity_to_attributes arity)
      { txt = Lident "Uncurried$"; loc }
      (Some funExpr)
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
  | Pexp_construct ({ txt = Lident "Uncurried$" }, Some _) -> true
  | _ -> false

let exprExtractUncurriedFun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_record ([ ({ txt = Ldot (Ldot (Lident "Js", "Fn"), _) }, e) ], None) ->
      e
  | Pexp_construct ({ txt = Lident "Uncurried$" }, Some e) -> e
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
  | Tconstr _ -> assert false
  | Tvar _ -> assert false
  | Tsubst _ -> assert false
  | _ -> assert false

let mk_js_fn ~env ~arity t =
  let typ_arity = arity_to_type arity in
  let lid : Longident.t = Lident "uncurried$" in
  let path = Env.lookup_type lid env in
  Ctype.newconstr path [ t; typ_arity ]

let uncurried_type_get_arity ~env typ =
  match (Ctype.expand_head env typ).desc with
  | Tconstr (Pident { name = "uncurried$" }, [ _t; tArity ], _) ->
      type_to_arity tArity
  | _ -> assert false
