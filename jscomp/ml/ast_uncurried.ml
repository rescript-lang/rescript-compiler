(* Untyped AST *)


let encode_arity_string arity = "Has_arity" ^ string_of_int arity
let decode_arity_string arity_s = int_of_string ((String.sub [@doesNotRaise]) arity_s 9 (String.length arity_s - 9))

let arityType ~loc arity =
  Ast_helper.Typ.variant ~loc
    [ Rtag ({ txt = encode_arity_string arity; loc }, [], true, []) ]
    Closed None

let arityFromType (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_variant ([Rtag ({txt}, _, _, _)], _, _) -> decode_arity_string txt
  | _ -> assert false

let uncurriedType ~loc ~arity tArg =
    let tArity = arityType ~loc arity in
    Ast_helper.Typ.constr ~loc
      { txt = Lident "function$"; loc }
      [ tArg; tArity ]

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
    Ast_helper.Exp.construct ~loc
      ~attrs:(arity_to_attributes arity)
      { txt = Lident "Function$"; loc }
      (Some funExpr)

let exprIsUncurriedFun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Lident "Function$" }, Some _) -> true
  | _ -> false

let exprExtractUncurriedFun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Lident "Function$" }, Some e) -> e
  | _ -> assert false

let typeIsUncurriedFun (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr ({txt = Lident "function$"}, [{ptyp_desc = Ptyp_arrow _}; _]) ->
    true
  | _ -> false

let typeExtractUncurriedFun (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr ({txt = Lident "function$"}, [tArg; tArity]) ->
    (arityFromType tArity, tArg)
  | _ -> assert false

(* Typed AST *)

let arity_to_type arity =
  let arity_s = encode_arity_string arity in
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
  match (Ctype.repr tArity).desc with
  | Tvariant { row_fields = [ (label, _) ] } -> decode_arity_string label
  | _ -> assert false

let make_uncurried_type ~env ~arity t =
  let typ_arity = arity_to_type arity in
  let lid : Longident.t = Lident "function$" in
  let path = Env.lookup_type lid env in
  Ctype.newconstr path [ t; typ_arity ]

let uncurried_type_get_arity ~env typ =
  match (Ctype.expand_head env typ).desc with
  | Tconstr (Pident { name = "function$" }, [ _t; tArity ], _) ->
      type_to_arity tArity
  | _ -> assert false
