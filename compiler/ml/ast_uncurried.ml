(* Uncurried AST *)

let encode_arity_string arity = "Has_arity" ^ string_of_int arity

let arity_type ~loc arity =
  Ast_helper.Typ.variant ~loc
    [Rtag ({txt = encode_arity_string arity; loc}, [], true, [])]
    Closed None

let uncurried_type ~loc ~arity (t_arg : Parsetree.core_type) =
  let t_arg =
    match t_arg.ptyp_desc with
    | Ptyp_arrow (l, t1, t2, _) ->
      {t_arg with ptyp_desc = Ptyp_arrow (l, t1, t2, Some arity)}
    | _ -> assert false
  in
  let t_arity = arity_type ~loc arity in
  Ast_helper.Typ.constr ~loc {txt = Lident "function$"; loc} [t_arg; t_arity]

let uncurried_fun ~arity fun_expr =
  let fun_expr =
    match fun_expr.Parsetree.pexp_desc with
    | Pexp_fun (l, eo, p, e, _) ->
      {fun_expr with pexp_desc = Pexp_fun (l, eo, p, e, Some arity)}
    | _ -> assert false
  in
  fun_expr

let expr_is_uncurried_fun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_fun (_, _, _, _, Some _) -> true
  | _ -> false

let expr_extract_uncurried_fun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_fun (_, _, _, _, Some _) -> expr
  | _ -> assert false

let core_type_is_uncurried_fun (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr ({txt = Lident "function$"}, [{ptyp_desc = Ptyp_arrow _}; _]) ->
    true
  | _ -> false

let core_type_extract_uncurried_fun (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr
      ( {txt = Lident "function$"},
        [({ptyp_desc = Ptyp_arrow (_, _, _, Some arity)} as t_arg); _] ) ->
    (arity, t_arg)
  | _ -> assert false

let type_is_uncurried_fun = Ast_uncurried_utils.type_is_uncurried_fun

let type_extract_uncurried_fun (typ : Types.type_expr) =
  match typ.desc with
  | Tconstr (Pident {name = "function$"}, [t_arg; _], _) -> t_arg
  | _ -> assert false

(* Typed AST *)

let arity_to_type arity =
  let arity_s = encode_arity_string arity in
  Ctype.newty
    (Tvariant
       {
         row_fields = [(arity_s, Rpresent None)];
         row_more = Ctype.newty Tnil;
         row_bound = ();
         row_closed = true;
         row_fixed = false;
         row_name = None;
       })

let tarrow_to_arity (t_arity : Types.type_expr) =
  match (Ctype.repr t_arity).desc with
  | Tarrow (_, _, _, _, Some arity) -> arity
  | Tarrow _ -> assert false
  | _ ->
    Format.eprintf "t: %a@." Printtyp.raw_type_expr t_arity;
    assert false

let tarrow_to_arity_opt (t_arity : Types.type_expr) =
  match (Ctype.repr t_arity).desc with
  | Tarrow (_, _, _, _, arity) -> arity
  | _ -> None

let make_uncurried_type ~env ~arity (t : Types.type_expr) =
  let typ_arity = arity_to_type arity in
  let lid : Longident.t = Lident "function$" in
  let path = Env.lookup_type lid env in
  let t =
    match t.desc with
    | Tarrow (l, t1, t2, c, _) ->
      {t with desc = Tarrow (l, t1, t2, c, Some arity)}
    | Tconstr _ -> assert false
    | Tvar _ -> t
    | _ -> assert false
  in
  Ctype.newconstr path [t; typ_arity]

let uncurried_type_get_arity ~env typ =
  match (Ctype.expand_head env typ).desc with
  | Tconstr (Pident {name = "function$"}, [t; _arity], _) -> tarrow_to_arity t
  | _ -> assert false

let uncurried_type_get_arity_opt ~env typ =
  match (Ctype.expand_head env typ).desc with
  | Tconstr (Pident {name = "function$"}, [t; _arity], _) ->
    Some (tarrow_to_arity t)
  | _ -> None
