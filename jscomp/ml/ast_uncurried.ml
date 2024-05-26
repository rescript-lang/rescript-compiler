(* Uncurried AST *)


let encode_arity_string arity = "Has_arity" ^ string_of_int arity
let decode_arity_string arity_s = int_of_string ((String.sub [@doesNotRaise]) arity_s 9 (String.length arity_s - 9))

let arity_type ~loc arity =
  Ast_helper.Typ.variant ~loc
    [ Rtag ({ txt = encode_arity_string arity; loc }, [], true, []) ]
    Closed None

let arity_from_type (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_variant ([Rtag ({txt}, _, _, _)], _, _) -> decode_arity_string txt
  | _ -> assert false

let uncurried_type ~loc ~arity t_arg =
    let t_arity = arity_type ~loc arity in
    Ast_helper.Typ.constr ~loc
      { txt = Lident "function$"; loc }
      [ t_arg; t_arity ]

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

let uncurried_fun ~loc ~arity fun_expr =
    Ast_helper.Exp.construct ~loc
      ~attrs:(arity_to_attributes arity)
      (Location.mknoloc (Longident.Lident "Function$"))
      (Some fun_expr)

let expr_is_uncurried_fun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Lident "Function$" }, Some _) -> true
  | _ -> false

let expr_extract_uncurried_fun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_construct ({ txt = Lident "Function$" }, Some e) -> e
  | _ -> assert false

let core_type_is_uncurried_fun (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr ({txt = Lident "function$"}, [{ptyp_desc = Ptyp_arrow _}; _]) ->
    true
  | _ -> false

let core_type_extract_uncurried_fun (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr ({txt = Lident "function$"}, [t_arg; t_arity]) ->
    (arity_from_type t_arity, t_arg)
  | _ -> assert false

let type_is_uncurried_fun = Ast_uncurried_utils.type_is_uncurried_fun

let type_extract_uncurried_fun (typ : Types.type_expr) = 
  match typ.desc with
  | Tconstr (Pident {name = "function$"}, [t_arg; _], _) ->
    t_arg
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

let type_to_arity (t_arity : Types.type_expr) =
  match (Ctype.repr t_arity).desc with
  | Tvariant { row_fields = [ (label, _) ] } -> decode_arity_string label
  | _ -> assert false

let make_uncurried_type ~env ~arity t =
  let typ_arity = arity_to_type arity in
  let lid : Longident.t = Lident "function$" in
  let path = Env.lookup_type lid env in
  Ctype.newconstr path [ t; typ_arity ]

let uncurried_type_get_arity ~env typ =
  match (Ctype.expand_head env typ).desc with
  | Tconstr (Pident { name = "function$" }, [ _t; t_arity ], _) ->
      type_to_arity t_arity
  | _ -> assert false

let uncurried_type_get_arity_opt ~env typ =
  match (Ctype.expand_head env typ).desc with
  | Tconstr (Pident { name = "function$" }, [ _t; t_arity ], _) ->
      Some (type_to_arity t_arity)
  | _ -> None



