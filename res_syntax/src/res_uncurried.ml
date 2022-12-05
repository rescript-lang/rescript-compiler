type config = Legacy | Default

let init = Legacy

let isDefault = function
  | Legacy -> false
  | Default -> true

(* For parsing *)
let fromDotted ~dotted = function
  | Legacy -> dotted
  | Default -> not dotted

(* For printing *)
let getDotted ~uncurried = function
  | Legacy -> uncurried
  | Default -> not uncurried

let arityType ~loc arity =
  Ast_helper.Typ.variant ~loc
    [Rtag ({txt = string_of_int arity; loc}, [], true, [])]
    Closed None

let arityFromType (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_variant ([Rtag ({txt}, _, _, _)], _, _) ->
    (int_of_string [@doesNotRaise]) txt
  | _ -> assert false

let new_representation arity = arity = 5

let uncurriedType ~loc ~arity tArg =
  if new_representation arity then
    let tArity = arityType ~loc arity in
    Ast_helper.Typ.constr ~loc
      {txt = Ldot (Lident "Js", "uncurried"); loc}
      [tArg; tArity]
  else
    Ast_helper.Typ.constr ~loc
      {
        txt = Ldot (Ldot (Lident "Js", "Fn"), "arity" ^ string_of_int arity);
        loc;
      }
      [tArg]

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

let uncurriedFun ~loc ~arity funExpr =
  if new_representation arity then
    Ast_helper.Exp.construct ~loc
      ~attrs:(arity_to_attributes arity)
      {txt = Ldot (Lident "Js", "Uncurried"); loc}
      (Some funExpr)
  else
    Ast_helper.Exp.record ~loc
      [
        ( {txt = Ldot (Ldot (Lident "Js", "Fn"), "I" ^ string_of_int arity); loc},
          funExpr );
      ]
      None

let exprIsUncurriedFun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_record ([({txt = Ldot (Ldot (Lident "Js", "Fn"), _)}, _e)], None) ->
    true
  | Pexp_construct ({txt = Ldot (Lident "Js", "Uncurried")}, Some _) -> true
  | _ -> false

let exprExtractUncurriedFun (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_record ([({txt = Ldot (Ldot (Lident "Js", "Fn"), _)}, e)], None) -> e
  | Pexp_construct ({txt = Ldot (Lident "Js", "Uncurried")}, Some e) -> e
  | _ -> assert false

let typeIsUncurriedFun (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr
      ({txt = Ldot (Ldot (Lident "Js", "Fn"), _)}, [{ptyp_desc = Ptyp_arrow _}])
    ->
    true
  | Ptyp_constr
      ({txt = Ldot (Lident "Js", "uncurried")}, [{ptyp_desc = Ptyp_arrow _}; _])
    ->
    true
  | _ -> false

let typeExtractUncurriedFun (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr ({txt = Ldot (Ldot (Lident "Js", "Fn"), arity)}, [tArg]) ->
    let arity =
      (int_of_string [@doesNotRaise])
        ((String.sub [@doesNotRaise]) arity 5 (String.length arity - 5))
    in
    (arity, tArg)
  | Ptyp_constr ({txt = Ldot (Lident "Js", "uncurried")}, [tArg; tArity]) ->
    (arityFromType tArity, tArg)
  | _ -> assert false
