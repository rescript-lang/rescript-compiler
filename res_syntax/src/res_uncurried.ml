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

let uncurriedType ~loc ~arity tArg =
  Ast_helper.Typ.constr ~loc
    {txt = Ldot (Ldot (Lident "Js", "Fn"), "arity" ^ string_of_int arity); loc}
    [tArg]

let uncurriedFun ~loc ~arity funExpr =
  Ast_helper.Exp.record ~loc
    [
      ( {txt = Ldot (Ldot (Lident "Js", "Fn"), "I" ^ string_of_int arity); loc},
        funExpr );
    ]
    None

let exprIsUncurriedFun (expr : Parsetree.expression) =
  match expr with
  | {
   pexp_desc =
     Pexp_record ([({txt = Ldot (Ldot (Lident "Js", "Fn"), _)}, _e)], None);
  } ->
    true
  | _ -> false

let exprExtractUncurriedFun (expr : Parsetree.expression) =
  match expr with
  | {
   pexp_desc =
     Pexp_record ([({txt = Ldot (Ldot (Lident "Js", "Fn"), _)}, e)], None);
  } ->
    e
  | _ -> assert false

let typeIsUncurriedFun (typ : Parsetree.core_type) =
  match typ.ptyp_desc with
  | Ptyp_constr
      ({txt = Ldot (Ldot (Lident "Js", "Fn"), _)}, [{ptyp_desc = Ptyp_arrow _}])
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
  | _ -> assert false
