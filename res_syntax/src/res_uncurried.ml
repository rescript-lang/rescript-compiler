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
  let unit = Ast_helper.Typ.constr ~loc {txt = Lident "unit"; loc} [] in
  Ast_helper.Typ.tuple ~loc
    (Array.to_list (Array.make arity unit [@doesNotRaise]))

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

let uncurriedFun ~loc ~arity funExpr =
  if new_representation arity then
    let tArity = arityType ~loc arity in
    let tAny = Ast_helper.Typ.any ~loc () in
    let tUncurried =
      Ast_helper.Typ.constr ~loc
        {txt = Ldot (Lident "Js", "uncurried"); loc}
        [tAny; tArity]
    in
    let expr =
      Ast_helper.Exp.construct ~loc
        {txt = Ldot (Lident "Js", "Uncurried"); loc}
        (Some funExpr)
    in
    Ast_helper.Exp.constraint_ ~loc expr tUncurried
  else
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
