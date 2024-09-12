let create_array_spread_expression (e : Parsetree.expression) =
  let loc = e.pexp_loc in
  let unsafe_array_spread =
    Ast_helper.Exp.ident ~loc
      {txt = Ldot (Lident Js_runtime_modules.array, "unsafe_spread"); loc}
  in
  Ast_helper.Exp.apply ~loc unsafe_array_spread [(Nolabel, e)]


