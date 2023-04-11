let create_await_expression (e : Parsetree.expression) =
  let loc = e.pexp_loc in
  let unsafe_await =
    Ast_helper.Exp.ident ~loc
      {txt = Ldot (Ldot (Lident "Js", "Promise"), "unsafe_await"); loc}
  in
  Ast_helper.Exp.apply ~loc unsafe_await [(Nolabel, e)]
