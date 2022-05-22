let create_await_expression (e : Parsetree.expression) =
  let txt =
    Longident.Ldot (Longident.Ldot (Lident "Js", "Promise"), "unsafe_await")
  in
  let pexp_desc = Parsetree.Pexp_ident { txt; loc = e.pexp_loc } in
  { e with pexp_desc = Pexp_apply ({ e with pexp_desc }, [ (Nolabel, e) ]) }
