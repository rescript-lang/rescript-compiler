let create_await_expression (e : Parsetree.expression) =
  let loc = e.pexp_loc in
  let unsafe_await =
    Ast_helper.Exp.ident ~loc
      {txt = Ldot (Ldot (Lident "Js", "Promise"), "unsafe_await"); loc}
  in
  Ast_helper.Exp.apply ~loc unsafe_await [ (Nolabel, e) ]

let create_await_module_expression ~module_type_name (e : Parsetree.module_expr)
    =
  let open Ast_helper in
  {
    e with
    pmod_desc =
      Pmod_unpack
        (create_await_expression
           (Exp.apply
              (Exp.ident ~loc:e.pmod_loc
                 {
                   txt = Longident.Ldot (Lident "Js", "import");
                   loc = e.pmod_loc;
                 })
              [
                ( Nolabel,
                  Exp.constraint_ ~loc:e.pmod_loc
                    (Exp.pack ~loc:e.pmod_loc e)
                    (Typ.package ~loc:e.pmod_loc
                       { txt = Lident module_type_name; loc = e.pmod_loc }
                       []) );
              ]));
  }
