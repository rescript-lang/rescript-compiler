module ParsetreeCompatibility = struct
  let normalize =
    let open Ast_mapper in
    { default_mapper with
      (* value_binding = (fun mapper vb -> *)
        (* {vb with pvb_expr = mapper.expr mapper vb.pvb_expr} *)
      (* ); *)
      attributes = (fun mapper attrs ->
        List.filter (fun attr ->
          match attr with
          | ({Location.txt = (
                "reason.preserve_braces"
              | "explicit_arity"
              | "implicity_arity"
              | "reason.raw_literal"
            )}, _) -> false
          | _ -> true
        ) attrs
      );
      expr = (fun mapper expr ->
        match expr.pexp_desc with
        | Pexp_function cases ->
          let loc = match (cases, List.rev cases) with
          | (first::_), (last::_) ->
            {first.pc_lhs.ppat_loc with loc_end = last.pc_rhs.pexp_loc.loc_end}
          | _ -> Location.none
          in
          Ast_helper.Exp.fun_ ~loc
            Asttypes.Nolabel None (Ast_helper.Pat.var (Location.mknoloc "x"))
            (Ast_helper.Exp.match_ ~loc
              (Ast_helper.Exp.ident (Location.mknoloc (Longident.Lident "x")))
              (mapper.cases mapper cases)
            )
        | Pexp_apply (
            {pexp_desc = Pexp_ident {txt = Longident.Lident "@@"}},
            [Asttypes.Nolabel, callExpr; Nolabel, argExpr]
          ) ->
          Ast_helper.Exp.apply (mapper.expr mapper callExpr) [
            Asttypes.Nolabel, mapper.expr mapper argExpr
          ]
        | Pexp_apply (
            {pexp_desc = Pexp_ident {txt = Longident.Lident "@"}},
            [Nolabel, arg1; Nolabel, arg2]
          ) ->
          let listConcat = Longident.Ldot (Longident.Lident "List", "append") in
          Ast_helper.Exp.apply
            (Ast_helper.Exp.ident (Location.mknoloc listConcat))
            [Nolabel, mapper.expr mapper arg1; Nolabel, mapper.expr mapper arg2]
        | _ -> default_mapper.expr mapper expr
      )
    }

  let structure s = normalize.Ast_mapper.structure normalize s
end
