(* AST for js externals *)
module JsFfi = struct
  type scope =
    | Global
    | Module of string (* bs.module("path") *)
    | Scope of Longident.t (* bs.scope(/"window", "location"/) *)

  type label_declaration = {
    jld_attributes: Parsetree.attributes;
    jld_name: string;
    jld_alias: string;
    jld_type: Parsetree.core_type;
    jld_loc:  Location.t
  }

  type importSpec =
    | Default of label_declaration
    | Spec of label_declaration list

  type import_description = {
    jid_loc: Location.t;
    jid_spec: importSpec;
    jid_scope: scope;
    jid_attributes:  Parsetree.attributes;
  }

  let decl ~attrs ~loc ~name ~alias ~typ = {
    jld_loc = loc;
    jld_attributes = attrs;
    jld_name = name;
    jld_alias = alias;
    jld_type = typ
  }

  let importDescr ~attrs ~scope ~importSpec ~loc = {
    jid_loc = loc;
    jid_spec = importSpec;
    jid_scope = scope;
    jid_attributes = attrs;
  }

  let toParsetree importDescr =
    let bsVal = (Location.mknoloc "bs.val", Parsetree.PStr []) in
    let attrs = match importDescr.jid_scope with
    | Global -> [bsVal]
    (* @genType.import("./MyMath"),
     * @genType.import(/"./MyMath", "default"/) *)
    | Module s ->
      let structure = [
        Parsetree.Pconst_string (s, None)
        |> Ast_helper.Exp.constant
        |> Ast_helper.Str.eval
      ] in
      let genType = (Location.mknoloc "genType.import", Parsetree.PStr structure) in
      [genType]
    | Scope longident ->
      let structureItem =
        let expr = match Longident.flatten longident |> List.map (fun s ->
          Ast_helper.Exp.constant (Parsetree.Pconst_string (s, None))
        ) with
        | [expr] -> expr
        | [] as exprs | (_ as exprs) -> exprs |> Ast_helper.Exp.tuple
        in
        Ast_helper.Str.eval expr
      in
      let bsScope = (
        Location.mknoloc "bs.scope",
        Parsetree. PStr [structureItem]
      ) in
      [bsVal; bsScope]
    in
    let valueDescrs = match importDescr.jid_spec with
    | Default decl ->
      let prim = [decl.jld_name] in
      let allAttrs =
        List.concat [attrs; importDescr.jid_attributes]
        |> List.map (fun attr -> match attr with
          | (
              {Location.txt = "genType.import"} as id,
              Parsetree.PStr [{pstr_desc = Parsetree.Pstr_eval (moduleName, _) }]
            ) ->
            let default =
              Parsetree.Pconst_string ("default", None) |> Ast_helper.Exp.constant
            in
            let structureItem =
              [moduleName; default]
              |> Ast_helper.Exp.tuple
              |> Ast_helper.Str.eval
            in
            (id, Parsetree.PStr [structureItem])
          | attr -> attr
        )
      in
      [Ast_helper.Val.mk
        ~loc:importDescr.jid_loc
        ~prim
        ~attrs:allAttrs
        (Location.mknoloc decl.jld_alias)
        decl.jld_type
      |> Ast_helper.Str.primitive]
    | Spec decls ->
      List.map (fun decl ->
        let prim = [decl.jld_name] in
        let allAttrs = List.concat [attrs; importDescr.jid_attributes] in
        Ast_helper.Val.mk
          ~loc:importDescr.jid_loc
          ~prim
          ~attrs:allAttrs
          (Location.mknoloc decl.jld_alias)
          decl.jld_type
        |> Ast_helper.Str.primitive ~loc:decl.jld_loc
      ) decls
    in
    Ast_helper.Mod.structure ~loc:importDescr.jid_loc valueDescrs
    |> Ast_helper.Incl.mk ~loc:importDescr.jid_loc
    |> Ast_helper.Str.include_ ~loc:importDescr.jid_loc
end
