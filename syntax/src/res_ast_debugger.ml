module Doc = Res_doc

let printEngine = Res_driver.{
  printImplementation = begin fun ~width:_ ~filename:_ ~comments:_ structure ->
    Printast.implementation Format.std_formatter structure
  end;
  printInterface = begin fun ~width:_ ~filename:_ ~comments:_ signature ->
    Printast.interface Format.std_formatter signature
  end;
}

module Sexp: sig
  type t

  val atom: string -> t
  val list: t list -> t
  val toString: t -> string
end = struct
  type t =
    | Atom of string
    | List of t list

  let atom s = Atom s
  let list l = List l

  let rec toDoc t =
    match t with
    | Atom s -> Doc.text s
    | List [] -> Doc.text "()"
    | List [sexpr] -> Doc.concat [Doc.lparen; toDoc sexpr; Doc.rparen;]
    | List (hd::tail) ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          toDoc hd;
          Doc.indent (
            Doc.concat [
              Doc.line;
              Doc.join ~sep:Doc.line (List.map toDoc tail);
            ]
          );
          Doc.rparen;
        ]
      )

  let toString sexpr =
    let doc = toDoc sexpr in
    Doc.toString ~width:80 doc
end

module SexpAst = struct
  open Parsetree

  let mapEmpty ~f items =
    match items with
    | [] -> [Sexp.list []]
    | items -> List.map f items

  let string txt =
    Sexp.atom ("\"" ^ txt ^ "\"")

  let char c =
    Sexp.atom ("'" ^ (Char.escaped c) ^ "'")

  let optChar oc =
    match oc with
    | None -> Sexp.atom "None"
    | Some c ->
      Sexp.list [
        Sexp.atom "Some";
        char c
      ]

  let longident l =
    let rec loop l = match l with
    | Longident.Lident ident -> Sexp.list [
        Sexp.atom "Lident";
        string ident;
      ]
    | Longident.Ldot (lident, txt) ->
      Sexp.list [
        Sexp.atom "Ldot";
        loop lident;
        string txt;
      ]
    | Longident.Lapply (l1, l2) ->
      Sexp.list [
        Sexp.atom "Lapply";
        loop l1;
        loop l2;
      ]
    in
    Sexp.list [
      Sexp.atom "longident";
      loop l;
    ]

  let closedFlag flag = match flag with
    | Asttypes.Closed -> Sexp.atom "Closed"
    | Open -> Sexp.atom "Open"

  let directionFlag flag = match flag with
    | Asttypes.Upto -> Sexp.atom "Upto"
    | Downto -> Sexp.atom "Downto"

  let recFlag flag = match flag with
    | Asttypes.Recursive -> Sexp.atom "Recursive"
    | Nonrecursive -> Sexp.atom "Nonrecursive"

  let overrideFlag flag = match flag with
    | Asttypes.Override -> Sexp.atom "Override"
    | Fresh -> Sexp.atom "Fresh"

  let privateFlag flag = match flag with
    | Asttypes.Public -> Sexp.atom "Public"
    | Private -> Sexp.atom "Private"

  let mutableFlag flag = match flag with
    | Asttypes.Immutable -> Sexp.atom "Immutable"
    | Mutable -> Sexp.atom "Mutable"

   let variance v = match v with
     | Asttypes.Covariant -> Sexp.atom "Covariant"
     | Contravariant -> Sexp.atom "Contravariant"
     | Invariant -> Sexp.atom "Invariant"

  let argLabel lbl = match lbl with
    | Asttypes.Nolabel -> Sexp.atom "Nolabel"
    | Labelled txt -> Sexp.list [
        Sexp.atom "Labelled";
        string txt;
      ]
    | Optional txt -> Sexp.list [
        Sexp.atom "Optional";
        string txt;
      ]

  let constant c =
    let sexpr = match c with
    | Pconst_integer (txt, tag) ->
      Sexp.list [
        Sexp.atom "Pconst_integer";
        string txt;
        optChar tag;
      ]
    | Pconst_char c ->
      Sexp.list [
        Sexp.atom "Pconst_char";
        Sexp.atom (Char.escaped c);
      ]
    | Pconst_string (txt, tag) ->
      Sexp.list [
        Sexp.atom "Pconst_string";
        string txt;
        match tag with
        | Some txt -> Sexp.list [
            Sexp.atom "Some";
            string txt;
          ]
        | None -> Sexp.atom "None";
      ]
    | Pconst_float (txt, tag)  ->
      Sexp.list [
        Sexp.atom "Pconst_float";
        string txt;
        optChar tag;
      ]
    in
      Sexp.list [
        Sexp.atom "constant";
        sexpr
      ]

  let rec structure s =
    Sexp.list (
      (Sexp.atom "structure")::(List.map structureItem s)
    )

  and structureItem si =
    let desc = match si.pstr_desc with
    | Pstr_eval (expr, attrs) ->
      Sexp.list [
        Sexp.atom "Pstr_eval";
        expression expr;
        attributes attrs;
      ]
    | Pstr_value (flag, vbs) ->
      Sexp.list [
        Sexp.atom "Pstr_value";
        recFlag flag;
        Sexp.list (mapEmpty ~f:valueBinding vbs)
      ]
    | Pstr_primitive (vd) ->
      Sexp.list [
        Sexp.atom "Pstr_primitive";
        valueDescription vd;
      ]
    | Pstr_type (flag, tds) ->
      Sexp.list [
        Sexp.atom "Pstr_type";
        recFlag flag;
        Sexp.list (mapEmpty ~f:typeDeclaration tds)
      ]
    | Pstr_typext typext ->
      Sexp.list [
        Sexp.atom "Pstr_type";
        typeExtension typext;
      ]
    | Pstr_exception ec ->
      Sexp.list [
        Sexp.atom "Pstr_exception";
        extensionConstructor ec;
      ]
    | Pstr_module mb ->
      Sexp.list [
        Sexp.atom "Pstr_module";
        moduleBinding mb;
      ]
    | Pstr_recmodule mbs ->
      Sexp.list [
        Sexp.atom "Pstr_recmodule";
        Sexp.list (mapEmpty ~f:moduleBinding mbs);
      ]
    | Pstr_modtype modTypDecl ->
      Sexp.list [
        Sexp.atom "Pstr_modtype";
        moduleTypeDeclaration modTypDecl;
      ]
    | Pstr_open openDesc ->
      Sexp.list [
        Sexp.atom "Pstr_open";
        openDescription openDesc;
      ]
    | Pstr_class _ -> Sexp.atom "Pstr_class"
    | Pstr_class_type _ -> Sexp.atom "Pstr_class_type"
    | Pstr_include id ->
      Sexp.list [
        Sexp.atom "Pstr_include";
        includeDeclaration id;
      ]
    | Pstr_attribute attr ->
      Sexp.list [
        Sexp.atom "Pstr_attribute";
        attribute attr;
      ]
    | Pstr_extension (ext, attrs) ->
      Sexp.list [
        Sexp.atom "Pstr_extension";
        extension ext;
        attributes attrs;
      ]
    in
    Sexp.list [
      Sexp.atom "structure_item";
      desc;
    ]

  and includeDeclaration id =
    Sexp.list [
      Sexp.atom "include_declaration";
      moduleExpression id.pincl_mod;
      attributes id.pincl_attributes;
    ]

  and openDescription od =
    Sexp.list [
      Sexp.atom "open_description";
      longident od.popen_lid.Asttypes.txt;
      attributes od.popen_attributes;
    ]

  and moduleTypeDeclaration mtd =
    Sexp.list [
      Sexp.atom "module_type_declaration";
      string mtd.pmtd_name.Asttypes.txt;
      (match mtd.pmtd_type with
      | None -> Sexp.atom "None"
      | Some modType -> Sexp.list [
          Sexp.atom "Some";
          moduleType modType;
      ]);
      attributes mtd.pmtd_attributes;
    ]

  and moduleBinding mb =
    Sexp.list [
      Sexp.atom "module_binding";
      string mb.pmb_name.Asttypes.txt;
      moduleExpression mb.pmb_expr;
      attributes mb.pmb_attributes;
    ]

  and moduleExpression me =
    let desc = match me.pmod_desc with
    | Pmod_ident modName ->
      Sexp.list [
        Sexp.atom "Pmod_ident";
        longident modName.Asttypes.txt;
      ]
    | Pmod_structure s ->
      Sexp.list [
        Sexp.atom "Pmod_structure";
        structure s;
      ]
    | Pmod_functor (lbl, optModType, modExpr) ->
      Sexp.list [
        Sexp.atom "Pmod_functor";
        string lbl.Asttypes.txt;
        (match optModType with
        | None -> Sexp.atom "None"
        | Some modType -> Sexp.list [
            Sexp.atom "Some";
            moduleType modType;
        ]);
        moduleExpression modExpr;
      ]
    | Pmod_apply (callModExpr, modExprArg) ->
      Sexp.list [
        Sexp.atom "Pmod_apply";
        moduleExpression callModExpr;
        moduleExpression modExprArg;
      ]
    | Pmod_constraint (modExpr, modType) ->
      Sexp.list [
        Sexp.atom "Pmod_constraint";
        moduleExpression modExpr;
        moduleType modType;
      ]
    | Pmod_unpack expr ->
      Sexp.list [
        Sexp.atom "Pmod_unpack";
        expression expr;
      ]
    | Pmod_extension ext ->
      Sexp.list [
        Sexp.atom "Pmod_extension";
        extension ext;
      ]
    in
    Sexp.list [
      Sexp.atom "module_expr";
      desc;
      attributes me.pmod_attributes;
    ]

  and moduleType mt =
    let desc = match mt.pmty_desc with
    | Pmty_ident longidentLoc ->
      Sexp.list [
        Sexp.atom "Pmty_ident";
        longident longidentLoc.Asttypes.txt;
      ]
    | Pmty_signature s ->
      Sexp.list [
        Sexp.atom "Pmty_signature";
        signature s;
      ]
    | Pmty_functor (lbl, optModType, modType) ->
      Sexp.list [
        Sexp.atom "Pmty_functor";
        string lbl.Asttypes.txt;
        (match optModType with
        | None -> Sexp.atom "None"
        | Some modType -> Sexp.list [
            Sexp.atom "Some";
            moduleType modType;
        ]);
        moduleType modType;
      ]
    | Pmty_alias longidentLoc ->
      Sexp.list [
        Sexp.atom "Pmty_alias";
        longident longidentLoc.Asttypes.txt;
      ]
    | Pmty_extension ext ->
      Sexp.list [
        Sexp.atom "Pmty_extension";
        extension ext;
      ]
    | Pmty_typeof modExpr ->
      Sexp.list [
        Sexp.atom "Pmty_typeof";
        moduleExpression modExpr;
      ]
    | Pmty_with (modType, withConstraints) ->
      Sexp.list [
        Sexp.atom "Pmty_with";
        moduleType modType;
        Sexp.list (mapEmpty ~f:withConstraint withConstraints);
      ]
    in
    Sexp.list [
      Sexp.atom "module_type";
      desc;
      attributes mt.pmty_attributes;
    ]

  and withConstraint wc = match wc with
    | Pwith_type (longidentLoc, td) ->
      Sexp.list [
        Sexp.atom "Pmty_with";
        longident longidentLoc.Asttypes.txt;
        typeDeclaration td;
      ]
    | Pwith_module (l1, l2) ->
      Sexp.list [
        Sexp.atom "Pwith_module";
        longident l1.Asttypes.txt;
        longident l2.Asttypes.txt;
      ]
    | Pwith_typesubst (longidentLoc, td) ->
      Sexp.list [
        Sexp.atom "Pwith_typesubst";
        longident longidentLoc.Asttypes.txt;
        typeDeclaration td;
      ]
    | Pwith_modsubst (l1, l2) ->
      Sexp.list [
        Sexp.atom "Pwith_modsubst";
        longident l1.Asttypes.txt;
        longident l2.Asttypes.txt;
      ]

  and signature s =
    Sexp.list (
      (Sexp.atom "signature")::(List.map signatureItem s)
    )

  and signatureItem si =
    let descr = match si.psig_desc with
    | Psig_value vd ->
      Sexp.list [
        Sexp.atom "Psig_value";
        valueDescription vd;
      ]
    | Psig_type (flag, typeDeclarations) ->
      Sexp.list [
        Sexp.atom "Psig_type";
        recFlag flag;
        Sexp.list (mapEmpty ~f:typeDeclaration typeDeclarations);
      ]
    | Psig_typext typExt ->
      Sexp.list [
        Sexp.atom "Psig_typext";
        typeExtension typExt;
      ]
    | Psig_exception extConstr ->
      Sexp.list [
        Sexp.atom "Psig_exception";
        extensionConstructor extConstr;
      ]
    | Psig_module modDecl ->
      Sexp.list [
        Sexp.atom "Psig_module";
        moduleDeclaration modDecl;
      ]
    | Psig_recmodule modDecls ->
      Sexp.list [
        Sexp.atom "Psig_recmodule";
        Sexp.list (mapEmpty ~f:moduleDeclaration modDecls);
      ]
    | Psig_modtype modTypDecl ->
      Sexp.list [
        Sexp.atom "Psig_modtype";
        moduleTypeDeclaration modTypDecl;
      ]
    | Psig_open openDesc ->
      Sexp.list [
        Sexp.atom "Psig_open";
        openDescription openDesc;
      ]
    | Psig_include inclDecl ->
      Sexp.list [
        Sexp.atom "Psig_include";
        includeDescription inclDecl
      ]
    | Psig_class _ -> Sexp.list [Sexp.atom "Psig_class";]
    | Psig_class_type _ -> Sexp.list [ Sexp.atom "Psig_class_type"; ]
    | Psig_attribute attr ->
      Sexp.list [
        Sexp.atom "Psig_attribute";
        attribute attr;
      ]
    | Psig_extension (ext, attrs) ->
      Sexp.list [
        Sexp.atom "Psig_extension";
        extension ext;
        attributes attrs;
      ]
    in
    Sexp.list [
      Sexp.atom "signature_item";
      descr;
    ]

  and includeDescription id =
    Sexp.list [
      Sexp.atom "include_description";
      moduleType id.pincl_mod;
      attributes id.pincl_attributes;
    ]

  and moduleDeclaration md =
    Sexp.list [
      Sexp.atom "module_declaration";
      string md.pmd_name.Asttypes.txt;
      moduleType md.pmd_type;
      attributes md.pmd_attributes;
    ]

  and valueBinding vb =
    Sexp.list [
      Sexp.atom "value_binding";
      pattern vb.pvb_pat;
      expression vb.pvb_expr;
      attributes vb.pvb_attributes;
    ]

  and valueDescription vd =
    Sexp.list [
      Sexp.atom "value_description";
      string vd.pval_name.Asttypes.txt;
      coreType vd.pval_type;
      Sexp.list (mapEmpty ~f:string vd.pval_prim);
      attributes vd.pval_attributes;
    ]

  and typeDeclaration td =
    Sexp.list [
      Sexp.atom "type_declaration";
      string td.ptype_name.Asttypes.txt;
      Sexp.list [
        Sexp.atom "ptype_params";
        Sexp.list (mapEmpty ~f:(fun (typexpr, var) ->
          Sexp.list [
            coreType typexpr;
            variance var;
          ]) td.ptype_params)
      ];
      Sexp.list [
        Sexp.atom "ptype_cstrs";
        Sexp.list (mapEmpty ~f:(fun (typ1, typ2, _loc) ->
          Sexp.list [
            coreType typ1;
            coreType typ2;
          ]) td.ptype_cstrs)
      ];
      Sexp.list [
        Sexp.atom "ptype_kind";
        typeKind td.ptype_kind;
      ];
      Sexp.list [
        Sexp.atom "ptype_manifest";
        match td.ptype_manifest with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            coreType typ;
          ]
      ];
      Sexp.list [
        Sexp.atom "ptype_private";
        privateFlag td.ptype_private;
      ];
      attributes td.ptype_attributes;
    ]

  and extensionConstructor ec =
    Sexp.list [
      Sexp.atom "extension_constructor";
      string ec.pext_name.Asttypes.txt;
      extensionConstructorKind ec.pext_kind;
      attributes ec.pext_attributes;
    ]

  and extensionConstructorKind kind = match kind with
    | Pext_decl (args, optTypExpr) ->
      Sexp.list [
        Sexp.atom "Pext_decl";
        constructorArguments args;
        match optTypExpr with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            coreType typ;
          ]
      ]
  | Pext_rebind longidentLoc ->
    Sexp.list [
      Sexp.atom "Pext_rebind";
      longident longidentLoc.Asttypes.txt;
    ]

  and typeExtension te =
    Sexp.list [
      Sexp.atom "type_extension";
      Sexp.list [
        Sexp.atom "ptyext_path";
        longident te.ptyext_path.Asttypes.txt;
      ];
      Sexp.list [
        Sexp.atom "ptyext_parms";
        Sexp.list (mapEmpty ~f:(fun (typexpr, var) ->
          Sexp.list [
            coreType typexpr;
            variance var;
          ]) te.ptyext_params)
      ];
      Sexp.list [
        Sexp.atom "ptyext_constructors";
        Sexp.list (mapEmpty ~f:extensionConstructor te.ptyext_constructors);
      ];
      Sexp.list [
        Sexp.atom "ptyext_private";
        privateFlag te.ptyext_private;
      ];
      attributes te.ptyext_attributes;
    ]

  and typeKind kind = match kind with
    | Ptype_abstract -> Sexp.atom "Ptype_abstract"
    | Ptype_variant constrDecls ->
      Sexp.list [
        Sexp.atom "Ptype_variant";
        Sexp.list (mapEmpty ~f:constructorDeclaration constrDecls);
      ]
    | Ptype_record lblDecls ->
      Sexp.list [
        Sexp.atom "Ptype_record";
        Sexp.list (mapEmpty ~f:labelDeclaration lblDecls);
      ]
    | Ptype_open -> Sexp.atom "Ptype_open"

  and constructorDeclaration cd =
    Sexp.list [
      Sexp.atom "constructor_declaration";
      string cd.pcd_name.Asttypes.txt;
      Sexp.list [
        Sexp.atom "pcd_args";
        constructorArguments cd.pcd_args;
      ];
      Sexp.list [
        Sexp.atom "pcd_res";
        match cd.pcd_res with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            coreType typ;
          ]
      ];
      attributes cd.pcd_attributes;
    ]

  and constructorArguments args = match args with
    | Pcstr_tuple types ->
      Sexp.list [
        Sexp.atom "Pcstr_tuple";
        Sexp.list (mapEmpty ~f:coreType types)
      ]
    | Pcstr_record lds ->
      Sexp.list [
        Sexp.atom "Pcstr_record";
        Sexp.list (mapEmpty ~f:labelDeclaration lds)
      ]

  and labelDeclaration ld =
    Sexp.list [
      Sexp.atom "label_declaration";
      string ld.pld_name.Asttypes.txt;
      mutableFlag ld.pld_mutable;
      coreType ld.pld_type;
      attributes ld.pld_attributes;
    ]

  and expression expr =
    let desc = match expr.pexp_desc with
    | Pexp_ident longidentLoc ->
      Sexp.list [
        Sexp.atom "Pexp_ident";
        longident longidentLoc.Asttypes.txt;
      ]
    | Pexp_constant c ->
      Sexp.list [
        Sexp.atom "Pexp_constant";
        constant c
      ]
    | Pexp_let (flag, vbs, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_let";
        recFlag flag;
        Sexp.list (mapEmpty ~f:valueBinding vbs);
        expression expr;
      ]
    | Pexp_function cases ->
      Sexp.list [
        Sexp.atom "Pexp_function";
        Sexp.list (mapEmpty ~f:case cases);
      ]
    | Pexp_fun (argLbl, exprOpt, pat, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_fun";
        argLabel argLbl;
        (match exprOpt with
        | None -> Sexp.atom "None"
        | Some expr -> Sexp.list [
            Sexp.atom "Some";
            expression expr;
        ]);
        pattern pat;
        expression expr;
      ]
    | Pexp_apply (expr, args) ->
      Sexp.list [
        Sexp.atom "Pexp_apply";
        expression expr;
        Sexp.list (mapEmpty ~f:(fun (argLbl, expr) -> Sexp.list [
          argLabel argLbl;
          expression expr
        ]) args);
      ]
    | Pexp_match (expr, cases) ->
      Sexp.list [
        Sexp.atom "Pexp_match";
        expression expr;
        Sexp.list (mapEmpty ~f:case cases);
      ]
    | Pexp_try (expr, cases) ->
      Sexp.list [
        Sexp.atom "Pexp_try";
        expression expr;
        Sexp.list (mapEmpty ~f:case cases);
      ]
    | Pexp_tuple exprs ->
      Sexp.list [
        Sexp.atom "Pexp_tuple";
        Sexp.list (mapEmpty ~f:expression exprs);
      ]
    | Pexp_construct (longidentLoc, exprOpt) ->
      Sexp.list [
        Sexp.atom "Pexp_construct";
        longident longidentLoc.Asttypes.txt;
        match exprOpt with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
      ]
    | Pexp_variant (lbl, exprOpt) ->
      Sexp.list [
        Sexp.atom "Pexp_variant";
        string lbl;
        match exprOpt with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
      ]
    | Pexp_record (rows, optExpr) ->
      Sexp.list [
        Sexp.atom "Pexp_record";
        Sexp.list (mapEmpty ~f:(fun (longidentLoc, expr) -> Sexp.list [
          longident longidentLoc.Asttypes.txt;
          expression expr;
        ]) rows);
        (match optExpr with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]);
      ]
    | Pexp_field (expr, longidentLoc) ->
      Sexp.list [
        Sexp.atom "Pexp_field";
        expression expr;
        longident longidentLoc.Asttypes.txt;
      ]
    | Pexp_setfield (expr1, longidentLoc, expr2) ->
      Sexp.list [
        Sexp.atom "Pexp_setfield";
        expression expr1;
        longident longidentLoc.Asttypes.txt;
        expression expr2;
      ]
    | Pexp_array exprs ->
      Sexp.list [
        Sexp.atom "Pexp_array";
        Sexp.list (mapEmpty ~f:expression exprs);
      ]
    | Pexp_ifthenelse (expr1, expr2, optExpr) ->
      Sexp.list [
        Sexp.atom "Pexp_ifthenelse";
        expression expr1;
        expression expr2;
        (match optExpr with
        | None -> Sexp.atom "None"
        | Some expr ->
          Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]);
      ]
    | Pexp_sequence (expr1, expr2) ->
      Sexp.list [
        Sexp.atom "Pexp_sequence";
        expression expr1;
        expression expr2;
      ]
    | Pexp_while (expr1, expr2) ->
      Sexp.list [
        Sexp.atom "Pexp_while";
        expression expr1;
        expression expr2;
      ]
    | Pexp_for (pat, e1, e2, flag, e3) ->
      Sexp.list [
        Sexp.atom "Pexp_for";
        pattern pat;
        expression e1;
        expression e2;
        directionFlag flag;
        expression e3;
      ]
    | Pexp_constraint (expr, typexpr) ->
      Sexp.list [
        Sexp.atom "Pexp_constraint";
        expression expr;
        coreType typexpr;
      ]
    | Pexp_coerce (expr, optTyp, typexpr) ->
      Sexp.list [
        Sexp.atom "Pexp_coerce";
        expression expr;
        (match optTyp with
        | None -> Sexp.atom "None"
        | Some typ -> Sexp.list [
            Sexp.atom "Some";
            coreType typ;
        ]);
        coreType typexpr;
      ]
    | Pexp_send _ ->
      Sexp.list [
        Sexp.atom "Pexp_send";
      ]
    | Pexp_new _ ->
      Sexp.list [
        Sexp.atom "Pexp_new";
      ]
    | Pexp_setinstvar _ ->
      Sexp.list [
        Sexp.atom "Pexp_setinstvar";
      ]
    | Pexp_override _ ->
      Sexp.list [
        Sexp.atom "Pexp_override";
      ]
    | Pexp_letmodule (modName, modExpr, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_letmodule";
        string modName.Asttypes.txt;
        moduleExpression modExpr;
        expression expr;
      ]
    | Pexp_letexception (extConstr, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_letexception";
        extensionConstructor extConstr;
        expression expr;
      ]
    | Pexp_assert expr ->
      Sexp.list [
        Sexp.atom "Pexp_assert";
        expression expr;
      ]
    | Pexp_lazy expr ->
      Sexp.list [
        Sexp.atom "Pexp_lazy";
        expression expr;
      ]
    | Pexp_poly _ ->
      Sexp.list [
        Sexp.atom "Pexp_poly";
      ]
    | Pexp_object _ ->
      Sexp.list [
        Sexp.atom "Pexp_object";
      ]
    | Pexp_newtype (lbl, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_newtype";
        string lbl.Asttypes.txt;
        expression expr;
      ]
    | Pexp_pack modExpr ->
      Sexp.list [
        Sexp.atom "Pexp_pack";
        moduleExpression modExpr;
      ]
    | Pexp_open (flag, longidentLoc, expr) ->
      Sexp.list [
        Sexp.atom "Pexp_open";
        overrideFlag flag;
        longident longidentLoc.Asttypes.txt;
        expression expr;
      ]
    | Pexp_extension ext ->
      Sexp.list [
        Sexp.atom "Pexp_extension";
        extension ext;
      ]
    | Pexp_unreachable -> Sexp.atom "Pexp_unreachable"
    in
    Sexp.list [
      Sexp.atom "expression";
      desc;
    ]

  and case c =
    Sexp.list [
      Sexp.atom "case";
      Sexp.list [
        Sexp.atom "pc_lhs";
        pattern c.pc_lhs;
      ];
      Sexp.list [
        Sexp.atom "pc_guard";
        match c.pc_guard with
        | None -> Sexp.atom "None"
        | Some expr -> Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
      ];
      Sexp.list [
        Sexp.atom "pc_rhs";
        expression c.pc_rhs;
      ]
    ]

  and pattern p =
    let descr = match p.ppat_desc with
    | Ppat_any ->
      Sexp.atom "Ppat_any"
    | Ppat_var var ->
      Sexp.list [
        Sexp.atom "Ppat_var";
        string var.Location.txt;
      ]
    | Ppat_alias (p, alias) ->
      Sexp.list [
        Sexp.atom "Ppat_alias";
        pattern p;
        string alias.txt;
      ]
    | Ppat_constant c ->
      Sexp.list [
        Sexp.atom "Ppat_constant";
        constant c;
      ]
    | Ppat_interval (lo, hi) ->
      Sexp.list [
        Sexp.atom "Ppat_interval";
        constant lo;
        constant hi;
      ]
    | Ppat_tuple (patterns) ->
      Sexp.list [
        Sexp.atom "Ppat_tuple";
        Sexp.list (mapEmpty ~f:pattern patterns);
      ]
    | Ppat_construct (longidentLoc, optPattern) ->
      Sexp.list [
        Sexp.atom "Ppat_construct";
        longident longidentLoc.Location.txt;
        match optPattern with
        | None -> Sexp.atom "None"
        | Some p -> Sexp.list [
            Sexp.atom "some";
            pattern p;
          ]
      ]
    | Ppat_variant (lbl, optPattern) ->
      Sexp.list [
        Sexp.atom "Ppat_variant";
        string lbl;
        match optPattern with
        | None -> Sexp.atom "None"
        | Some p -> Sexp.list [
            Sexp.atom "Some";
            pattern p;
          ]
      ]
    | Ppat_record (rows, flag) ->
      Sexp.list [
        Sexp.atom "Ppat_record";
        closedFlag flag;
        Sexp.list (mapEmpty ~f:(fun (longidentLoc, p) ->
          Sexp.list [
            longident longidentLoc.Location.txt;
            pattern p;
          ]
        ) rows)
      ]
    | Ppat_array patterns ->
      Sexp.list [
        Sexp.atom "Ppat_array";
        Sexp.list (mapEmpty ~f:pattern patterns);
      ]
    | Ppat_or (p1, p2) ->
      Sexp.list [
        Sexp.atom "Ppat_or";
        pattern p1;
        pattern p2;
      ]
    | Ppat_constraint (p, typexpr) ->
      Sexp.list [
        Sexp.atom "Ppat_constraint";
        pattern p;
        coreType typexpr;
      ]
    | Ppat_type longidentLoc ->
      Sexp.list [
        Sexp.atom "Ppat_type";
        longident longidentLoc.Location.txt
      ]
    | Ppat_lazy p ->
      Sexp.list [
        Sexp.atom "Ppat_lazy";
        pattern p;
      ]
    | Ppat_unpack stringLoc ->
      Sexp.list [
        Sexp.atom "Ppat_unpack";
        string stringLoc.Location.txt;
      ]
    | Ppat_exception p ->
      Sexp.list [
        Sexp.atom "Ppat_exception";
        pattern p;
      ]
    | Ppat_extension ext ->
      Sexp.list [
        Sexp.atom "Ppat_extension";
        extension ext;
      ]
    | Ppat_open (longidentLoc, p) ->
      Sexp.list [
        Sexp.atom "Ppat_open";
        longident longidentLoc.Location.txt;
        pattern p;
      ]
    in
    Sexp.list [
      Sexp.atom "pattern";
      descr;
    ]

  and objectField field = match field with
  | Otag (lblLoc, attrs, typexpr) ->
    Sexp.list [
      Sexp.atom "Otag";
      string lblLoc.txt;
      attributes attrs;
      coreType typexpr;
    ]
  | Oinherit typexpr ->
    Sexp.list [
      Sexp.atom "Oinherit";
      coreType typexpr;
    ]

  and rowField field = match field with
    | Rtag (labelLoc, attrs, truth, types) ->
      Sexp.list [
        Sexp.atom "Rtag";
        string labelLoc.txt;
        attributes attrs;
        Sexp.atom (if truth then "true" else "false");
        Sexp.list (mapEmpty ~f:coreType types);
      ]
    | Rinherit typexpr ->
      Sexp.list [
        Sexp.atom "Rinherit";
        coreType typexpr;
      ]

  and packageType (modNameLoc, packageConstraints) =
    Sexp.list [
      Sexp.atom "package_type";
      longident modNameLoc.Asttypes.txt;
      Sexp.list (mapEmpty ~f:(fun (modNameLoc, typexpr) ->
        Sexp.list [
          longident modNameLoc.Asttypes.txt;
          coreType typexpr;
        ]
      ) packageConstraints)
    ]

  and coreType typexpr =
    let desc = match typexpr.ptyp_desc with
      | Ptyp_any -> Sexp.atom "Ptyp_any"
      | Ptyp_var var -> Sexp.list [
          Sexp.atom "Ptyp_var";
          string  var
        ]
      | Ptyp_arrow (argLbl, typ1, typ2) ->
        Sexp.list [
          Sexp.atom "Ptyp_arrow";
          argLabel argLbl;
          coreType typ1;
          coreType typ2;
        ]
      | Ptyp_tuple types ->
        Sexp.list [
          Sexp.atom "Ptyp_tuple";
          Sexp.list (mapEmpty ~f:coreType types);
        ]
      | Ptyp_constr (longidentLoc, types) ->
        Sexp.list [
          Sexp.atom "Ptyp_constr";
          longident longidentLoc.txt;
          Sexp.list (mapEmpty ~f:coreType types);
        ]
      | Ptyp_alias (typexpr, alias) ->
        Sexp.list [
          Sexp.atom "Ptyp_alias";
          coreType typexpr;
          string alias;
        ]
      | Ptyp_object (fields, flag) ->
        Sexp.list [
          Sexp.atom "Ptyp_object";
          closedFlag flag;
          Sexp.list (mapEmpty ~f:objectField fields)
        ]
      | Ptyp_class (longidentLoc, types) ->
        Sexp.list [
          Sexp.atom "Ptyp_class";
          longident longidentLoc.Location.txt;
          Sexp.list (mapEmpty ~f:coreType types)
        ]
      | Ptyp_variant (fields, flag, optLabels) ->
        Sexp.list [
          Sexp.atom "Ptyp_variant";
          Sexp.list (mapEmpty ~f:rowField fields);
          closedFlag flag;
          match optLabels with
          | None -> Sexp.atom "None"
          | Some lbls -> Sexp.list (mapEmpty ~f:string lbls);
        ]
      | Ptyp_poly (lbls, typexpr) ->
        Sexp.list [
          Sexp.atom "Ptyp_poly";
          Sexp.list (mapEmpty ~f:(fun lbl -> string lbl.Asttypes.txt) lbls);
          coreType typexpr;
        ]
      | Ptyp_package (package) ->
        Sexp.list [
          Sexp.atom "Ptyp_package";
          packageType package;
        ]
      | Ptyp_extension (ext) ->
        Sexp.list [
          Sexp.atom "Ptyp_extension";
          extension ext;
        ]
    in
    Sexp.list [
      Sexp.atom "core_type";
      desc;
    ]

  and payload p =
    match p with
    | PStr s ->
      Sexp.list (
        (Sexp.atom "PStr")::(mapEmpty ~f:structureItem s)
      )
    | PSig s ->
      Sexp.list [
        Sexp.atom "PSig";
        signature s;
      ]
    | PTyp ct ->
      Sexp.list [
        Sexp.atom "PTyp";
        coreType ct
      ]
    | PPat (pat, optExpr) ->
      Sexp.list [
        Sexp.atom "PPat";
        pattern pat;
        match optExpr with
        | Some expr -> Sexp.list [
            Sexp.atom "Some";
            expression expr;
          ]
        | None -> Sexp.atom "None";
      ]

  and attribute (stringLoc, p) =
    Sexp.list [
      Sexp.atom "attribute";
      Sexp.atom stringLoc.Asttypes.txt;
      payload p;
    ]

  and extension (stringLoc, p) =
    Sexp.list [
      Sexp.atom "extension";
      Sexp.atom stringLoc.Asttypes.txt;
      payload p;
    ]

  and attributes attrs =
    let sexprs = mapEmpty ~f:attribute attrs in
    Sexp.list ((Sexp.atom "attributes")::sexprs)

  let printEngine = Res_driver.{
    printImplementation = begin fun ~width:_ ~filename:_ ~comments:_ parsetree ->
      parsetree |> structure |> Sexp.toString |> print_string
    end;
    printInterface = begin fun ~width:_ ~filename:_ ~comments:_ parsetree ->
      parsetree |> signature |> Sexp.toString |> print_string
    end;
  }
end

let sexpPrintEngine = SexpAst.printEngine
