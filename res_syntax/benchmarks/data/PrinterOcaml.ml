module Printer = struct
  type printer = {
    src: bytes;
    comments: CommentAst.t;
  }


  (* TODO: should this go inside a ast utility module? *)
  let rec collectPatternsFromListConstruct acc pattern =
    let open Parsetree in
    match pattern.ppat_desc with
    | Ppat_construct(
        {txt = Longident.Lident "::"},
        Some {ppat_desc=Ppat_tuple (pat::rest::[])}
      ) ->
      collectPatternsFromListConstruct (pat::acc) rest
    | _ -> List.rev acc, pattern

  let addParens doc =
    Doc.group (
      Doc.concat [
        Doc.lparen;
        Doc.indent (
          Doc.concat [
            Doc.softLine;
            doc
          ]
        );
        Doc.softLine;
        Doc.rparen;
      ]
    )

  let addBraces doc =
    Doc.group (
      Doc.concat [
        Doc.lbrace;
        doc;
        Doc.rbrace;
      ]
    )

  (* This could be done in one pass by collecting locations as we go? *)
  let interleaveWhitespace ?(forceBreak=false) (rows: (Location.t * Doc.t) list) =
    let rec loop prevLoc acc rows =
      match rows with
      | [] -> Doc.concat (List.rev acc)
      | (loc, doc)::rest ->
        if loc.Location.loc_start.pos_lnum - prevLoc.Location.loc_end.pos_lnum > 1 then
          loop loc (doc::Doc.line::Doc.line::acc) rest
        else
          loop loc (doc::Doc.line::acc) rest
    in
    match rows with
    | [] -> Doc.nil
    | (firstLoc, firstDoc)::rest ->
      (* TODO: perf, reversing the list twice! *)
      let forceBreak = forceBreak || (match List.rev rest with
      | (lastLoc, _)::_ ->
        firstLoc.loc_start.pos_lnum != lastLoc.loc_end.pos_lnum
      | _ -> false)
      in
      Doc.breakableGroup ~forceBreak (
        loop firstLoc [firstDoc] rest
      )

  let printLongident l = match l with
    | Longident.Lident lident -> Doc.text lident
    | Longident.Ldot (lident, txt) as l ->
      let txts = Longident.flatten l in
      Doc.join ~sep:Doc.dot (List.map Doc.text txts)
    | _ -> failwith "unsupported ident"

  (* TODO: better allocation strategy for the buffer *)
  let escapeStringContents s =
    let len = String.length s in
    let b = Buffer.create len in
    for i = 0 to len - 1 do
      let c = String.get s i in
      if c = '\008'  then (
        Buffer.add_char b '\\';
        Buffer.add_char b 'b';
      ) else if c = '\009'  then (
        Buffer.add_char b '\\';
        Buffer.add_char b 't';
      ) else if c = '\010' then (
        Buffer.add_char b '\\';
        Buffer.add_char b 'n';
      ) else if c = '\013' then (
        Buffer.add_char b '\\';
        Buffer.add_char b 'r';
      ) else if c = '\034' then (
        Buffer.add_char b '\\';
        Buffer.add_char b '"';
      ) else if c = '\092' then (
        Buffer.add_char b '\\';
        Buffer.add_char b '\\';
      )else (
        Buffer.add_char b c;
      );
    done;
    Buffer.contents b

  let printConstant c = match c with
    | Parsetree.Pconst_integer (s, _) -> Doc.text s
    | Pconst_string (s, _) -> Doc.text ("\"" ^ (escapeStringContents s) ^ "\"")
    | Pconst_float (s, _) -> Doc.text s
    | Pconst_char c -> Doc.text ("'" ^ (Char.escaped c) ^ "'")

  let rec printStructure (s : Parsetree.structure) =
    interleaveWhitespace  (
      List.map (fun si -> (si.Parsetree.pstr_loc, printStructureItem si)) s
    )

  and printStructureItem (si: Parsetree.structure_item) =
    match si.pstr_desc with
    | Pstr_value(rec_flag, valueBindings) ->
			let recFlag = match rec_flag with
			| Asttypes.Nonrecursive -> Doc.nil
			| Asttypes.Recursive -> Doc.text "rec "
			in
      printValueBindings ~recFlag valueBindings
    | Pstr_type(recFlag, typeDeclarations) ->
      let recFlag = match recFlag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      printTypeDeclarations ~recFlag typeDeclarations
    | Pstr_primitive valueDescription ->
      printValueDescription valueDescription
    | Pstr_eval (expr, attrs) ->
      let needsParens = match expr with
      | {pexp_attributes=[({txt="ns.ternary"},_)]; pexp_desc = Pexp_ifthenelse _} -> false
      | _ when ParsetreeViewer.hasAttributes expr.pexp_attributes -> true
      | _ -> false
      in
      let exprDoc =
        let doc = printExpression expr in
        if needsParens then addParens doc else doc
      in
      Doc.concat [
        printAttributes attrs;
        exprDoc;
      ]
    | Pstr_attribute attr -> Doc.concat [Doc.text "@"; printAttribute attr]
    | Pstr_extension (extension, attrs) -> Doc.concat [
        printAttributes attrs;
        Doc.concat [Doc.text "%";printExtension extension];
      ]
    | Pstr_include includeDeclaration ->
      printIncludeDeclaration includeDeclaration
    | Pstr_open openDescription ->
      printOpenDescription openDescription
    | Pstr_modtype modTypeDecl ->
      printModuleTypeDeclaration modTypeDecl
    | Pstr_module moduleBinding ->
      printModuleBinding ~isRec:false 0 moduleBinding
    | Pstr_recmodule moduleBindings ->
      Doc.join ~sep:Doc.line (List.mapi (fun i mb ->
        printModuleBinding ~isRec:true i mb
      ) moduleBindings)
    | Pstr_exception extensionConstructor ->
      printExceptionDef extensionConstructor;
    | Pstr_typext typeExtension ->
      printTypeExtension typeExtension
    | Pstr_class _ | Pstr_class_type _ -> Doc.nil

  and printTypeExtension (te : Parsetree.type_extension) =
    let prefix = Doc.text "type " in
    let name = printLongident te.ptyext_path.txt in
    let typeParams = match te.ptyext_params with
    | [] -> Doc.nil
    | typeParams -> Doc.group (
        Doc.concat [
          Doc.lessThan;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printTypeParam typeParams
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.greaterThan;
        ]
      )
    in
    let extensionConstructors =
      let ecs = te.ptyext_constructors in
      let forceBreak =
        match (ecs, List.rev ecs) with
        | (first::_, last::_) ->
          first.pext_loc.loc_start.pos_lnum > te.ptyext_path.loc.loc_end.pos_lnum ||
          first.pext_loc.loc_start.pos_lnum < last.pext_loc.loc_end.pos_lnum
        | _ -> false
        in
      let privateFlag = match te.ptyext_private with
      | Asttypes.Private -> Doc.concat [
          Doc.text "private";
          Doc.line;
        ]
      | Public -> Doc.nil
      in
      Doc.breakableGroup ~forceBreak (
        Doc.indent (
          Doc.concat [
            Doc.line;
            privateFlag;
            Doc.join ~sep:Doc.line (
              List.mapi printExtensionConstructor ecs
            )
          ]
        )
      )
    in
    Doc.group (
      Doc.concat [
        printAttributes ~loc: te.ptyext_path.loc te.ptyext_attributes;
        prefix;
        name;
        typeParams;
        Doc.text " +=";
        extensionConstructors;
      ]
    )

  and printModuleBinding ~isRec i moduleBinding =
    let prefix = if i = 0 then
      Doc.concat [
        Doc.text "module ";
        if isRec then Doc.text "rec " else Doc.nil;
      ]
    else
      Doc.text "and "
    in
    let (modExprDoc, modConstraintDoc) =
      match moduleBinding.pmb_expr with
      | {pmod_desc = Pmod_constraint (modExpr, modType)} ->
        (
          printModExpr modExpr,
          Doc.concat [
            Doc.text ": ";
            printModType modType
          ]
        )
      | modExpr ->
        (printModExpr modExpr, Doc.nil)
    in
    Doc.concat [
      printAttributes ~loc:moduleBinding.pmb_name.loc moduleBinding.pmb_attributes;
      prefix;
      Doc.text moduleBinding.pmb_name.Location.txt;
      modConstraintDoc;
      Doc.text " = ";
      modExprDoc;
    ]

  and printModuleTypeDeclaration (modTypeDecl : Parsetree.module_type_declaration) =
    Doc.concat [
      printAttributes modTypeDecl.pmtd_attributes;
      Doc.text "module type ";
      Doc.text modTypeDecl.pmtd_name.txt;
      (match modTypeDecl.pmtd_type with
      | None -> Doc.nil
      | Some modType -> Doc.concat [
          Doc.text " = ";
          printModType modType;
        ]);
    ]

  and printModType modType =
    let modTypeDoc = match modType.pmty_desc with
    | Parsetree.Pmty_ident {txt = longident; loc} ->
      Doc.concat [
        printAttributes ~loc modType.pmty_attributes;
        printLongident longident
      ]
    | Pmty_signature signature ->
      let signatureDoc = Doc.breakableGroup ~forceBreak:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.line;
              printSignature signature;
            ]
          );
          Doc.line;
          Doc.rbrace;
        ]
      ) in
      Doc.concat [
        printAttributes modType.pmty_attributes;
        signatureDoc
      ]
    | Pmty_functor _ ->
      let (parameters, returnType) = ParsetreeViewer.functorType modType in
      let parametersDoc = match parameters with
      | [] -> Doc.nil
      | [attrs, {Location.txt = "_"}, Some modType] ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map printAttribute attrs);
          Doc.line;
        ] in
        Doc.concat [
          attrs;
          printModType modType
        ]
      | params ->
        Doc.group (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map (fun (attrs, lbl, modType) ->
                    let attrs = match attrs with
                    | [] -> Doc.nil
                    | attrs -> Doc.concat [
                      Doc.join ~sep:Doc.line (List.map printAttribute attrs);
                      Doc.line;
                    ] in
                    Doc.concat [
                      attrs;
                      if lbl.Location.txt = "_" then Doc.nil else Doc.text lbl.txt;
                      (match modType with
                      | None -> Doc.nil
                      | Some modType -> Doc.concat [
                        if lbl.txt = "_" then Doc.nil else Doc.text ": ";
                        printModType modType;
                      ]);
                    ]
                  ) params
                );
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
            Doc.rparen;
          ]
        )
      in
      let returnDoc =
        let doc = printModType returnType in
        if Parens.modTypeFunctorReturn returnType then addParens doc else doc
      in
      Doc.group (
        Doc.concat [
          parametersDoc;
          Doc.group (
            Doc.concat [
            Doc.text " =>";
            Doc.line;
            returnDoc;
            ]
          )
        ]
      )
    | Pmty_typeof modExpr -> Doc.concat [
        Doc.text "module type of ";
        printModExpr modExpr;
      ]
    | Pmty_extension extension -> printExtension extension
    | Pmty_alias {txt = longident} -> Doc.concat [
        Doc.text "module ";
        printLongident longident;
      ]
    | Pmty_with (modType, withConstraints) ->
      let operand =
        let doc = printModType modType in
        if Parens.modTypeWithOperand modType then addParens doc else doc
      in
      Doc.group (
        Doc.concat [
          operand;
          Doc.indent (
            Doc.concat [
              Doc.line;
              printWithConstraints withConstraints;
            ]
          )
        ]
      )
    in
    let attrsAlreadyPrinted = match modType.pmty_desc with
    | Pmty_functor _ | Pmty_signature _ | Pmty_ident _ -> true
    | _ -> false in
    Doc.concat [
      if attrsAlreadyPrinted then Doc.nil else printAttributes modType.pmty_attributes;
      modTypeDoc;
    ]

  and printWithConstraints withConstraints =
    let rows =List.mapi (fun i withConstraint  ->
      Doc.group (
        Doc.concat [
          if i == 0 then Doc.text "with " else Doc.text "and ";
          printWithConstraint withConstraint;
        ]
      )
    ) withConstraints
    in
    Doc.join ~sep:Doc.line rows

  and printWithConstraint (withConstraint : Parsetree.with_constraint) =
    match withConstraint with
    (* with type X.t = ... *)
    | Pwith_type ({txt = longident}, typeDeclaration) ->
      Doc.group (printTypeDeclaration
        ~name:(printLongident longident)
        ~equalSign:"="
        ~recFlag:Doc.nil
        0
        typeDeclaration)
    (* with module X.Y = Z *)
    | Pwith_module ({txt = longident1}, {txt = longident2}) ->
        Doc.concat [
          Doc.text "module ";
          printLongident longident1;
          Doc.text " =";
          Doc.indent (
            Doc.concat [
              Doc.line;
              printLongident longident2;
            ]
          )
        ]
    (* with type X.t := ..., same format as [Pwith_type] *)
    | Pwith_typesubst ({txt = longident}, typeDeclaration) ->
      Doc.group(printTypeDeclaration
        ~name:(printLongident longident)
        ~equalSign:":="
        ~recFlag:Doc.nil
        0
        typeDeclaration)
    | Pwith_modsubst ({txt = longident1}, {txt = longident2}) ->
      Doc.concat [
        Doc.text "module ";
        printLongident longident1;
        Doc.text " :=";
        Doc.indent (
          Doc.concat [
            Doc.line;
            printLongident longident2;
          ]
        )
      ]

  and printSignature signature =
    interleaveWhitespace  (
      List.map (fun si -> (si.Parsetree.psig_loc, printSignatureItem si)) signature
    )

  and printSignatureItem (si : Parsetree.signature_item) =
    match si.psig_desc with
    | Parsetree.Psig_value valueDescription ->
      printValueDescription valueDescription
    | Psig_type (recFlag, typeDeclarations) ->
      let recFlag = match recFlag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      printTypeDeclarations ~recFlag typeDeclarations
    | Psig_typext typeExtension ->
      printTypeExtension typeExtension
    | Psig_exception extensionConstructor ->
      printExceptionDef extensionConstructor
    | Psig_module moduleDeclaration ->
      printModuleDeclaration moduleDeclaration
    | Psig_recmodule moduleDeclarations ->
      printRecModuleDeclarations moduleDeclarations
    | Psig_modtype modTypeDecl ->
      printModuleTypeDeclaration modTypeDecl
    | Psig_open openDescription ->
      printOpenDescription openDescription
    | Psig_include includeDescription ->
      printIncludeDescription includeDescription
    | Psig_attribute attr -> Doc.concat [Doc.text "@"; printAttribute attr]
    | Psig_extension (extension, attrs) -> Doc.concat [
        printAttributes attrs;
        Doc.concat [Doc.text "%";printExtension extension];
      ]
    | Psig_class _ | Psig_class_type _ -> Doc.nil

  and printRecModuleDeclarations moduleDeclarations =
    Doc.group (
      Doc.join ~sep:Doc.line (
        List.mapi (fun i (md: Parsetree.module_declaration) ->
          let body = match md.pmd_type.pmty_desc with
          | Parsetree.Pmty_alias {txt = longident } ->
            Doc.concat [Doc.text " = "; printLongident longident]
          | _ ->
            let needsParens = match md.pmd_type.pmty_desc with
            | Pmty_with _ -> true
            | _ -> false
            in
            let modTypeDoc =
              let doc = printModType md.pmd_type in
              if needsParens then addParens doc else doc
            in
            Doc.concat [Doc.text ": "; modTypeDoc]
          in
          let prefix = if i < 1 then "module rec " else "and " in
          Doc.concat [
            printAttributes ~loc:md.pmd_name.loc md.pmd_attributes;
            Doc.text prefix;
            Doc.text md.pmd_name.txt;
            body
          ]
        ) moduleDeclarations
      )
    )

  and printModuleDeclaration (md: Parsetree.module_declaration) =
    let body = match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias {txt = longident } ->
      Doc.concat [Doc.text " = "; printLongident longident]
    | _ -> Doc.concat [Doc.text ": "; printModType md.pmd_type]
    in
    Doc.concat [
      printAttributes ~loc:md.pmd_name.loc md.pmd_attributes;
      Doc.text "module ";
      Doc.text md.pmd_name.txt;
      body
    ]

  and printOpenDescription (openDescription : Parsetree.open_description) =
    Doc.concat [
      printAttributes openDescription.popen_attributes;
      Doc.text "open";
      (match openDescription.popen_override with
      | Asttypes.Fresh -> Doc.space
      | Asttypes.Override -> Doc.text "! ");
      printLongident openDescription.popen_lid.txt
    ]

  and printIncludeDescription (includeDescription: Parsetree.include_description) =
    Doc.concat [
      printAttributes includeDescription.pincl_attributes;
      Doc.text "include ";
      printModType includeDescription.pincl_mod;
    ]

  and printIncludeDeclaration (includeDeclaration : Parsetree.include_declaration) =
    Doc.concat [
      printAttributes includeDeclaration.pincl_attributes;
      Doc.text "include ";
      printModExpr includeDeclaration.pincl_mod;
    ]


  and printValueBindings ~recFlag (vbs: Parsetree.value_binding list) =
    let rows = List.mapi (fun i vb ->
      let doc = printValueBinding ~recFlag i vb in
      (vb.Parsetree.pvb_loc, doc)
    ) vbs
    in
    interleaveWhitespace rows

  (*
   * type value_description = {
   *   pval_name : string Asttypes.loc;
   *   pval_type : Parsetree.core_type;
   *   pval_prim : string list;
   *   pval_attributes : Parsetree.attributes;
   *   pval_loc : Location.t;
   * }
   *)
  and printValueDescription valueDescription =
    let isExternal =
      match valueDescription.pval_prim with | [] -> false | _ -> true
    in
    Doc.group (
      Doc.concat [
        Doc.text (if isExternal then "external " else "let ");
        Doc.text valueDescription.pval_name.txt;
        Doc.text ": ";
        printTypExpr valueDescription.pval_type;
        if isExternal then
          Doc.group (
            Doc.concat [
              Doc.text " =";
              Doc.indent(
                Doc.concat [
                  Doc.line;
                  Doc.join ~sep:Doc.line (
                    List.map(fun s -> Doc.concat [
                      Doc.text "\"";
                      Doc.text s;
                      Doc.text "\"";
                    ])
                    valueDescription.pval_prim
                  );
                ]
              )
            ]
          )
        else Doc.nil
      ]
    )

  and printTypeDeclarations ~recFlag typeDeclarations =
    let rows = List.mapi (fun i td ->
      let doc = printTypeDeclaration
        ~name:(Doc.text td.Parsetree.ptype_name.txt)
        ~equalSign:"="
        ~recFlag
        i td
      in
      (td.Parsetree.ptype_loc, doc)
    ) typeDeclarations in
    interleaveWhitespace rows

  (*
   * type_declaration = {
   *    ptype_name: string loc;
   *    ptype_params: (core_type * variance) list;
   *          (* ('a1,...'an) t; None represents  _*)
   *    ptype_cstrs: (core_type * core_type * Location.t) list;
   *          (* ... constraint T1=T1'  ... constraint Tn=Tn' *)
   *    ptype_kind: type_kind;
   *    ptype_private: private_flag;   (* = private ... *)
   *    ptype_manifest: core_type option;  (* = T *)
   *    ptype_attributes: attributes;   (* ... [@@id1] [@@id2] *)
   *    ptype_loc: Location.t;
   * }
   *
   *
   *  type t                     (abstract, no manifest)
   *  type t = T0                (abstract, manifest=T0)
   *  type t = C of T | ...      (variant,  no manifest)
   *  type t = T0 = C of T | ... (variant,  manifest=T0)
   *  type t = {l: T; ...}       (record,   no manifest)
   *  type t = T0 = {l : T; ...} (record,   manifest=T0)
   *  type t = ..                (open,     no manifest)
   *
   *
   * and type_kind =
   *  | Ptype_abstract
   *  | Ptype_variant of constructor_declaration list
   *        (* Invariant: non-empty list *)
   *  | Ptype_record of label_declaration list
   *        (* Invariant: non-empty list *)
   *  | Ptype_open
   *)
  and printTypeDeclaration ~name ~equalSign ~recFlag i (td: Parsetree.type_declaration) =
    let attrs = printAttributes ~loc:td.ptype_loc td.ptype_attributes in
    let prefix = if i > 0 then
      Doc.text "and "
    else
      Doc.concat [Doc.text "type "; recFlag]
    in
    let typeName = name in
    let typeParams = match td.ptype_params with
    | [] -> Doc.nil
    | typeParams -> Doc.group (
        Doc.concat [
          Doc.lessThan;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printTypeParam typeParams
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.greaterThan;
        ]
      )
    in
    let manifestAndKind = match td.ptype_kind with
    | Ptype_abstract ->
      begin match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) ->
        Doc.concat [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printPrivateFlag td.ptype_private;
          printTypExpr typ;
        ]
      end
    | Ptype_open -> Doc.concat [
        Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
        printPrivateFlag td.ptype_private;
        Doc.text "..";
      ]
    | Ptype_record(lds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printTypExpr typ;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
        printPrivateFlag td.ptype_private;
        printRecordDeclaration lds;
      ]
    | Ptype_variant(cds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equalSign; Doc.space];
          printTypExpr typ;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equalSign];
        printConstructorDeclarations ~privateFlag:td.ptype_private cds;
      ]
    in
    let constraints = printTypeDefinitionConstraints td.ptype_cstrs in
    Doc.group (
      Doc.concat [
        attrs;
        prefix;
        typeName;
        typeParams;
        manifestAndKind;
        constraints;
      ]
    )

  and printTypeDefinitionConstraints cstrs =
    match cstrs with
    | [] -> Doc.nil
    | cstrs -> Doc.indent (
        Doc.group (
          Doc.concat [
            Doc.line;
            Doc.group(
              Doc.join ~sep:Doc.line (
                List.map printTypeDefinitionConstraint cstrs
              )
            )
          ]
        )
      )

  and printTypeDefinitionConstraint ((typ1, typ2, _loc ): Parsetree.core_type * Parsetree.core_type * Location.t) =
    Doc.concat [
      Doc.text "constraint ";
      printTypExpr typ1;
      Doc.text " = ";
      printTypExpr typ2;
    ]

  and printPrivateFlag (flag : Asttypes.private_flag) = match flag with
    | Private -> Doc.text "private "
    | Public -> Doc.nil

  and printTypeParam (param : (Parsetree.core_type * Asttypes.variance)) =
    let (typ, variance) = param in
    let printedVariance = match variance with
    | Covariant -> Doc.text "+"
    | Contravariant -> Doc.text "-"
    | Invariant -> Doc.nil
    in
    Doc.concat [
      printedVariance;
      printTypExpr typ
    ]

  and printRecordDeclaration (lds: Parsetree.label_declaration list) =
    let forceBreak = match (lds, List.rev lds) with
    | (first::_, last::_) ->
       first.pld_loc.loc_start.pos_lnum < last.pld_loc.loc_end.pos_lnum
    | _ -> false
    in
    Doc.breakableGroup ~forceBreak (
      Doc.concat [
        Doc.lbrace;
        Doc.indent (
          Doc.concat [
            Doc.softLine;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
              (List.map printLabelDeclaration lds)
          ]
        );
        Doc.trailingComma;
        Doc.softLine;
        Doc.rbrace;
      ]
    )

  and printConstructorDeclarations ~privateFlag (cds: Parsetree.constructor_declaration list) =
    let forceBreak = match (cds, List.rev cds) with
    | (first::_, last::_) ->
       first.pcd_loc.loc_start.pos_lnum < last.pcd_loc.loc_end.pos_lnum
    | _ -> false
    in
    let privateFlag = match privateFlag with
    | Asttypes.Private -> Doc.concat [
        Doc.text "private";
        Doc.line;
      ]
    | Public -> Doc.nil
    in
    Doc.breakableGroup ~forceBreak (
      Doc.indent (
        Doc.concat [
          Doc.line;
          privateFlag;
          Doc.join ~sep:Doc.line (
            List.mapi printConstructorDeclaration cds
          )
        ]
      )
    )

  (*
   * {
   *  pcd_name: string loc;
   *  pcd_args: constructor_arguments;
   *  pcd_res: core_type option;
   *  pcd_loc: Location.t;
   *  pcd_attributes: attributes; (* C of ... [@id1] [@id2] *)
   * }
   *)
  and printConstructorDeclaration i (cd : Parsetree.constructor_declaration) =
    let attrs = printAttributes cd.pcd_attributes in
    let bar = if i > 0 then Doc.text "| "
      else Doc.ifBreaks (Doc.text "| ") Doc.nil
    in
    let constrName = Doc.text cd.pcd_name.txt in
    let constrArgs = printConstructorArguments cd.pcd_args in
    let gadt = match cd.pcd_res with
    | None -> Doc.nil
    | Some(typ) -> Doc.indent (
        Doc.concat [
          Doc.text ": ";
          printTypExpr typ;
        ]
      )
    in
    Doc.concat [
      bar;
      Doc.group (
        Doc.concat [
          attrs; (* TODO: fix parsing of attributes, so when can print them above the bar? *)
          constrName;
          constrArgs;
          gadt;
        ]
      )
    ]

  and printConstructorArguments (cdArgs : Parsetree.constructor_arguments) =
    match cdArgs with
    | Pcstr_tuple [] -> Doc.nil
    | Pcstr_tuple types -> Doc.group (
        Doc.indent (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map printTypExpr types
                )
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
            Doc.rparen;
          ]
        )
      )
    | Pcstr_record lds ->
      Doc.indent (
        Doc.concat [
          Doc.lparen;
          (* manually inline the printRecordDeclaration, gives better layout *)
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
                (List.map printLabelDeclaration lds)
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rbrace;
          Doc.rparen;
        ]
      )


  and printLabelDeclaration (ld : Parsetree.label_declaration) =
    let attrs = printAttributes ~loc:ld.pld_name.loc ld.pld_attributes in
    let mutableFlag = match ld.pld_mutable with
    | Mutable -> Doc.text "mutable "
    | Immutable -> Doc.nil
    in
    let name = Doc.text ld.pld_name.txt in
    Doc.group (
      Doc.concat [
        attrs;
        mutableFlag;
        name;
        Doc.text ": ";
        printTypExpr ld.pld_type;
      ]
    )

  and printTypExpr (typExpr : Parsetree.core_type) =
    let renderedType = match typExpr.ptyp_desc with
    | Ptyp_any -> Doc.text "_"
    | Ptyp_var var -> Doc.text ("'" ^ var)
    | Ptyp_extension(extension) ->
      printExtension extension
    | Ptyp_alias(typ, alias) ->
      let typ =
        (* Technically type t = (string, float) => unit as 'x, doesn't require
         * parens around the arrow expression. This is very confusing though.
         * Is the "as" part of "unit" or "(string, float) => unit". By printing
         * parens we guide the user towards its meaning.*)
        let needsParens = match typ.ptyp_desc with
        | Ptyp_arrow _ -> true
        | _ -> false
        in
        let doc = printTypExpr typ in
        if needsParens then
          Doc.concat [Doc.lparen; doc; Doc.rparen]
        else
          doc
      in
      Doc.concat [typ; Doc.text " as "; Doc.text ("'" ^ alias)]
    | Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")}, [typ]) ->
      let bsObject = printTypExpr typ in
      begin match typExpr.ptyp_attributes with
      | [] -> bsObject
      | attrs ->
        Doc.concat [
          Doc.group (
            Doc.join ~sep:Doc.line (List.map printAttribute attrs)
          );
          Doc.space;
          printTypExpr typ;
        ]
      end
    | Ptyp_constr(longidentLoc, [{ ptyp_desc = Parsetree.Ptyp_tuple tuple }]) ->
      let constrName = printLongident longidentLoc.txt in
      Doc.group(
        Doc.concat([
          constrName;
          Doc.lessThan;
          printTupleType ~inline:true tuple;
          Doc.greaterThan;
        ])
      )
    | Ptyp_constr(longidentLoc, constrArgs) ->
      let constrName = printLongident longidentLoc.txt in
      begin match constrArgs with
      | [] -> constrName
      | [{
          Parsetree.ptyp_desc =
            Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")},
          [{ptyp_desc = Ptyp_object (fields, openFlag)}])
        }] ->
        Doc.concat([
          constrName;
          Doc.lessThan;
          printBsObjectSugar ~inline:true fields openFlag;
          Doc.greaterThan;
        ])
      | args -> Doc.group(
        Doc.concat([
          constrName;
          Doc.lessThan;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printTypExpr constrArgs
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.greaterThan;
        ])
      )
      end
    | Ptyp_arrow _ ->
      let (attrsBefore, args, returnType) = ParsetreeViewer.arrowType typExpr in
      let returnTypeNeedsParens = match returnType.ptyp_desc with
      | Ptyp_alias _ -> true
      | _ -> false
      in
      let returnDoc =
        let doc = printTypExpr returnType in
        if returnTypeNeedsParens then
          Doc.concat [Doc.lparen; doc; Doc.rparen]
        else doc
      in
      let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute attrsBefore in
      begin match args with
      | [] -> Doc.nil
      | [([], Nolabel, n)] when not isUncurried ->
          let hasAttrsBefore = not (attrs = []) in
          let attrs = if hasAttrsBefore then
            Doc.concat [
              Doc.join ~sep:Doc.line (List.map printAttribute attrsBefore);
              Doc.space;
            ]
          else Doc.nil
          in
          Doc.group (
            Doc.concat [
              Doc.group attrs;
              Doc.group (
                if hasAttrsBefore then
                  Doc.concat [
                    Doc.lparen;
                    Doc.indent (
                      Doc.concat [
                        Doc.softLine;
                        printTypExpr n;
                        Doc.text " => ";
                        returnDoc;
                      ]
                    );
                    Doc.softLine;
                    Doc.rparen
                  ]
                else
                Doc.concat [
                  printTypExpr n;
                  Doc.text " => ";
                  returnDoc;
                ]
              )
            ]
          )
      | args ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
            Doc.join ~sep:Doc.line (List.map printAttribute attrs);
            Doc.space;
          ]
        in
        let renderedArgs = Doc.concat [
          attrs;
          Doc.text "(";
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              if isUncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printTypeParameter args
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.text ")";
        ] in
        Doc.group (
          Doc.concat [
            renderedArgs;
            Doc.text " => ";
            returnDoc;
          ]
        )
      end
    | Ptyp_tuple types -> printTupleType ~inline:false types
    | Ptyp_object (fields, openFlag) ->
      printBsObjectSugar ~inline:false fields openFlag
    | Ptyp_poly(stringLocs, typ) ->
      Doc.concat [
        Doc.join ~sep:Doc.space (List.map (fun {Location.txt} ->
          Doc.text ("'" ^ txt)) stringLocs);
        Doc.dot;
        Doc.space;
        printTypExpr typ
      ]
    | Ptyp_package packageType ->
      printPackageType ~printModuleKeywordAndParens:true packageType
    | Ptyp_class _ -> failwith "classes are not supported in types"
    | Ptyp_variant _ -> failwith "Polymorphic variants currently not supported"
    in
    let shouldPrintItsOwnAttributes = match typExpr.ptyp_desc with
    | Ptyp_arrow _ (* es6 arrow types print their own attributes *)
    | Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")}, _) -> true
    | _ -> false
    in
    begin match typExpr.ptyp_attributes with
    | _::_ as attrs when not shouldPrintItsOwnAttributes ->
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          renderedType;
        ]
      )
    | _ -> renderedType
    end

  and printBsObjectSugar ~inline fields openFlag =
    let flag = match openFlag with
    | Asttypes.Closed -> Doc.nil
    | Open -> Doc.dotdot
    in
    let doc = Doc.concat [
      Doc.lbrace;
      flag;
      Doc.indent (
        Doc.concat [
          Doc.softLine;
          Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
            List.map printObjectField fields
          )
        ]
      );
      Doc.trailingComma;
      Doc.softLine;
      Doc.rbrace;
    ] in
    if inline then doc else Doc.group doc


  and printTupleType ~inline (types: Parsetree.core_type list) =
    let tuple = Doc.concat([
      Doc.text "/";
      Doc.indent (
        Doc.concat([
          Doc.softLine;
          Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
            List.map printTypExpr types
          )
        ])
      );
      (* Doc.trailingComma; *) (* Trailing comma not supported in tuples right now‚Ä¶¬†*)
      Doc.softLine;
      Doc.text "/";
    ])
    in
    if inline == false then Doc.group(tuple) else tuple

  and printObjectField (field : Parsetree.object_field) =
    match field with
    | Otag (labelLoc, attrs, typ) ->
      Doc.concat [
        Doc.text ("\"" ^ labelLoc.txt ^ "\"");
        Doc.text ": ";
        printTypExpr typ;
      ]
    | _ -> Doc.nil

  (* es6 arrow type arg
   * type t = (~foo: string, ~bar: float=?, unit) => unit
   * i.e. ~foo: string, ~bar: float *)
  and printTypeParameter (attrs, lbl, typ)  =
    let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute attrs in
    let uncurried = if isUncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map printAttribute attrs);
      Doc.line;
    ] in
    let label = match lbl with
    | Asttypes.Nolabel -> Doc.nil
    | Labelled lbl -> Doc.text ("~" ^ lbl ^ ": ")
    | Optional lbl -> Doc.text ("~" ^ lbl ^ ": ")
    in
    let optionalIndicator = match lbl with
    | Asttypes.Nolabel
    | Labelled _ -> Doc.nil
    | Optional lbl -> Doc.text "=?"
    in
    Doc.group (
      Doc.concat [
        uncurried;
        attrs;
        label;
        printTypExpr typ;
        optionalIndicator;
      ]
    )


  (*
   * {
   *   pvb_pat: pattern;
   *   pvb_expr: expression;
   *   pvb_attributes: attributes;
   *   pvb_loc: Location.t;
   * }
   *)
  and printValueBinding ~recFlag i vb =
    let isGhost = ParsetreeViewer.isGhostUnitBinding i vb in
		let header = if isGhost then Doc.nil else
			if i == 0 then Doc.concat [Doc.text "let "; recFlag]
			else Doc.text "and "
		in
    let printedExpr =
      let exprDoc = printExpression vb.pvb_expr in
      let needsParens = match vb.pvb_expr.pexp_desc with
      | Pexp_constraint(
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        ) -> false
      | Pexp_constraint _ -> true
      | _ -> false
      in
      if needsParens then addParens exprDoc else exprDoc
    in
		if isGhost then
			printedExpr
		else
      let shouldIndent =
        ParsetreeViewer.isBinaryExpression vb.pvb_expr ||
        (match vb.pvb_expr with
        | {
            pexp_attributes = [({Location.txt="ns.ternary"}, _)];
            pexp_desc = Pexp_ifthenelse (ifExpr, _, _)
          }  ->
          ParsetreeViewer.isBinaryExpression ifExpr || ParsetreeViewer.hasAttributes ifExpr.pexp_attributes
      | { pexp_desc = Pexp_newtype _} -> false
      | e ->
          ParsetreeViewer.hasAttributes e.pexp_attributes ||
          ParsetreeViewer.isArrayAccess e
        )
      in
			Doc.concat [
        printAttributes ~loc:vb.pvb_loc vb.pvb_attributes;
				header;
				printPattern vb.pvb_pat;
				Doc.text " =";
        if shouldIndent then
          Doc.indent (
            Doc.concat [
              Doc.line;
              printedExpr;
            ]
          )
        else
          Doc.concat [
            Doc.space;
            printedExpr;
          ]
      ]

  and printPackageType ~printModuleKeywordAndParens (packageType: Parsetree.package_type) =
    let doc = match packageType with
    | (longidentLoc, []) -> Doc.group(
        Doc.concat [
          printLongident longidentLoc.txt;
        ]
      )
    | (longidentLoc, packageConstraints) -> Doc.group(
        Doc.concat [
          printLongident longidentLoc.txt;
          printPackageConstraints packageConstraints;
          Doc.softLine;
        ]
      )
    in
    if printModuleKeywordAndParens then
      Doc.concat[
        Doc.text "module(";
        doc;
        Doc.rparen
      ]
    else
      doc




  and printPackageConstraints packageConstraints  =
    Doc.concat [
      Doc.text " with";
      Doc.indent (
        Doc.concat [
          Doc.line;
          Doc.join ~sep:Doc.line (
            List.mapi printPackageconstraint packageConstraints
          )
        ]
      )
    ]

  and printPackageconstraint i (longidentLoc, typ) =
    let prefix = if i == 0 then Doc.text "type " else Doc.text "and type " in
    Doc.concat [
      prefix;
      printLongident longidentLoc.Location.txt;
      Doc.text " = ";
      printTypExpr typ
    ]

  and printExtension (stringLoc, payload) =
    let extName = Doc.text ("%" ^ stringLoc.Location.txt) in
    match payload with
    | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
      let exprDoc = printExpression expr in
      let needsParens = match attrs with | [] -> false | _ -> true in
      Doc.group (
        Doc.concat [
          extName;
          addParens (
            Doc.concat [
              printAttributes attrs;
              if needsParens then addParens exprDoc else exprDoc;
            ]
          )
        ]
      )
    | _ -> extName

  and printPattern (p : Parsetree.pattern) =
    let patternWithoutAttributes = match p.ppat_desc with
    | Ppat_any -> Doc.text "_"
    | Ppat_var stringLoc -> Doc.text (stringLoc.txt)
    | Ppat_constant c -> printConstant c
    | Ppat_tuple patterns ->
      Doc.group(
        Doc.concat([
          Doc.text "/";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map printPattern patterns)
            ])
          );
          (* Doc.ifBreaks (Doc.text ",") Doc.nil; *)
          Doc.softLine;
          Doc.text "/";
        ])
      )
    | Ppat_array patterns ->
      Doc.group(
        Doc.concat([
          Doc.text "[";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map printPattern patterns)
            ])
          );
          Doc.ifBreaks (Doc.text ",") Doc.nil;
          Doc.softLine;
          Doc.text "]";
        ])
      )
    | Ppat_construct({txt = Longident.Lident "[]"}, _) ->
        Doc.text "list()"
    | Ppat_construct({txt = Longident.Lident "::"}, _) ->
      let (patterns, tail) = collectPatternsFromListConstruct [] p in
      let shouldHug = match (patterns, tail) with
      | ([pat],
        {ppat_desc = Ppat_construct({txt = Longident.Lident "[]"}, _)}) when ParsetreeViewer.isHuggablePattern pat -> true
      | _ -> false
      in
      let children = Doc.concat([
        if shouldHug then Doc.nil else Doc.softLine;
        Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
          (List.map printPattern patterns);
        begin match tail.Parsetree.ppat_desc with
        | Ppat_construct({txt = Longident.Lident "[]"}, _) -> Doc.nil
        | _ -> Doc.concat([Doc.text ","; Doc.line; Doc.text "..."; printPattern tail])
        end;
      ]) in
      Doc.group(
        Doc.concat([
          Doc.text "list(";
          if shouldHug then children else Doc.concat [
            Doc.indent children;
            Doc.ifBreaks (Doc.text ",") Doc.nil;
            Doc.softLine;
          ];
          Doc.text ")";
        ])
      )
    | Ppat_construct(constrName, constructorArgs) ->
      let constrName = printLongident constrName.txt in
      begin match constructorArgs with
      | None -> constrName
      | Some(args) ->
        let args = match args.ppat_desc with
        | Ppat_construct({txt = Longident.Lident "()"}, None) -> [Doc.nil]
        | Ppat_tuple(patterns) -> List.map printPattern patterns
        | _ -> [printPattern args]
        in
        Doc.group(
          Doc.concat([
            constrName;
            Doc.text "(";
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  args
              ]
            );
            Doc.ifBreaks (Doc.text ",") Doc.nil;
            Doc.softLine;
            Doc.text ")";
          ])
        )
      end
    | Ppat_record(rows, openFlag) ->
        Doc.group(
          Doc.concat([
            Doc.text "{";
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map printPatternRecordRow rows);
                begin match openFlag with
                | Open -> Doc.concat [Doc.text ","; Doc.line; Doc.text "_"]
                | Closed -> Doc.nil
                end;
              ]
            );
            Doc.ifBreaks (Doc.text ",") Doc.nil;
            Doc.softLine;
            Doc.text "}";
          ])
        )

    | Ppat_exception p ->
        let needsParens = match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
        in
        let pat =
          let p = printPattern p in
          if needsParens then
            Doc.concat [Doc.text "("; p; Doc.text ")"]
          else
            p
        in
        Doc.group (
          Doc.concat [ Doc.text "exception"; Doc.line; pat ]
        )
    | Ppat_or (p1, p2) ->
      let p1 =
        let p = printPattern p1 in
        match p1.ppat_desc with
        | Ppat_or (_, _) -> Doc.concat [Doc.text "("; p; Doc.text ")"]
        | _ -> p
      in
      let p2 =
        let p = printPattern p2 in
        match p2.ppat_desc with
        | Ppat_or (_, _) -> Doc.concat [Doc.text "("; p; Doc.text ")"]
        | _ -> p
      in
      Doc.group(
        Doc.concat([p1; Doc.line; Doc.text "| "; p2])
      )
    | Ppat_extension ext ->
      printExtension ext
    | Ppat_lazy p ->
      let needsParens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let pat =
        let p = printPattern p in
        if needsParens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat [Doc.text "lazy "; pat]
    | Ppat_alias (p, aliasLoc) ->
      let needsParens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let renderedPattern =
        let p = printPattern p in
        if needsParens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat([
        renderedPattern;
        Doc.text " as ";
        Doc.text aliasLoc.txt
      ])

     (* Note: module(P : S) is represented as *)
     (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_constraint ({ppat_desc = Ppat_unpack stringLoc}, {ptyp_desc = Ptyp_package packageType}) ->
        Doc.concat [
          Doc.text "module(";
          Doc.text stringLoc.txt;
          Doc.text ": ";
          printPackageType ~printModuleKeywordAndParens:false packageType;
          Doc.rparen;
        ]
    | Ppat_constraint (pattern, typ) ->
      Doc.concat [
        printPattern pattern;
        Doc.text ": ";
        printTypExpr typ;
      ]

     (* Note: module(P : S) is represented as *)
     (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_unpack stringLoc ->
        Doc.concat [
          Doc.text "module(";
          Doc.text stringLoc.txt;
          Doc.rparen;
        ]
    | _ -> failwith "unsupported pattern"
    in
    begin match p.ppat_attributes with
    | [] -> patternWithoutAttributes
    | attrs ->
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          patternWithoutAttributes;
        ]
      )
    end

  and printPatternRecordRow row =
    match row with
    (* punned {x}*)
    | ({Location.txt=Longident.Lident ident},
       {Parsetree.ppat_desc=Ppat_var {txt;_}}) when ident = txt ->
        Doc.text ident
    | (longident, pattern) ->
        Doc.group (
          Doc.concat([
            printLongident longident.txt;
            Doc.text ": ";
            Doc.indent(
              Doc.concat [
                Doc.softLine;
                printPattern pattern;
              ]
            )
          ])
        )

  and printExpression (e : Parsetree.expression) =
    let printedExpression = match e.pexp_desc with
    | Parsetree.Pexp_constant c -> printConstant c
    | Pexp_construct _ when ParsetreeViewer.hasJsxAttribute e.pexp_attributes ->
      printJsxFragment e
    | Pexp_construct ({txt = Longident.Lident "()"}, _) -> Doc.text "()"
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) -> Doc.text "list()"
    | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
      let (expressions, spread) = ParsetreeViewer.collectListExpressions e in
      let spreadDoc = match spread with
      | Some(expr) -> Doc.concat [
          Doc.text ",";
          Doc.line;
          Doc.dotdotdot;
          printExpression expr
        ]
      | None -> Doc.nil
      in
      Doc.group(
        Doc.concat([
          Doc.text "list(";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map printExpression expressions);
              spreadDoc;
            ])
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ])
      )
    | Pexp_construct (longidentLoc, args) ->
      let constr = printLongident longidentLoc.txt in
      let args = match args with
      | None -> Doc.nil
      | Some({pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.text "()"
      | Some({pexp_desc = Pexp_tuple args }) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printExpression args
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      | Some(arg) ->
        let argDoc = printExpression arg in
        let shouldHug = ParsetreeViewer.isHuggableExpression arg in
        Doc.concat [
          Doc.lparen;
          if shouldHug then argDoc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                argDoc;
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
          ];
          Doc.rparen;
        ]
      in
      Doc.group(Doc.concat [constr; args])
    | Pexp_ident(longidentLoc) ->
      printLongident longidentLoc.txt
    | Pexp_tuple exprs ->
      Doc.group(
        Doc.concat([
          Doc.text "/";
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map printExpression exprs)
            ])
          );
          Doc.ifBreaks (Doc.text ",") Doc.nil;
          Doc.softLine;
          Doc.text "/";
        ])
      )
    | Pexp_array [] -> Doc.text "[]"
    | Pexp_array exprs ->
      Doc.group(
        Doc.concat([
          Doc.lbracket;
          Doc.indent (
            Doc.concat([
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map printExpression exprs)
            ])
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rbracket;
        ])
      )
    | Pexp_record (rows, spreadExpr) ->
      let spread = match spreadExpr with
      | None -> Doc.nil
      | Some expr -> Doc.concat [
          Doc.dotdotdot;
          printExpression expr;
          Doc.comma;
          Doc.line;
        ]
      in
      (* If the record is written over multiple lines, break automatically
       * `let x = {a: 1, b: 3}` -> same line, break when line-width exceeded
       * `let x = {
       *   a: 1,
       *   b: 2,
       *  }` -> record is written on multiple lines, break the group *)
      let forceBreak =
        e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum
      in
      Doc.breakableGroup ~forceBreak (
        Doc.concat([
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              spread;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map printRecordRow rows)
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rbrace;
        ])
      )
    | Pexp_extension extension ->
      begin match extension with
      | (
          {txt = "bs.obj"},
          PStr [{
            pstr_loc = loc;
            pstr_desc = Pstr_eval({pexp_desc = Pexp_record (rows, _)}, [])
          }]
        ) ->
        (* If the object is written over multiple lines, break automatically
         * `let x = {"a": 1, "b": 3}` -> same line, break when line-width exceeded
         * `let x = {
         *   "a": 1,
         *   "b": 2,
         *  }` -> object is written on multiple lines, break the group *)
        let forceBreak =
          loc.loc_start.pos_lnum < loc.loc_end.pos_lnum
        in
        Doc.breakableGroup ~forceBreak (
          Doc.concat([
            Doc.lbrace;
            Doc.indent (
              Doc.concat [
                Doc.softLine;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map printBsObjectRow rows)
              ]
            );
            Doc.trailingComma;
            Doc.softLine;
            Doc.rbrace;
          ])
        )
      | extension ->
        printExtension extension
      end
    | Pexp_apply _ ->
      if ParsetreeViewer.isUnaryExpression e then
        printUnaryExpression e
      else if ParsetreeViewer.isBinaryExpression e then
        printBinaryExpression e
      else
        printPexpApply e
    | Pexp_unreachable -> Doc.dot
    | Pexp_field (expr, longidentLoc) ->
      let lhs =
        let doc = printExpression expr in
        if Parens.fieldExpr expr then addParens doc else doc
      in
      Doc.concat [
        lhs;
        Doc.dot;
        printLongident longidentLoc.txt;
      ]
    | Pexp_setfield (expr1, longidentLoc, expr2) ->
      printSetFieldExpr e.pexp_attributes expr1 longidentLoc expr2
    | Pexp_ifthenelse (ifExpr, thenExpr, elseExpr) ->
      if ParsetreeViewer.isTernaryExpr e then
        let (parts, alternate) = ParsetreeViewer.collectTernaryParts e in
        let ternaryDoc = match parts with
        | (condition1, consequent1)::rest ->
          Doc.group (Doc.concat [
            printTernaryOperand condition1;
            Doc.indent (
              Doc.concat [
                Doc.line;
                Doc.indent (Doc.concat [Doc.text "? "; printTernaryOperand consequent1]);
                Doc.concat (
                  List.map (fun (condition, consequent) ->
                    Doc.concat [
                      Doc.line;
                      Doc.text ": ";
                      printTernaryOperand condition;
                      Doc.line;
                      Doc.text "? ";
                      printTernaryOperand consequent;
                    ]
                  ) rest
                );
                Doc.line;
                Doc.text ": ";
                Doc.indent (printTernaryOperand alternate);
              ]
            )
          ])
        | _ -> Doc.nil
        in
        let attrs = ParsetreeViewer.filterTernaryAttributes e.pexp_attributes in
        let needsParens = match attrs with | [] -> false | _ -> true in
        Doc.concat [
          printAttributes attrs;
          if needsParens then addParens ternaryDoc else ternaryDoc;
        ]
      else
      let (ifs, elseExpr) = ParsetreeViewer.collectIfExpressions e in
      let ifDocs = Doc.join ~sep:Doc.space (
        List.mapi (fun i (ifExpr, thenExpr) ->
          let ifTxt = if i > 0 then Doc.text "else if " else  Doc.text "if " in
          let condition = printExpression ifExpr in
          Doc.concat [
            ifTxt;
            Doc.group (
              Doc.ifBreaks (addParens condition) condition;
            );
            Doc.space;
            printExpressionBlock ~braces:true thenExpr;
          ]
        ) ifs
      ) in
      let elseDoc = match elseExpr with
      | None -> Doc.nil
      | Some expr -> Doc.concat [
          Doc.text " else ";
          printExpressionBlock ~braces:true expr;
        ]
      in
      Doc.concat [
        printAttributes e.pexp_attributes;
        ifDocs;
        elseDoc;
      ]
    | Pexp_while (expr1, expr2) ->
      let condition = printExpression expr1 in
      Doc.breakableGroup ~forceBreak:true (
        Doc.concat [
          Doc.text "while ";
          Doc.group (
            Doc.ifBreaks (addParens condition) condition
          );
          Doc.space;
          printExpressionBlock ~braces:true expr2;
        ]
      )
    | Pexp_for (pattern, fromExpr, toExpr, directionFlag, body) ->
      Doc.breakableGroup ~forceBreak:true (
        Doc.concat [
          Doc.text "for ";
          printPattern pattern;
          Doc.text " in ";
          printExpression fromExpr;
          printDirectionFlag directionFlag;
          printExpression toExpr;
          Doc.space;
          printExpressionBlock ~braces:true body;
        ]
      )
    | Pexp_constraint(
        {pexp_desc = Pexp_pack modExpr},
        {ptyp_desc = Ptyp_package packageType}
      ) ->
      Doc.group (
        Doc.concat [
          Doc.text "module(";
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              printModExpr modExpr;
              Doc.text ": ";
              printPackageType ~printModuleKeywordAndParens:false packageType;
            ]
          );
          Doc.softLine;
          Doc.rparen;
        ]
      )

    | Pexp_constraint (expr, typ) ->
      Doc.concat [
        printExpression expr;
        Doc.text ": ";
        printTypExpr typ;
      ]
    | Pexp_letmodule ({txt = modName}, modExpr, expr) ->
      printExpressionBlock ~braces:true e

    | Pexp_letexception (extensionConstructor, expr) ->
      printExpressionBlock ~braces:true e
    | Pexp_assert expr ->
      let rhs =
        let doc = printExpression expr in
        if Parens.lazyOrAssertExprRhs expr then addParens doc else doc
      in
      Doc.concat [
        Doc.text "assert ";
        rhs;
      ]
    | Pexp_lazy expr ->
      let rhs =
        let doc = printExpression expr in
        if Parens.lazyOrAssertExprRhs expr then addParens doc else doc
      in
      Doc.concat [
        Doc.text "lazy ";
        rhs;
      ]
    | Pexp_open (overrideFlag, longidentLoc, expr) ->
      printExpressionBlock ~braces:true e
    | Pexp_pack (modExpr) ->
      Doc.group (Doc.concat [
        Doc.text "module(";
        Doc.indent (
          Doc.concat [
            Doc.softLine;
            printModExpr modExpr;
          ]
        );
        Doc.softLine;
        Doc.rparen;
      ])
    | Pexp_sequence _ ->
      printExpressionBlock ~braces:true e
    | Pexp_let _ ->
      printExpressionBlock ~braces:true e
    | Pexp_fun _ | Pexp_newtype _ ->
      let (attrsOnArrow, parameters, returnExpr) = ParsetreeViewer.funExpr e in
      let (uncurried, attrs) =
        ParsetreeViewer.processUncurriedAttribute attrsOnArrow
      in
      let (returnExpr, typConstraint) = match returnExpr.pexp_desc with
      | Pexp_constraint (expr, typ) -> (expr, Some typ)
      | _ -> (returnExpr, None)
      in
      let parametersDoc = printExprFunParameters ~inCallback:false ~uncurried parameters in
      let returnExprDoc =
        let shouldInline = match returnExpr.pexp_desc with
        | Pexp_array _
        | Pexp_tuple _
        | Pexp_construct (_, Some _)
        | Pexp_record _ -> true
        | _ -> false
        in
        let shouldIndent = match returnExpr.pexp_desc with
        | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _ -> false
        | _ -> true
        in
        let returnDoc = printExpression returnExpr in
        if shouldInline then Doc.concat [
          Doc.space;
          returnDoc;
        ] else
          Doc.group (
            if shouldIndent then
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  returnDoc;
                ]
              )
            else
              Doc.concat [
                Doc.space;
                returnDoc
              ]
          )
      in
      let typConstraintDoc = match typConstraint with
      | Some(typ) -> Doc.concat [Doc.text ": "; printTypExpr typ]
      | _ -> Doc.nil
      in
      let attrs = match attrs with
      | [] -> Doc.nil
      | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map printAttribute attrs);
          Doc.space;
        ]
      in
      Doc.group (
        Doc.concat [
          attrs;
          parametersDoc;
          typConstraintDoc;
          Doc.text " =>";
          returnExprDoc;
        ]
      )
    | Pexp_try (expr, cases) ->
      Doc.concat [
        Doc.text "try ";
        printExpression expr;
        Doc.text " catch ";
        printCases cases;
      ]
    | Pexp_match (expr, cases) ->
      Doc.concat [
        Doc.text "switch ";
        printExpression expr;
        Doc.space;
        printCases cases;
      ]
    | _ -> failwith "expression not yet implemented in printer"
    in
    let shouldPrintItsOwnAttributes = match e.pexp_desc with
    | Pexp_apply _
    | Pexp_fun _
    | Pexp_newtype _
    | Pexp_setfield _
    | Pexp_ifthenelse _ -> true
    | Pexp_construct _ when ParsetreeViewer.hasJsxAttribute e.pexp_attributes -> true
    | _ -> false
    in
    begin match e.pexp_attributes with
    | [] -> printedExpression
    | attrs when not shouldPrintItsOwnAttributes ->
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          printedExpression;
        ]
      )
    | _ -> printedExpression
    end

  and printPexpFun ~inCallback e =
      let (attrsOnArrow, parameters, returnExpr) = ParsetreeViewer.funExpr e in
      let (uncurried, attrs) =
        ParsetreeViewer.processUncurriedAttribute attrsOnArrow
      in
      let (returnExpr, typConstraint) = match returnExpr.pexp_desc with
      | Pexp_constraint (expr, typ) -> (expr, Some typ)
      | _ -> (returnExpr, None)
      in
      let parametersDoc = printExprFunParameters ~inCallback  ~uncurried parameters in
      let returnShouldIndent = match returnExpr.pexp_desc with
      | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _ -> false
      | _ -> true
      in
      let returnExprDoc =
        let shouldInline = match returnExpr.pexp_desc with
        | Pexp_array _
        | Pexp_tuple _
        | Pexp_construct (_, Some _)
        | Pexp_record _ -> true
        | _ -> false
        in
        let returnDoc = printExpression returnExpr in
        if shouldInline then Doc.concat [
          Doc.space;
          returnDoc;
        ] else
          Doc.group (
            if returnShouldIndent then
              Doc.concat [
                Doc.indent (
                  Doc.concat [
                    Doc.line;
                    returnDoc;
                  ]
                );
                if inCallback then Doc.softLine else Doc.nil;
              ]
            else
              Doc.concat [
                Doc.space;
                returnDoc;
              ]
          )
      in
      let typConstraintDoc = match typConstraint with
      | Some(typ) -> Doc.concat [Doc.text ": "; printTypExpr typ]
      | _ -> Doc.nil
      in
      let attrs = match attrs with
      | [] -> Doc.nil
      | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map printAttribute attrs);
          Doc.space;
        ]
      in
      Doc.group (
        Doc.concat [
          attrs;
          parametersDoc;
          typConstraintDoc;
          Doc.text " =>";
          returnExprDoc;
        ]
      )

  and printTernaryOperand expr =
    let doc = printExpression expr in
    if Parens.ternaryOperand expr then addParens doc else doc

  and printSetFieldExpr attrs lhs longidentLoc rhs =
    let rhsDoc =
      let doc = printExpression rhs in
      if Parens.setFieldExprRhs rhs then addParens doc else doc
    in
    let lhsDoc =
      let doc = printExpression lhs in
      if Parens.fieldExpr lhs then addParens doc else doc
    in
    let shouldIndent = ParsetreeViewer.isBinaryExpression rhs in
    let doc = Doc.concat [
      lhsDoc;
      Doc.dot;
      printLongident longidentLoc.txt;
      Doc.text " =";
      if shouldIndent then Doc.group (
        Doc.indent (
          (Doc.concat [Doc.line; rhsDoc])
        )
      ) else
        Doc.concat [Doc.space; rhsDoc]
    ] in
    match attrs with
    | [] -> doc
    | attrs ->
      Doc.group (
        Doc.concat [
          printAttributes attrs;
          doc
        ]
      )

  and printUnaryExpression expr =
    let printUnaryOperator op = Doc.text (
      match op with
      | "~+" -> "+"
      | "~+." -> "+."
      | "~-" -> "-"
      | "~-." ->  "-."
      | "not" -> "!"
      | "!" -> "&"
      | _ -> assert false
    ) in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, operand]
      ) ->
      let printedOperand =
        let doc = printExpression operand in
        if Parens.unaryExprOperand operand then addParens doc else doc
      in
      Doc.concat [
        printUnaryOperator operator;
        printedOperand;
      ]
    | _ -> assert false

  and printBinaryExpression (expr : Parsetree.expression) =
    let printBinaryOperator ~inlineRhs operator =
      let operatorTxt = match operator with
      | "|." -> "->"
      | "^" -> "++"
      | "=" -> "=="
      | "==" -> "==="
      | "<>" -> "!="
      | "!=" -> "!=="
      | txt -> txt
      in
      let spacingBeforeOperator =
        if operator = "|." then Doc.softLine
        else if operator = "|>" then Doc.line
        else Doc.space;
      in
      let spacingAfterOperator =
        if operator = "|." then Doc.nil
        else if operator = "|>" then Doc.space
        else if inlineRhs then Doc.space else Doc.line
      in
      Doc.concat [
        spacingBeforeOperator;
        Doc.text operatorTxt;
        spacingAfterOperator;
      ]
    in
    let printOperand ~isLhs expr parentOperator =
      let rec flatten ~isLhs expr parentOperator =
        if ParsetreeViewer.isBinaryExpression expr then
          begin match expr with
          | {pexp_desc = Pexp_apply (
              {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
              [_, left; _, right]
            )} ->
            if ParsetreeViewer.flattenableOperators parentOperator operator &&
                not (ParsetreeViewer.hasAttributes expr.pexp_attributes) then
              let leftPrinted = flatten ~isLhs:true left operator in
              let rightPrinted =
                let (_, rightAttrs) =
                  ParsetreeViewer.partitionPrinteableAttributes right.pexp_attributes
                in
                let doc =
                  printExpression {right with pexp_attributes = rightAttrs } in
                let doc = if Parens.flattenOperandRhs parentOperator right then
                  Doc.concat [Doc.lparen; doc; Doc.rparen]
                else
                  doc
                in
                let printeableAttrs =
                  ParsetreeViewer.filterPrinteableAttributes right.pexp_attributes
                in
                Doc.concat [printAttributes printeableAttrs; doc]
              in
              Doc.concat [
                leftPrinted;
                printBinaryOperator ~inlineRhs:false operator;
                rightPrinted;
              ]
            else
              let doc = printExpression {expr with pexp_attributes = []} in
              let doc = if Parens.subBinaryExprOperand parentOperator operator ||
                (expr.pexp_attributes <> [] &&
                  (ParsetreeViewer.isBinaryExpression expr ||
                ParsetreeViewer.isTernaryExpr expr)) then
                Doc.concat [Doc.lparen; doc; Doc.rparen]
              else doc
              in Doc.concat [
                printAttributes expr.pexp_attributes;
                doc
              ]
          | _ -> assert false
          end
        else
          begin match expr.pexp_desc with
          | Pexp_setfield (lhs, field, rhs) ->
            let doc = printSetFieldExpr expr.pexp_attributes lhs field rhs in
            if isLhs then addParens doc else doc
          | Pexp_apply(
              {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
              [(Nolabel, lhs); (Nolabel, rhs)]
            ) ->
            let rhsDoc = printExpression rhs in
            let lhsDoc = printExpression lhs in
            (* TODO: unify indentation of "=" *)
            let shouldIndent = ParsetreeViewer.isBinaryExpression rhs in
            let doc = Doc.group(
              Doc.concat [
                lhsDoc;
                Doc.text " =";
                if shouldIndent then Doc.group (
                  Doc.indent (Doc.concat [Doc.line; rhsDoc])
                ) else
                  Doc.concat [Doc.space; rhsDoc]
              ]
            ) in
            let doc = match expr.pexp_attributes with
            | [] -> doc
            | attrs ->
              Doc.group (
                Doc.concat [
                  printAttributes attrs;
                  doc
                ]
              )
            in
            if isLhs then addParens doc else doc
          | _ ->
            let doc = printExpression expr in
            if Parens.binaryExprOperand ~isLhs expr parentOperator then
              addParens doc
            else doc
          end
      in
      flatten ~isLhs expr parentOperator
    in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident (("|." | "|>") as op)}},
        [Nolabel, lhs; Nolabel, rhs]
      ) when not (
          ParsetreeViewer.isBinaryExpression lhs ||
          ParsetreeViewer.isBinaryExpression rhs
      ) ->
      let lhsDoc = printOperand ~isLhs:true lhs op in
      let rhsDoc = printOperand ~isLhs:false rhs op in
      Doc.concat [
        lhsDoc;
        (match op with
        | "|." -> Doc.text "->"
        | "|>" -> Doc.text " |> "
        | _ -> assert false);
        rhsDoc;
      ]
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, lhs; Nolabel, rhs]
      ) ->
      let right =
        let operatorWithRhs = Doc.concat [
          printBinaryOperator
            ~inlineRhs:(ParsetreeViewer.shouldInlineRhsBinaryExpr rhs) operator;
          printOperand ~isLhs:false rhs operator;
        ] in
        if ParsetreeViewer.shouldIndentBinaryExpr expr then
          Doc.group (Doc.indent operatorWithRhs)
        else operatorWithRhs
      in
      let doc = Doc.group (
        Doc.concat [
          printOperand ~isLhs:true lhs operator;
          right
        ]
      ) in
      Doc.concat [
        printAttributes expr.pexp_attributes;
        if Parens.binaryExpr expr then addParens doc else doc
      ]
    | _ -> Doc.nil

  (* callExpr(arg1, arg2)*)
  and printPexpApply expr =
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "##"}},
        [Nolabel, parentExpr; Nolabel, memberExpr]
      ) ->
        let member =
          let memberDoc = printExpression memberExpr in
          Doc.concat [Doc.text "\""; memberDoc; Doc.text "\""]
        in
        Doc.group (Doc.concat [
          printAttributes expr.pexp_attributes;
          printExpression parentExpr;
          Doc.lbracket;
          member;
          Doc.rbracket;
        ])
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
        [Nolabel, lhs; Nolabel, rhs]
      ) ->
        let rhsDoc = printExpression rhs in
        (* TODO: unify indentation of "=" *)
        let shouldIndent = ParsetreeViewer.isBinaryExpression rhs in
        let doc = Doc.group(
          Doc.concat [
            printExpression lhs;
            Doc.text " =";
            if shouldIndent then Doc.group (
              Doc.indent (
                (Doc.concat [Doc.line; rhsDoc])
              )
            ) else
              Doc.concat [Doc.space; rhsDoc]
          ]
        ) in
        begin match expr.pexp_attributes with
        | [] -> doc
        | attrs ->
          Doc.group (
            Doc.concat [
              printAttributes attrs;
              doc
            ]
          )
        end
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")}},
        [Nolabel, parentExpr; Nolabel, memberExpr]
      ) ->
        let member =
          let memberDoc = printExpression memberExpr in
          let shouldInline = match memberExpr.pexp_desc with
          | Pexp_constant _ | Pexp_ident _ -> true
          | _ -> false
          in
          if shouldInline then memberDoc else (
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.softLine;
                  memberDoc;
                ]
              );
              Doc.softLine
            ]
          )
        in
        Doc.group (Doc.concat [
          printAttributes expr.pexp_attributes;
          printExpression parentExpr;
          Doc.lbracket;
          member;
          Doc.rbracket;
        ])
    (* TODO: cleanup, are those branches even remotely performant? *)
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = lident}},
        args
      ) when ParsetreeViewer.isJsxExpression expr ->
      printJsxExpression lident args
    | Pexp_apply (callExpr, args) ->
      let (uncurried, attrs) =
        ParsetreeViewer.processUncurriedAttribute expr.pexp_attributes
      in
      let callExprDoc = printExpression callExpr in
      if ParsetreeViewer.requiresSpecialCallbackPrinting args then
        let argsDoc = printArgumentsWithCallback ~uncurried args in
        Doc.concat [
          printAttributes attrs;
          callExprDoc;
          argsDoc;
        ]
      else
        let argsDoc = printArguments ~uncurried args in
        Doc.concat [
          printAttributes attrs;
          callExprDoc;
          argsDoc;
        ]
    | _ -> assert false

  and printJsxExpression lident args =
    let name = printJsxName lident in
    let (formattedProps, children) = formatJsxProps args in
    (* <div className="test" /> *)
    let isSelfClosing = match children with | [] -> true | _ -> false in
    Doc.group (
      Doc.concat [
        Doc.group (
          Doc.concat [
            Doc.lessThan;
            name;
            formattedProps;
            if isSelfClosing then Doc.concat [Doc.line; Doc.text "/>"] else Doc.nil
          ]
        );
        if isSelfClosing then Doc.nil
        else
          Doc.concat [
            Doc.greaterThan;
            Doc.indent (
              Doc.concat [
                Doc.line;
                printJsxChildren children;
              ]
            );
            Doc.line;
            Doc.text "</";
            name;
            Doc.greaterThan;
          ]
      ]
    )

  and printJsxFragment expr =
    let opening = Doc.text "<>" in
    let closing = Doc.text "</>" in
    let (children, _) = ParsetreeViewer.collectListExpressions expr in
    Doc.group (
      Doc.concat [
        opening;
        begin match children with
        | [] -> Doc.nil
        | children ->
          Doc.indent (
            Doc.concat [
              Doc.line;
              printJsxChildren children;
            ]
          )
        end;
        Doc.line;
        closing;
      ]
    )

  and printJsxChildren (children: Parsetree.expression list) =
    Doc.group (
      Doc.join ~sep:Doc.line (
        List.map (fun expr ->
          let exprDoc = printExpression expr in
          if Parens.jsxChildExpr expr then addBraces exprDoc else exprDoc
        ) children
      )
    )

  and formatJsxProps args =
    let rec loop props args =
      match args with
      | [] -> (Doc.nil, [])
      | [
          (Asttypes.Labelled "children", children);
          (
            Asttypes.Nolabel,
            {Parsetree.pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, None)}
          )
        ] ->
        let formattedProps = Doc.indent (
          match props with
          | [] -> Doc.nil
          | props ->
            Doc.concat [
              Doc.line;
              Doc.group (
                Doc.join ~sep:Doc.line (props |> List.rev)
              )
            ]
        ) in
        let (children, _) = ParsetreeViewer.collectListExpressions children in
        (formattedProps, children)
      | arg::args ->
        let propDoc = formatJsxProp arg in
        loop (propDoc::props) args
    in
    loop [] args

  and formatJsxProp arg =
    match arg with
    | (
        (Asttypes.Labelled lblTxt | Optional lblTxt) as lbl,
        {
          Parsetree.pexp_attributes = [];
          pexp_desc = Pexp_ident {txt = Longident.Lident ident}
        }
      ) when lblTxt = ident (* jsx punning *) ->

      begin match lbl with
      | Nolabel -> Doc.nil
      | Labelled lbl -> Doc.text lbl
      | Optional lbl -> Doc.text ("?" ^ lbl)
      end
    | (lbl, expr) ->
      let lblDoc = match lbl with
      | Asttypes.Labelled lbl -> Doc.text (lbl ^ "=")
      | Asttypes.Optional lbl -> Doc.text (lbl ^ "=?")
      | Nolabel -> Doc.nil
      in
      let exprDoc = printExpression expr in
      Doc.concat [
        lblDoc;
        if Parens.jsxPropExpr expr then addBraces exprDoc else exprDoc;
      ]

  (* div -> div.
   * Navabar.createElement -> Navbar
   * Staff.Users.createElement -> Staff.Users *)
  and printJsxName lident =
    let rec flatten acc lident = match lident with
    | Longident.Lident txt -> txt::acc
    | Ldot (lident, txt) ->
      let acc = if txt = "createElement" then acc else txt::acc in
      flatten acc lident
    | _ -> acc
    in
    match lident with
    | Longident.Lident txt -> Doc.text txt
    | _ as lident ->
      let segments = flatten [] lident in
      Doc.join ~sep:Doc.dot (List.map Doc.text segments)

  and printArgumentsWithCallback ~uncurried args =
    let rec loop acc args = match args with
    | [] -> (Doc.nil, Doc.nil)
    | [_lbl, expr] ->
      let callback = printPexpFun ~inCallback:true expr in
      (Doc.concat (List.rev acc), callback)
    | arg::args ->
      let argDoc = printArgument arg in
      loop (Doc.line::Doc.comma::argDoc::acc) args
    in
    let (printedArgs, callback) = loop [] args in

    (* Thing.map(foo,(arg1, arg2) => MyModuleBlah.toList(argument)) *)
    let fitsOnOneLine = Doc.concat [
      if uncurried then Doc.text "(." else Doc.lparen;
      Doc.concat [
        printedArgs;
        callback;
      ];
      Doc.rparen;
    ] in

    (* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
     *   MyModuleBlah.toList(argument)
     * )
     *)
    let arugmentsFitOnOneLine =
      Doc.concat [
        if uncurried then Doc.text "(." else Doc.lparen;
        Doc.concat [
          Doc.softLine;
          printedArgs;
          Doc.breakableGroup ~forceBreak:true callback;
        ];
        Doc.softLine;
        Doc.rparen;
      ]
    in

    (* Thing.map(
     *   arg1,
     *   arg2,
     *   arg3,
     *   (param1, parm2) => doStuff(param1, parm2)
     * )
     *)
    let breakAllArgs = printArguments ~uncurried args in
    Doc.customLayout [
      fitsOnOneLine;
      arugmentsFitOnOneLine;
      breakAllArgs;
    ]

	and printArguments ~uncurried (args : (Asttypes.arg_label * Parsetree.expression) list) =
		match args with
		| [Nolabel, {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}] ->
      if uncurried then Doc.text "(.)" else Doc.text "()"
    | [(Nolabel, arg)] when ParsetreeViewer.isHuggableExpression arg ->
      Doc.concat [
        if uncurried then Doc.text "(." else Doc.lparen;
        printExpression arg;
        Doc.rparen;
      ]
		| args -> Doc.group (
				Doc.concat [
          if uncurried then Doc.text "(." else Doc.lparen;
					Doc.indent (
						Doc.concat [
              if uncurried then Doc.line else Doc.softLine;
							Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
								List.map printArgument args
							)
						]
					);
					Doc.trailingComma;
					Doc.softLine;
					Doc.rparen;
				]
			)

(*
   * argument ::=
   *   | _                            (* syntax sugar *)
   *   | expr
   *   | expr : type
   *   | ~ label-name
   *   | ~ label-name
   *   | ~ label-name ?
   *   | ~ label-name =   expr
   *   | ~ label-name =   _           (* syntax sugar *)
   *   | ~ label-name =   expr : type
   *   | ~ label-name = ? expr
   *   | ~ label-name = ? _           (* syntax sugar *)
   *   | ~ label-name = ? expr : type *)
	and printArgument ((argLbl, arg) : Asttypes.arg_label * Parsetree.expression) =
		match (argLbl, arg) with
		(* ~a (punned)*)
		| (
				(Asttypes.Labelled lbl),
				{pexp_desc=Pexp_ident {txt =Longident.Lident name}}
			) when lbl = name ->
			Doc.text ("~" ^ lbl)
		(* ~a? (optional lbl punned)*)
		| (
				(Asttypes.Optional lbl),
				{pexp_desc=Pexp_ident {txt =Longident.Lident name}}
			) when lbl = name ->
			Doc.text ("~" ^ lbl ^ "?")
		| (lbl, expr) ->
			let printedLbl = match argLbl with
			| Asttypes.Nolabel -> Doc.nil
			| Asttypes.Labelled lbl -> Doc.text ("~" ^ lbl ^ "=")
			| Asttypes.Optional lbl -> Doc.text ("~" ^ lbl ^ "=?")
			in
			let printedExpr = printExpression expr in
			Doc.concat [
				printedLbl;
				printedExpr;
			]

  and printCases (cases: Parsetree.case list) =
    Doc.breakableGroup ~forceBreak:true (
      Doc.concat [
        Doc.lbrace;
          Doc.concat [
            Doc.line;
            Doc.join ~sep:Doc.line (
              List.map printCase cases
            )
          ];
        Doc.line;
        Doc.rbrace;
      ]
    )

  and printCase (case: Parsetree.case) =
    let rhs = match case.pc_rhs.pexp_desc with
    | Pexp_let _
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_open _
    | Pexp_sequence _ ->
      printExpressionBlock ~braces:false case.pc_rhs
    | _ -> printExpression case.pc_rhs
    in
    let guard = match case.pc_guard with
    | None -> Doc.nil
    | Some expr -> Doc.group (
        Doc.concat [
          Doc.line;
          Doc.text "when ";
          printExpression expr;
        ]
      )
    in
    Doc.group (
      Doc.concat [
        Doc.text "| ";
        Doc.indent (
          Doc.concat [
            printPattern case.pc_lhs;
            guard;
            Doc.text " =>";
            Doc.line;
            rhs;
          ]
        );
      ]
    )

  and printExprFunParameters ~inCallback ~uncurried parameters =
    match parameters with
    (* let f = _ => () *)
    | [([], Asttypes.Nolabel, None, {Parsetree.ppat_desc = Ppat_any})] when not uncurried ->
      Doc.text "_"
    (* let f = a => () *)
    | [([], Asttypes.Nolabel, None, {Parsetree.ppat_desc = Ppat_var stringLoc})]  when not uncurried ->
      Doc.text stringLoc.txt
    (* let f = () => () *)
    | [([], Nolabel, None, {ppat_desc = Ppat_construct({txt = Longident.Lident "()"}, None)})] when not uncurried ->
      Doc.text "()"
    (* let f = (~greeting, ~from as hometown, ~x=?) => () *)
    | parameters ->
      let lparen = if uncurried then Doc.text "(. " else Doc.lparen in
      let shouldHug = ParsetreeViewer.parametersShouldHug parameters in
      let printedParamaters = Doc.concat [
        if shouldHug || inCallback then Doc.nil else Doc.softLine;
        Doc.join ~sep:(Doc.concat [Doc.comma; if inCallback then Doc.space else Doc.line])
          (List.map printExpFunParameter parameters)
      ] in
      Doc.group (
        Doc.concat [
          lparen;
          if shouldHug || inCallback then printedParamaters else Doc.indent (printedParamaters);
          if shouldHug || inCallback then Doc.nil else Doc.concat [Doc.trailingComma; Doc.softLine];
          Doc.rparen;
        ]
      )

  and printExpFunParameter (attrs, lbl, defaultExpr, pattern) =
    let (isUncurried, attrs) = ParsetreeViewer.processUncurriedAttribute attrs in
    let uncurried = if isUncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map printAttribute attrs);
      Doc.line;
    ] in
    (* =defaultValue *)
    let defaultExprDoc = match defaultExpr with
    | Some expr -> Doc.concat [
        Doc.text "=";
        printExpression expr
      ]
    | None -> Doc.nil
    in
    (* ~from as hometown
     * ~from                   ->  punning *)
    let labelWithPattern = match (lbl, pattern) with
    | (Asttypes.Nolabel, pattern) -> printPattern pattern
    | (
        (Asttypes.Labelled lbl | Optional lbl),
        {ppat_desc = Ppat_var stringLoc}
      ) when lbl = stringLoc.txt ->
      Doc.concat [
        Doc.text "~";
        Doc.text lbl;
      ]
    | ((Asttypes.Labelled lbl | Optional lbl), pattern) ->
      Doc.concat [
        Doc.text "~";
        Doc.text lbl;
        Doc.text " as ";
        printPattern pattern;
      ]
    in
    let optionalLabelSuffix = match (lbl, defaultExpr) with
    | (Asttypes.Optional _, None) -> Doc.text "=?"
    | _ -> Doc.nil
    in
    Doc.group (
      Doc.concat [
        uncurried;
        attrs;
        labelWithPattern;
        defaultExprDoc;
        optionalLabelSuffix;
      ]
    )

  (*
   * let x = {
   *   module Foo = Bar
   *   exception Exit
   *   open Belt
   *   let a = 1
   *   let b = 2
   *   sideEffect()
   *   a + b
   * }
   * What is an expr-block ? Everything between { ... }
   *)
  and printExpressionBlock ~braces expr =
    let rec collectRows acc expr = match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_letmodule ({txt = modName; loc = modLoc}, modExpr, expr) ->
      let letModuleDoc = Doc.concat [
        Doc.text "module ";
        Doc.text modName;
        Doc.text " = ";
        printModExpr modExpr;
      ] in
      let loc = {modLoc with loc_end = modExpr.pmod_loc.loc_end} in
      collectRows ((loc, letModuleDoc)::acc) expr
    | Pexp_letexception (extensionConstructor, expr) ->
      let letExceptionDoc = printExceptionDef extensionConstructor in
      let loc = extensionConstructor.pext_loc in
      collectRows ((loc, letExceptionDoc)::acc) expr
    | Pexp_open (overrideFlag, longidentLoc, expr) ->
      let openDoc = Doc.concat [
        Doc.text "open";
        printOverrideFlag overrideFlag;
        Doc.space;
        printLongident longidentLoc.txt;
      ] in
      let loc = longidentLoc.loc in
      collectRows ((loc, openDoc)::acc) expr
    | Pexp_sequence (expr1, expr2) ->
      let exprDoc =
        let doc = printExpression expr1 in
        if Parens.blockExpr expr1 then addParens doc else doc
      in
      let loc = expr1.pexp_loc in
      collectRows ((loc, exprDoc)::acc) expr2
    | Pexp_let (recFlag, valueBindings, expr) ->
			let recFlag = match recFlag with
			| Asttypes.Nonrecursive -> Doc.nil
			| Asttypes.Recursive -> Doc.text "rec "
			in
      let letDoc = printValueBindings ~recFlag valueBindings in
      let loc = match (valueBindings, List.rev valueBindings) with
      | ({pvb_loc = firstLoc}::_,{pvb_loc = lastLoc}::_) ->
          {firstLoc with loc_end = lastLoc.loc_end}
      | _ -> Location.none
      in
      collectRows((loc, letDoc)::acc) expr
    | _ ->
      let exprDoc =
        let doc = printExpression expr in
        if Parens.blockExpr expr then addParens doc else doc
      in
      List.rev ((expr.pexp_loc, exprDoc)::acc)
    in
    let block = collectRows [] expr |> interleaveWhitespace ~forceBreak:true in
    Doc.breakableGroup ~forceBreak:true (
      if braces then
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.line;
              block;
            ]
          );
          Doc.line;
          Doc.rbrace;
        ]
      else block
    )

  and printOverrideFlag overrideFlag = match overrideFlag with
    | Asttypes.Override -> Doc.text "!"
    | Fresh -> Doc.nil

  and printDirectionFlag flag = match flag with
    | Asttypes.Downto -> Doc.text " downto "
    | Asttypes.Upto -> Doc.text " to "

  and printRecordRow (lbl, expr) =
    Doc.concat [
      printLongident lbl.txt;
      Doc.text ": ";
      printExpression expr;
    ]

  and printBsObjectRow (lbl, expr) =
    Doc.concat [
      Doc.text "\"";
      printLongident lbl.txt;
      Doc.text "\"";
      Doc.text ": ";
      printExpression expr;
    ]
  (* The optional loc indicates whether we need to print the attributes in
   * relation to some location. In practise this means the following:
   *  `@attr type t = string` -> on the same line, print on the same line
   *  `@attr
   *   type t = string` -> attr is on prev line, print the attributes
   *   with a line break between, we respect the users' original layout *)
  and printAttributes ?loc (attrs: Parsetree.attributes) =
    match attrs with
    | [] -> Doc.nil
    | attrs ->
      let lineBreak = match loc with
      | None -> Doc.line
      | Some loc -> begin match List.rev attrs with
        | ({loc = firstLoc}, _)::_ when loc.loc_start.pos_lnum > firstLoc.loc_end.pos_lnum ->
          Doc.literalLine;
        | _ -> Doc.line
        end
      in
      Doc.concat [
        Doc.group (Doc.join ~sep:Doc.line (List.map printAttribute attrs));
        lineBreak;
      ]

  and printAttribute ((id, payload) : Parsetree.attribute) =
      let attrName = Doc.text ("@" ^ id.txt) in
        match payload with
      | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
        let exprDoc = printExpression expr in
        let needsParens = match attrs with | [] -> false | _ -> true in
        Doc.group (
          Doc.concat [
            attrName;
            addParens (
              Doc.concat [
                printAttributes attrs;
                if needsParens then addParens exprDoc else exprDoc;
              ]
            )
          ]
        )
      | _ -> attrName


  and printModExpr modExpr =
    match modExpr.pmod_desc with
    | Pmod_ident longidentLoc ->
      printLongident longidentLoc.txt
    | Pmod_structure structure ->
      Doc.breakableGroup ~forceBreak:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              printStructure structure;
            ];
          );
          Doc.softLine;
          Doc.rbrace;
        ]
      )
    | Pmod_unpack expr ->
      let shouldHug = match expr.pexp_desc with
      | Pexp_let _ -> true
      | Pexp_constraint (
          {pexp_desc = Pexp_let _ },
          {ptyp_desc = Ptyp_package packageType}
        ) -> true
      | _ -> false
      in
      let (expr, moduleConstraint) = match expr.pexp_desc with
      | Pexp_constraint (
          expr,
          {ptyp_desc = Ptyp_package packageType}
      ) ->
        let typeDoc = Doc.group (Doc.concat [
          Doc.text ":";
          Doc.indent (
            Doc.concat [
              Doc.line;
              printPackageType ~printModuleKeywordAndParens:false packageType
            ]
          )
        ]) in
        (expr, typeDoc)
      | _ -> (expr, Doc.nil)
      in
      let unpackDoc = Doc.group(Doc.concat [
        printExpression expr;
        moduleConstraint;
      ]) in
      Doc.group (
        Doc.concat [
          Doc.text "unpack(";
          if shouldHug then unpackDoc
          else
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.softLine;
                  unpackDoc;
                ]
              );
             Doc.softLine;
            ];
          Doc.rparen;
        ]
      )
    | Pmod_extension extension ->
      printExtension extension
    | Pmod_apply _ ->
      let (args, callExpr) = ParsetreeViewer.modExprApply modExpr in
      let isUnitSugar = match args with
      | [{pmod_desc = Pmod_structure []}] -> true
      | _ -> false
      in
      let shouldHug = match args with
      | [{pmod_desc = Pmod_structure _}] -> true
      | _ -> false
      in
      Doc.group (
        Doc.concat [
          printModExpr callExpr;
          if isUnitSugar then
            printModApplyArg (List.hd args)
          else
            Doc.concat [
              Doc.lparen;
              if shouldHug then
                printModApplyArg (List.hd args)
              else
                Doc.indent (
                  Doc.concat [
                    Doc.softLine;
                    Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                      List.map printModApplyArg args
                    )
                  ]
                );
              if not shouldHug then
                Doc.concat [
                  Doc.trailingComma;
                  Doc.softLine;
                ]
              else Doc.nil;
              Doc.rparen;
            ]
        ]
      )
    | Pmod_constraint (modExpr, modType) ->
      Doc.concat [
        printModExpr modExpr;
        Doc.text ": ";
        printModType modType;
      ]
    | Pmod_functor _ ->
      printModFunctor modExpr

  and printModFunctor modExpr =
    let (parameters, returnModExpr) = ParsetreeViewer.modExprFunctor modExpr in
    (* let shouldInline = match returnModExpr.pmod_desc with *)
    (* | Pmod_structure _ | Pmod_ident _ -> true *)
    (* | Pmod_constraint ({pmod_desc = Pmod_structure _}, _) -> true *)
    (* | _ -> false *)
    (* in *)
    let (returnConstraint, returnModExpr) = match returnModExpr.pmod_desc with
    | Pmod_constraint (modExpr, modType) ->
      let constraintDoc =
        let doc = printModType modType in
        if Parens.modExprFunctorConstraint modType then addParens doc else doc
      in
      let modConstraint = Doc.concat [
        Doc.text ": ";
        constraintDoc;
      ] in
      (modConstraint, printModExpr modExpr)
    | _ -> (Doc.nil, printModExpr returnModExpr)
    in
    let parametersDoc = match parameters with
    | [(attrs, {txt = "*"}, None)] ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map printAttribute attrs);
          Doc.line;
        ] in
        Doc.group (Doc.concat [
          attrs;
          Doc.text "()"
        ])
    | [([], {txt = lbl}, None)] -> Doc.text lbl
    | parameters ->
      Doc.group (
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.softLine;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map printModFunctorParam parameters
              )
            ]
          );
          Doc.trailingComma;
          Doc.softLine;
          Doc.rparen;
        ]
      )
    in
    Doc.group (
      Doc.concat [
        parametersDoc;
        returnConstraint;
        Doc.text " => ";
        returnModExpr

      ]
    )

  and printModFunctorParam (attrs, lbl, optModType) =
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map printAttribute attrs);
      Doc.line;
    ] in
    Doc.group (
      Doc.concat [
        attrs;
        Doc.text lbl.txt;
        (match optModType with
        | None -> Doc.nil
        | Some modType ->
          Doc.concat [
            Doc.text ": ";
            printModType modType
          ]);
      ]
    )

  and printModApplyArg modExpr =
    match modExpr.pmod_desc with
    | Pmod_structure [] -> Doc.text "()"
    | _ -> printModExpr modExpr


  and printExceptionDef (constr : Parsetree.extension_constructor) =
    let kind = match constr.pext_kind with
    | Pext_rebind {txt = longident} -> Doc.indent (
        Doc.concat [
          Doc.text " =";
          Doc.line;
          printLongident longident;
        ]
     )
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadtDoc = match gadt with
      | Some typ -> Doc.concat [
          Doc.text ": ";
          printTypExpr typ;
        ]
      | None -> Doc.nil
      in
      Doc.concat [
        printConstructorArguments args;
        gadtDoc
      ]
    in
    Doc.group (
      Doc.concat [
        printAttributes constr.pext_attributes;
        Doc.text "exception ";
        Doc.text constr.pext_name.txt;
        kind
      ]
    )

  and printExtensionConstructor i (constr : Parsetree.extension_constructor) =
    let attrs = printAttributes constr.pext_attributes in
    let bar = if i > 0 then Doc.text "| "
      else Doc.ifBreaks (Doc.text "| ") Doc.nil
    in
    let kind = match constr.pext_kind with
    | Pext_rebind {txt = longident} -> Doc.indent (
        Doc.concat [
          Doc.text " =";
          Doc.line;
          printLongident longident;
        ]
     )
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadtDoc = match gadt with
      | Some typ -> Doc.concat [
          Doc.text ": ";
          printTypExpr typ;
        ]
      | None -> Doc.nil
      in
      Doc.concat [
        printConstructorArguments args;
        gadtDoc
      ]
    in
    Doc.concat [
      bar;
      Doc.group (
        Doc.concat [
          attrs;
          Doc.text constr.pext_name.txt;
          kind;
        ]
      )
    ]

  let printImplementation (s: Parsetree.structure) comments src =
    let t = CommentAst.initStructure s comments in

    let stringDoc = Doc.toString ~width:80 (printStructure s) in
    print_endline stringDoc;
    print_newline()

  let printInterface (s: Parsetree.signature) =
    let stringDoc = Doc.toString ~width:80 (printSignature s) in
    print_endline stringDoc;
    print_newline()
end

