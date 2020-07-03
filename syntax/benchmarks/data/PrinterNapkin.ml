module Printer = {
  type rec printer = {
    src: bytes,
    comments: CommentAst.t,
  }
  
  let rec collectPatternsFromListConstruct = (acc, pattern) =>
    {
      open Parsetree
      switch pattern.ppat_desc {
      | Ppat_construct(
          {txt: Longident.Lident("::")},
          Some({ppat_desc: Ppat_tuple(list(pat, rest))}),
        ) =>
        collectPatternsFromListConstruct(list(pat, ...acc), rest)
      | _ => /List.rev(acc), pattern/
      }
    }
  
  let addParens = doc =>
    Doc.group(
      Doc.concat(list(
        Doc.lparen,
        Doc.indent(Doc.concat(list(Doc.softLine, doc))),
        Doc.softLine,
        Doc.rparen,
      )),
    )
  
  let addBraces = doc =>
    Doc.group(Doc.concat(list(Doc.lbrace, doc, Doc.rbrace)))
  
  let interleaveWhitespace = (
    ~forceBreak=false,
    rows: list</Location.t, Doc.t/>,
  ) => {
    let rec loop = (prevLoc, acc, rows) =>
      switch rows {
      | list() => Doc.concat(List.rev(acc))
      | list(/loc, doc/, ...rest) =>
        if (
          loc.Location.loc_start.pos_lnum -
          prevLoc.Location.loc_end.pos_lnum > 1
        ) {
          loop(loc, list(doc, Doc.line, Doc.line, ...acc), rest)
        } else {
          loop(loc, list(doc, Doc.line, ...acc), rest)
        }
      }
    
    switch rows {
    | list() => Doc.nil
    | list(/firstLoc, firstDoc/, ...rest) =>
      let forceBreak =
        forceBreak ||
        switch List.rev(rest) {
        | list(/lastLoc, _/, ..._) =>
          firstLoc.loc_start.pos_lnum !== lastLoc.loc_end.pos_lnum
        | _ => false
        }
      
      Doc.breakableGroup(~forceBreak, loop(firstLoc, list(firstDoc), rest))
    }
  }
  
  let printLongident = l =>
    switch l {
    | Longident.Lident(lident) => Doc.text(lident)
    | Longident.Ldot(lident, txt) as l =>
      let txts = Longident.flatten(l)
      Doc.join(~sep=Doc.dot, List.map(Doc.text, txts))
    | _ => failwith("unsupported ident")
    }
  
  let escapeStringContents = s => {
    let len = String.length(s)
    let b = Buffer.create(len)
    for i in 0 to len - 1 {
      let c = String.get(s, i)
      if c == '\b' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 'b')
      } else if c == '\t' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 't')
      } else if c == '\n' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 'n')
      } else if c == '\r' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, 'r')
      } else if c == '"' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, '"')
      } else if c == '\\' {
        Buffer.add_char(b, '\\')
        Buffer.add_char(b, '\\')
      } else {
        Buffer.add_char(b, c)
      }
    }
    Buffer.contents(b)
  }
  
  let printConstant = c =>
    switch c {
    | Parsetree.Pconst_integer(s, _) => Doc.text(s)
    | Pconst_string(s, _) => Doc.text("\"" ++ escapeStringContents(s) ++ "\"")
    | Pconst_float(s, _) => Doc.text(s)
    | Pconst_char(c) => Doc.text("'" ++ Char.escaped(c) ++ "'")
    }
  
  let rec printStructure = (s: Parsetree.structure) =>
    interleaveWhitespace(
      List.map(si => /si.Parsetree.pstr_loc, printStructureItem(si)/, s),
    )
  
  and printStructureItem = (si: Parsetree.structure_item) =>
    switch si.pstr_desc {
    | Pstr_value(rec_flag, valueBindings) =>
      let recFlag = switch rec_flag {
      | Asttypes.Nonrecursive => Doc.nil
      | Asttypes.Recursive => Doc.text("rec ")
      }
      
      printValueBindings(~recFlag, valueBindings)
    | Pstr_type(recFlag, typeDeclarations) =>
      let recFlag = switch recFlag {
      | Asttypes.Nonrecursive => Doc.nil
      | Asttypes.Recursive => Doc.text("rec ")
      }
      
      printTypeDeclarations(~recFlag, typeDeclarations)
    | Pstr_primitive(valueDescription) =>
      printValueDescription(valueDescription)
    | Pstr_eval(expr, attrs) =>
      let needsParens = switch expr {
      | {
          pexp_attributes: list(/{txt: "ns.ternary"}, _/),
          pexp_desc: Pexp_ifthenelse(_),
        } =>
        false
      | _ when ParsetreeViewer.hasAttributes(expr.pexp_attributes) => true
      | _ => false
      }
      
      let exprDoc = {
        let doc = printExpression(expr)
        if needsParens {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.concat(list(printAttributes(attrs), exprDoc))
    | Pstr_attribute(attr) =>
      Doc.concat(list(Doc.text("@"), printAttribute(attr)))
    | Pstr_extension(extension, attrs) =>
      Doc.concat(list(
        printAttributes(attrs),
        Doc.concat(list(Doc.text("%"), printExtension(extension))),
      ))
    | Pstr_include(includeDeclaration) =>
      printIncludeDeclaration(includeDeclaration)
    | Pstr_open(openDescription) => printOpenDescription(openDescription)
    | Pstr_modtype(modTypeDecl) => printModuleTypeDeclaration(modTypeDecl)
    | Pstr_module(moduleBinding) =>
      printModuleBinding(~isRec=false, 0, moduleBinding)
    | Pstr_recmodule(moduleBindings) =>
      Doc.join(
        ~sep=Doc.line,
        List.mapi(
          (i, mb) => printModuleBinding(~isRec=true, i, mb),
          moduleBindings,
        ),
      )
    | Pstr_exception(extensionConstructor) =>
      printExceptionDef(extensionConstructor)
    | Pstr_typext(typeExtension) => printTypeExtension(typeExtension)
    | Pstr_class(_) | Pstr_class_type(_) => Doc.nil
    }
  
  and printTypeExtension = (te: Parsetree.type_extension) => {
    let prefix = Doc.text("type ")
    let name = printLongident(te.ptyext_path.txt)
    let typeParams = switch te.ptyext_params {
    | list() => Doc.nil
    | typeParams =>
      Doc.group(
        Doc.concat(list(
          Doc.lessThan,
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                List.map(printTypeParam, typeParams),
              ),
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.greaterThan,
        )),
      )
    }
    
    let extensionConstructors = {
      let ecs = te.ptyext_constructors
      let forceBreak = switch /ecs, List.rev(ecs)/ {
      | /list(first, ..._), list(last, ..._)/ =>
        first.pext_loc.loc_start.pos_lnum >
        te.ptyext_path.loc.loc_end.pos_lnum ||
          first.pext_loc.loc_start.pos_lnum < last.pext_loc.loc_end.pos_lnum
      | _ => false
      }
      
      let privateFlag = switch te.ptyext_private {
      | Asttypes.Private => Doc.concat(list(Doc.text("private"), Doc.line))
      | Public => Doc.nil
      }
      
      Doc.breakableGroup(
        ~forceBreak,
        Doc.indent(
          Doc.concat(list(
            Doc.line,
            privateFlag,
            Doc.join(~sep=Doc.line, List.mapi(printExtensionConstructor, ecs)),
          )),
        ),
      )
    }
    
    Doc.group(
      Doc.concat(list(
        printAttributes(~loc=te.ptyext_path.loc, te.ptyext_attributes),
        prefix,
        name,
        typeParams,
        Doc.text(" +="),
        extensionConstructors,
      )),
    )
  }
  
  and printModuleBinding = (~isRec, i, moduleBinding) => {
    let prefix = if i == 0 {
      Doc.concat(list(
        Doc.text("module "),
        if isRec {
          Doc.text("rec ")
        } else {
          Doc.nil
        },
      ))
    } else {
      Doc.text("and ")
    }
    
    let /modExprDoc, modConstraintDoc/ = switch moduleBinding.pmb_expr {
    | {pmod_desc: Pmod_constraint(modExpr, modType)} =>
      /
        printModExpr(modExpr),
        Doc.concat(list(Doc.text(": "), printModType(modType))),
      /
    | modExpr => /printModExpr(modExpr), Doc.nil/
    }
    
    Doc.concat(list(
      printAttributes(
        ~loc=moduleBinding.pmb_name.loc,
        moduleBinding.pmb_attributes,
      ),
      prefix,
      Doc.text(moduleBinding.pmb_name.Location.txt),
      modConstraintDoc,
      Doc.text(" = "),
      modExprDoc,
    ))
  }
  
  and printModuleTypeDeclaration = (
    modTypeDecl: Parsetree.module_type_declaration,
  ) =>
    Doc.concat(list(
      printAttributes(modTypeDecl.pmtd_attributes),
      Doc.text("module type "),
      Doc.text(modTypeDecl.pmtd_name.txt),
      switch modTypeDecl.pmtd_type {
      | None => Doc.nil
      | Some(modType) =>
        Doc.concat(list(Doc.text(" = "), printModType(modType)))
      },
    ))
  
  and printModType = modType => {
    let modTypeDoc = switch modType.pmty_desc {
    | Parsetree.Pmty_ident({txt: longident, loc}) =>
      Doc.concat(list(
        printAttributes(~loc, modType.pmty_attributes),
        printLongident(longident),
      ))
    | Pmty_signature(signature) =>
      let signatureDoc = Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list(
          Doc.lbrace,
          Doc.indent(Doc.concat(list(Doc.line, printSignature(signature)))),
          Doc.line,
          Doc.rbrace,
        )),
      )
      Doc.concat(list(printAttributes(modType.pmty_attributes), signatureDoc))
    | Pmty_functor(_) =>
      let /parameters, returnType/ = ParsetreeViewer.functorType(modType)
      let parametersDoc = switch parameters {
      | list() => Doc.nil
      | list(/attrs, {Location.txt: "_"}, Some(modType)/) =>
        let attrs = switch attrs {
        | list() => Doc.nil
        | attrs =>
          Doc.concat(list(
            Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
            Doc.line,
          ))
        }
        Doc.concat(list(attrs, printModType(modType)))
      | params =>
        Doc.group(
          Doc.concat(list(
            Doc.lparen,
            Doc.indent(
              Doc.concat(list(
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                  List.map(
                    (/attrs, lbl, modType/) => {
                      let attrs = switch attrs {
                      | list() => Doc.nil
                      | attrs =>
                        Doc.concat(list(
                          Doc.join(
                            ~sep=Doc.line,
                            List.map(printAttribute, attrs),
                          ),
                          Doc.line,
                        ))
                      }
                      Doc.concat(list(
                        attrs,
                        if lbl.Location.txt == "_" {
                          Doc.nil
                        } else {
                          Doc.text(lbl.txt)
                        },
                        switch modType {
                        | None => Doc.nil
                        | Some(modType) =>
                          Doc.concat(list(
                            if lbl.txt == "_" {
                              Doc.nil
                            } else {
                              Doc.text(": ")
                            },
                            printModType(modType),
                          ))
                        },
                      ))
                    },
                    params,
                  ),
                ),
              )),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.rparen,
          )),
        )
      }
      
      let returnDoc = {
        let doc = printModType(returnType)
        if Parens.modTypeFunctorReturn(returnType) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.group(
        Doc.concat(list(
          parametersDoc,
          Doc.group(Doc.concat(list(Doc.text(" =>"), Doc.line, returnDoc))),
        )),
      )
    | Pmty_typeof(modExpr) =>
      Doc.concat(list(Doc.text("module type of "), printModExpr(modExpr)))
    | Pmty_extension(extension) => printExtension(extension)
    | Pmty_alias({txt: longident}) =>
      Doc.concat(list(Doc.text("module "), printLongident(longident)))
    | Pmty_with(modType, withConstraints) =>
      let operand = {
        let doc = printModType(modType)
        if Parens.modTypeWithOperand(modType) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.group(
        Doc.concat(list(
          operand,
          Doc.indent(
            Doc.concat(list(Doc.line, printWithConstraints(withConstraints))),
          ),
        )),
      )
    }
    
    let attrsAlreadyPrinted = switch modType.pmty_desc {
    | (Pmty_functor(_) | Pmty_signature(_)) | Pmty_ident(_) => true
    | _ => false
    }
    Doc.concat(list(
      if attrsAlreadyPrinted {
        Doc.nil
      } else {
        printAttributes(modType.pmty_attributes)
      },
      modTypeDoc,
    ))
  }
  
  and printWithConstraints = withConstraints => {
    let rows = List.mapi(
      (i, withConstraint) =>
        Doc.group(
          Doc.concat(list(
            if i === 0 {
              Doc.text("with ")
            } else {
              Doc.text("and ")
            },
            printWithConstraint(withConstraint),
          )),
        ),
      withConstraints,
    )
    
    Doc.join(~sep=Doc.line, rows)
  }
  
  and printWithConstraint = (withConstraint: Parsetree.with_constraint) =>
    switch withConstraint {
    | Pwith_type({txt: longident}, typeDeclaration) =>
      Doc.group(
        printTypeDeclaration(
          ~name=printLongident(longident),
          ~equalSign="=",
          ~recFlag=Doc.nil,
          0,
          typeDeclaration,
        ),
      )
    | Pwith_module({txt: longident1}, {txt: longident2}) =>
      Doc.concat(list(
        Doc.text("module "),
        printLongident(longident1),
        Doc.text(" ="),
        Doc.indent(Doc.concat(list(Doc.line, printLongident(longident2)))),
      ))
    | Pwith_typesubst({txt: longident}, typeDeclaration) =>
      Doc.group(
        printTypeDeclaration(
          ~name=printLongident(longident),
          ~equalSign=":=",
          ~recFlag=Doc.nil,
          0,
          typeDeclaration,
        ),
      )
    | Pwith_modsubst({txt: longident1}, {txt: longident2}) =>
      Doc.concat(list(
        Doc.text("module "),
        printLongident(longident1),
        Doc.text(" :="),
        Doc.indent(Doc.concat(list(Doc.line, printLongident(longident2)))),
      ))
    }
  
  and printSignature = signature =>
    interleaveWhitespace(
      List.map(
        si => /si.Parsetree.psig_loc, printSignatureItem(si)/,
        signature,
      ),
    )
  
  and printSignatureItem = (si: Parsetree.signature_item) =>
    switch si.psig_desc {
    | Parsetree.Psig_value(valueDescription) =>
      printValueDescription(valueDescription)
    | Psig_type(recFlag, typeDeclarations) =>
      let recFlag = switch recFlag {
      | Asttypes.Nonrecursive => Doc.nil
      | Asttypes.Recursive => Doc.text("rec ")
      }
      
      printTypeDeclarations(~recFlag, typeDeclarations)
    | Psig_typext(typeExtension) => printTypeExtension(typeExtension)
    | Psig_exception(extensionConstructor) =>
      printExceptionDef(extensionConstructor)
    | Psig_module(moduleDeclaration) =>
      printModuleDeclaration(moduleDeclaration)
    | Psig_recmodule(moduleDeclarations) =>
      printRecModuleDeclarations(moduleDeclarations)
    | Psig_modtype(modTypeDecl) => printModuleTypeDeclaration(modTypeDecl)
    | Psig_open(openDescription) => printOpenDescription(openDescription)
    | Psig_include(includeDescription) =>
      printIncludeDescription(includeDescription)
    | Psig_attribute(attr) =>
      Doc.concat(list(Doc.text("@"), printAttribute(attr)))
    | Psig_extension(extension, attrs) =>
      Doc.concat(list(
        printAttributes(attrs),
        Doc.concat(list(Doc.text("%"), printExtension(extension))),
      ))
    | Psig_class(_) | Psig_class_type(_) => Doc.nil
    }
  
  and printRecModuleDeclarations = moduleDeclarations =>
    Doc.group(
      Doc.join(
        ~sep=Doc.line,
        List.mapi(
          (i, md: Parsetree.module_declaration) => {
            let body = switch md.pmd_type.pmty_desc {
            | Parsetree.Pmty_alias({txt: longident}) =>
              Doc.concat(list(Doc.text(" = "), printLongident(longident)))
            | _ =>
              let needsParens = switch md.pmd_type.pmty_desc {
              | Pmty_with(_) => true
              | _ => false
              }
              
              let modTypeDoc = {
                let doc = printModType(md.pmd_type)
                if needsParens {
                  addParens(doc)
                } else {
                  doc
                }
              }
              
              Doc.concat(list(Doc.text(": "), modTypeDoc))
            }
            
            let prefix = if i < 1 {
              "module rec "
            } else {
              "and "
            }
            Doc.concat(list(
              printAttributes(~loc=md.pmd_name.loc, md.pmd_attributes),
              Doc.text(prefix),
              Doc.text(md.pmd_name.txt),
              body,
            ))
          },
          moduleDeclarations,
        ),
      ),
    )
  
  and printModuleDeclaration = (md: Parsetree.module_declaration) => {
    let body = switch md.pmd_type.pmty_desc {
    | Parsetree.Pmty_alias({txt: longident}) =>
      Doc.concat(list(Doc.text(" = "), printLongident(longident)))
    | _ => Doc.concat(list(Doc.text(": "), printModType(md.pmd_type)))
    }
    
    Doc.concat(list(
      printAttributes(~loc=md.pmd_name.loc, md.pmd_attributes),
      Doc.text("module "),
      Doc.text(md.pmd_name.txt),
      body,
    ))
  }
  
  and printOpenDescription = (openDescription: Parsetree.open_description) =>
    Doc.concat(list(
      printAttributes(openDescription.popen_attributes),
      Doc.text("open"),
      switch openDescription.popen_override {
      | Asttypes.Fresh => Doc.space
      | Asttypes.Override => Doc.text("! ")
      },
      printLongident(openDescription.popen_lid.txt),
    ))
  
  and printIncludeDescription = (
    includeDescription: Parsetree.include_description,
  ) =>
    Doc.concat(list(
      printAttributes(includeDescription.pincl_attributes),
      Doc.text("include "),
      printModType(includeDescription.pincl_mod),
    ))
  
  and printIncludeDeclaration = (
    includeDeclaration: Parsetree.include_declaration,
  ) =>
    Doc.concat(list(
      printAttributes(includeDeclaration.pincl_attributes),
      Doc.text("include "),
      printModExpr(includeDeclaration.pincl_mod),
    ))
  
  and printValueBindings = (~recFlag, vbs: list<Parsetree.value_binding>) => {
    let rows = List.mapi(
      (i, vb) => {
        let doc = printValueBinding(~recFlag, i, vb)
        /vb.Parsetree.pvb_loc, doc/
      },
      vbs,
    )
    
    interleaveWhitespace(rows)
  }
  
  and printValueDescription = valueDescription => {
    let isExternal = switch valueDescription.pval_prim {
    | list() => false
    | _ => true
    }
    
    Doc.group(
      Doc.concat(list(
        Doc.text(
          if isExternal {
            "external "
          } else {
            "let "
          },
        ),
        Doc.text(valueDescription.pval_name.txt),
        Doc.text(": "),
        printTypExpr(valueDescription.pval_type),
        if isExternal {
          Doc.group(
            Doc.concat(list(
              Doc.text(" ="),
              Doc.indent(
                Doc.concat(list(
                  Doc.line,
                  Doc.join(
                    ~sep=Doc.line,
                    List.map(
                      s =>
                        Doc.concat(list(
                          Doc.text("\""),
                          Doc.text(s),
                          Doc.text("\""),
                        )),
                      valueDescription.pval_prim,
                    ),
                  ),
                )),
              ),
            )),
          )
        } else {
          Doc.nil
        },
      )),
    )
  }
  
  and printTypeDeclarations = (~recFlag, typeDeclarations) => {
    let rows = List.mapi(
      (i, td) => {
        let doc = printTypeDeclaration(
          ~name=Doc.text(td.Parsetree.ptype_name.txt),
          ~equalSign="=",
          ~recFlag,
          i,
          td,
        )
        
        /td.Parsetree.ptype_loc, doc/
      },
      typeDeclarations,
    )
    interleaveWhitespace(rows)
  }
  
  and printTypeDeclaration = (
    ~name,
    ~equalSign,
    ~recFlag,
    i,
    td: Parsetree.type_declaration,
  ) => {
    let attrs = printAttributes(~loc=td.ptype_loc, td.ptype_attributes)
    let prefix = if i > 0 {
      Doc.text("and ")
    } else {
      Doc.concat(list(Doc.text("type "), recFlag))
    }
    
    let typeName = name
    let typeParams = switch td.ptype_params {
    | list() => Doc.nil
    | typeParams =>
      Doc.group(
        Doc.concat(list(
          Doc.lessThan,
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                List.map(printTypeParam, typeParams),
              ),
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.greaterThan,
        )),
      )
    }
    
    let manifestAndKind = switch td.ptype_kind {
    | Ptype_abstract =>
      switch td.ptype_manifest {
      | None => Doc.nil
      | Some(typ) =>
        Doc.concat(list(
          Doc.concat(list(Doc.space, Doc.text(equalSign), Doc.space)),
          printPrivateFlag(td.ptype_private),
          printTypExpr(typ),
        ))
      }
    | Ptype_open =>
      Doc.concat(list(
        Doc.concat(list(Doc.space, Doc.text(equalSign), Doc.space)),
        printPrivateFlag(td.ptype_private),
        Doc.text(".."),
      ))
    | Ptype_record(lds) =>
      let manifest = switch td.ptype_manifest {
      | None => Doc.nil
      | Some(typ) =>
        Doc.concat(list(
          Doc.concat(list(Doc.space, Doc.text(equalSign), Doc.space)),
          printTypExpr(typ),
        ))
      }
      
      Doc.concat(list(
        manifest,
        Doc.concat(list(Doc.space, Doc.text(equalSign), Doc.space)),
        printPrivateFlag(td.ptype_private),
        printRecordDeclaration(lds),
      ))
    | Ptype_variant(cds) =>
      let manifest = switch td.ptype_manifest {
      | None => Doc.nil
      | Some(typ) =>
        Doc.concat(list(
          Doc.concat(list(Doc.space, Doc.text(equalSign), Doc.space)),
          printTypExpr(typ),
        ))
      }
      
      Doc.concat(list(
        manifest,
        Doc.concat(list(Doc.space, Doc.text(equalSign))),
        printConstructorDeclarations(~privateFlag=td.ptype_private, cds),
      ))
    }
    
    let constraints = printTypeDefinitionConstraints(td.ptype_cstrs)
    Doc.group(
      Doc.concat(list(
        attrs,
        prefix,
        typeName,
        typeParams,
        manifestAndKind,
        constraints,
      )),
    )
  }
  
  and printTypeDefinitionConstraints = cstrs =>
    switch cstrs {
    | list() => Doc.nil
    | cstrs =>
      Doc.indent(
        Doc.group(
          Doc.concat(list(
            Doc.line,
            Doc.group(
              Doc.join(
                ~sep=Doc.line,
                List.map(printTypeDefinitionConstraint, cstrs),
              ),
            ),
          )),
        ),
      )
    }
  
  and printTypeDefinitionConstraint = (
    /typ1, typ2, _loc/: /Parsetree.core_type, Parsetree.core_type, Location.t/,
  ) =>
    Doc.concat(list(
      Doc.text("constraint "),
      printTypExpr(typ1),
      Doc.text(" = "),
      printTypExpr(typ2),
    ))
  
  and printPrivateFlag = (flag: Asttypes.private_flag) =>
    switch flag {
    | Private => Doc.text("private ")
    | Public => Doc.nil
    }
  
  and printTypeParam = (param: /Parsetree.core_type, Asttypes.variance/) => {
    let /typ, variance/ = param
    let printedVariance = switch variance {
    | Covariant => Doc.text("+")
    | Contravariant => Doc.text("-")
    | Invariant => Doc.nil
    }
    
    Doc.concat(list(printedVariance, printTypExpr(typ)))
  }
  
  and printRecordDeclaration = (lds: list<Parsetree.label_declaration>) => {
    let forceBreak = switch /lds, List.rev(lds)/ {
    | /list(first, ..._), list(last, ..._)/ =>
      first.pld_loc.loc_start.pos_lnum < last.pld_loc.loc_end.pos_lnum
    | _ => false
    }
    
    Doc.breakableGroup(
      ~forceBreak,
      Doc.concat(list(
        Doc.lbrace,
        Doc.indent(
          Doc.concat(list(
            Doc.softLine,
            Doc.join(
              ~sep=Doc.concat(list(Doc.comma, Doc.line)),
              List.map(printLabelDeclaration, lds),
            ),
          )),
        ),
        Doc.trailingComma,
        Doc.softLine,
        Doc.rbrace,
      )),
    )
  }
  
  and printConstructorDeclarations = (
    ~privateFlag,
    cds: list<Parsetree.constructor_declaration>,
  ) => {
    let forceBreak = switch /cds, List.rev(cds)/ {
    | /list(first, ..._), list(last, ..._)/ =>
      first.pcd_loc.loc_start.pos_lnum < last.pcd_loc.loc_end.pos_lnum
    | _ => false
    }
    
    let privateFlag = switch privateFlag {
    | Asttypes.Private => Doc.concat(list(Doc.text("private"), Doc.line))
    | Public => Doc.nil
    }
    
    Doc.breakableGroup(
      ~forceBreak,
      Doc.indent(
        Doc.concat(list(
          Doc.line,
          privateFlag,
          Doc.join(~sep=Doc.line, List.mapi(printConstructorDeclaration, cds)),
        )),
      ),
    )
  }
  
  and printConstructorDeclaration = (
    i,
    cd: Parsetree.constructor_declaration,
  ) => {
    let attrs = printAttributes(cd.pcd_attributes)
    let bar = if i > 0 {
      Doc.text("| ")
    } else {
      Doc.ifBreaks(Doc.text("| "), Doc.nil)
    }
    
    let constrName = Doc.text(cd.pcd_name.txt)
    let constrArgs = printConstructorArguments(cd.pcd_args)
    let gadt = switch cd.pcd_res {
    | None => Doc.nil
    | Some(typ) =>
      Doc.indent(Doc.concat(list(Doc.text(": "), printTypExpr(typ))))
    }
    
    Doc.concat(list(
      bar,
      Doc.group(Doc.concat(list(attrs, constrName, constrArgs, gadt))),
    ))
  }
  
  and printConstructorArguments = (cdArgs: Parsetree.constructor_arguments) =>
    switch cdArgs {
    | Pcstr_tuple(list()) => Doc.nil
    | Pcstr_tuple(types) =>
      Doc.group(
        Doc.indent(
          Doc.concat(list(
            Doc.lparen,
            Doc.indent(
              Doc.concat(list(
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                  List.map(printTypExpr, types),
                ),
              )),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.rparen,
          )),
        ),
      )
    | Pcstr_record(lds) =>
      Doc.indent(
        Doc.concat(list(
          Doc.lparen,
          Doc.lbrace,
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                List.map(printLabelDeclaration, lds),
              ),
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rbrace,
          Doc.rparen,
        )),
      )
    }
  
  and printLabelDeclaration = (ld: Parsetree.label_declaration) => {
    let attrs = printAttributes(~loc=ld.pld_name.loc, ld.pld_attributes)
    let mutableFlag = switch ld.pld_mutable {
    | Mutable => Doc.text("mutable ")
    | Immutable => Doc.nil
    }
    
    let name = Doc.text(ld.pld_name.txt)
    Doc.group(
      Doc.concat(list(
        attrs,
        mutableFlag,
        name,
        Doc.text(": "),
        printTypExpr(ld.pld_type),
      )),
    )
  }
  
  and printTypExpr = (typExpr: Parsetree.core_type) => {
    let renderedType = switch typExpr.ptyp_desc {
    | Ptyp_any => Doc.text("_")
    | Ptyp_var(var) => Doc.text("'" ++ var)
    | Ptyp_extension(extension) => printExtension(extension)
    | Ptyp_alias(typ, alias) =>
      let typ = {
        let needsParens = switch typ.ptyp_desc {
        | Ptyp_arrow(_) => true
        | _ => false
        }
        
        let doc = printTypExpr(typ)
        if needsParens {
          Doc.concat(list(Doc.lparen, doc, Doc.rparen))
        } else {
          doc
        }
      }
      
      Doc.concat(list(typ, Doc.text(" as "), Doc.text("'" ++ alias)))
    | Ptyp_constr(
        {txt: Longident.Ldot(Longident.Lident("Js"), "t")},
        list(typ),
      ) =>
      let bsObject = printTypExpr(typ)
      switch typExpr.ptyp_attributes {
      | list() => bsObject
      | attrs =>
        Doc.concat(list(
          Doc.group(Doc.join(~sep=Doc.line, List.map(printAttribute, attrs))),
          Doc.space,
          printTypExpr(typ),
        ))
      }
    | Ptyp_constr(
        longidentLoc,
        list({ptyp_desc: Parsetree.Ptyp_tuple(tuple)}),
      ) =>
      let constrName = printLongident(longidentLoc.txt)
      Doc.group(
        Doc.concat(list(
          constrName,
          Doc.lessThan,
          printTupleType(~inline=true, tuple),
          Doc.greaterThan,
        )),
      )
    | Ptyp_constr(longidentLoc, constrArgs) =>
      let constrName = printLongident(longidentLoc.txt)
      switch constrArgs {
      | list() => constrName
      | list({
          Parsetree.ptyp_desc: 
            Ptyp_constr(
              {txt: Longident.Ldot(Longident.Lident("Js"), "t")},
              list({ptyp_desc: Ptyp_object(fields, openFlag)}),
            ),
        }) =>
        Doc.concat(list(
          constrName,
          Doc.lessThan,
          printBsObjectSugar(~inline=true, fields, openFlag),
          Doc.greaterThan,
        ))
      | args =>
        Doc.group(
          Doc.concat(list(
            constrName,
            Doc.lessThan,
            Doc.indent(
              Doc.concat(list(
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                  List.map(printTypExpr, constrArgs),
                ),
              )),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.greaterThan,
          )),
        )
      }
    | Ptyp_arrow(_) =>
      let /attrsBefore, args, returnType/ = ParsetreeViewer.arrowType(typExpr)
      let returnTypeNeedsParens = switch returnType.ptyp_desc {
      | Ptyp_alias(_) => true
      | _ => false
      }
      
      let returnDoc = {
        let doc = printTypExpr(returnType)
        if returnTypeNeedsParens {
          Doc.concat(list(Doc.lparen, doc, Doc.rparen))
        } else {
          doc
        }
      }
      
      let /isUncurried, attrs/ = ParsetreeViewer.processUncurriedAttribute(
        attrsBefore,
      )
      switch args {
      | list() => Doc.nil
      | list(/list(), Nolabel, n/) when !isUncurried =>
        let hasAttrsBefore = !(attrs == list())
        let attrs = if hasAttrsBefore {
          Doc.concat(list(
            Doc.join(~sep=Doc.line, List.map(printAttribute, attrsBefore)),
            Doc.space,
          ))
        } else {
          Doc.nil
        }
        
        Doc.group(
          Doc.concat(list(
            Doc.group(attrs),
            Doc.group(
              if hasAttrsBefore {
                Doc.concat(list(
                  Doc.lparen,
                  Doc.indent(
                    Doc.concat(list(
                      Doc.softLine,
                      printTypExpr(n),
                      Doc.text(" => "),
                      returnDoc,
                    )),
                  ),
                  Doc.softLine,
                  Doc.rparen,
                ))
              } else {
                Doc.concat(list(printTypExpr(n), Doc.text(" => "), returnDoc))
              },
            ),
          )),
        )
      | args =>
        let attrs = switch attrs {
        | list() => Doc.nil
        | attrs =>
          Doc.concat(list(
            Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
            Doc.space,
          ))
        }
        
        let renderedArgs = Doc.concat(list(
          attrs,
          Doc.text("("),
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              if isUncurried {
                Doc.concat(list(Doc.dot, Doc.space))
              } else {
                Doc.nil
              },
              Doc.join(
                ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                List.map(printTypeParameter, args),
              ),
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.text(")"),
        ))
        Doc.group(Doc.concat(list(renderedArgs, Doc.text(" => "), returnDoc)))
      }
    | Ptyp_tuple(types) => printTupleType(~inline=false, types)
    | Ptyp_object(fields, openFlag) =>
      printBsObjectSugar(~inline=false, fields, openFlag)
    | Ptyp_poly(stringLocs, typ) =>
      Doc.concat(list(
        Doc.join(
          ~sep=Doc.space,
          List.map(({Location.txt: txt}) => Doc.text("'" ++ txt), stringLocs),
        ),
        Doc.dot,
        Doc.space,
        printTypExpr(typ),
      ))
    | Ptyp_package(packageType) =>
      printPackageType(~printModuleKeywordAndParens=true, packageType)
    | Ptyp_class(_) => failwith("classes are not supported in types")
    | Ptyp_variant(_) =>
      failwith("Polymorphic variants currently not supported")
    }
    
    let shouldPrintItsOwnAttributes = switch typExpr.ptyp_desc {
    | Ptyp_arrow(_)
      | Ptyp_constr({txt: Longident.Ldot(Longident.Lident("Js"), "t")}, _) =>
      true
    | _ => false
    }
    
    switch typExpr.ptyp_attributes {
    | list(_, ..._) as attrs when !shouldPrintItsOwnAttributes =>
      Doc.group(Doc.concat(list(printAttributes(attrs), renderedType)))
    | _ => renderedType
    }
  }
  
  and printBsObjectSugar = (~inline, fields, openFlag) => {
    let flag = switch openFlag {
    | Asttypes.Closed => Doc.nil
    | Open => Doc.dotdot
    }
    
    let doc = Doc.concat(list(
      Doc.lbrace,
      flag,
      Doc.indent(
        Doc.concat(list(
          Doc.softLine,
          Doc.join(
            ~sep=Doc.concat(list(Doc.comma, Doc.line)),
            List.map(printObjectField, fields),
          ),
        )),
      ),
      Doc.trailingComma,
      Doc.softLine,
      Doc.rbrace,
    ))
    if inline {
      doc
    } else {
      Doc.group(doc)
    }
  }
  
  and printTupleType = (~inline, types: list<Parsetree.core_type>) => {
    let tuple = Doc.concat(list(
      Doc.text("/"),
      Doc.indent(
        Doc.concat(list(
          Doc.softLine,
          Doc.join(
            ~sep=Doc.concat(list(Doc.comma, Doc.line)),
            List.map(printTypExpr, types),
          ),
        )),
      ),
      Doc.softLine,
      Doc.text("/"),
    ))
    
    if inline === false {
      Doc.group(tuple)
    } else {
      tuple
    }
  }
  
  and printObjectField = (field: Parsetree.object_field) =>
    switch field {
    | Otag(labelLoc, attrs, typ) =>
      Doc.concat(list(
        Doc.text("\"" ++ labelLoc.txt ++ "\""),
        Doc.text(": "),
        printTypExpr(typ),
      ))
    | _ => Doc.nil
    }
  
  and printTypeParameter = (/attrs, lbl, typ/) => {
    let /isUncurried, attrs/ = ParsetreeViewer.processUncurriedAttribute(attrs)
    let uncurried = if isUncurried {
      Doc.concat(list(Doc.dot, Doc.space))
    } else {
      Doc.nil
    }
    let attrs = switch attrs {
    | list() => Doc.nil
    | attrs =>
      Doc.concat(list(
        Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
        Doc.line,
      ))
    }
    let label = switch lbl {
    | Asttypes.Nolabel => Doc.nil
    | Labelled(lbl) => Doc.text("~" ++ lbl ++ ": ")
    | Optional(lbl) => Doc.text("~" ++ lbl ++ ": ")
    }
    
    let optionalIndicator = switch lbl {
    | Asttypes.Nolabel | Labelled(_) => Doc.nil
    | Optional(lbl) => Doc.text("=?")
    }
    
    Doc.group(
      Doc.concat(list(
        uncurried,
        attrs,
        label,
        printTypExpr(typ),
        optionalIndicator,
      )),
    )
  }
  
  and printValueBinding = (~recFlag, i, vb) => {
    let isGhost = ParsetreeViewer.isGhostUnitBinding(i, vb)
    let header = if isGhost {
      Doc.nil
    } else if i === 0 {
      Doc.concat(list(Doc.text("let "), recFlag))
    } else {
      Doc.text("and ")
    }
    
    let printedExpr = {
      let exprDoc = printExpression(vb.pvb_expr)
      let needsParens = switch vb.pvb_expr.pexp_desc {
      | Pexp_constraint(
          {pexp_desc: Pexp_pack(_)},
          {ptyp_desc: Ptyp_package(_)},
        ) =>
        false
      | Pexp_constraint(_) => true
      | _ => false
      }
      
      if needsParens {
        addParens(exprDoc)
      } else {
        exprDoc
      }
    }
    
    if isGhost {
      printedExpr
    } else {
      let shouldIndent =
        ParsetreeViewer.isBinaryExpression(vb.pvb_expr) ||
        switch vb.pvb_expr {
        | {
            pexp_attributes: list(/{Location.txt: "ns.ternary"}, _/),
            pexp_desc: Pexp_ifthenelse(ifExpr, _, _),
          } =>
          ParsetreeViewer.isBinaryExpression(ifExpr) ||
          ParsetreeViewer.hasAttributes(ifExpr.pexp_attributes)
        | {pexp_desc: Pexp_newtype(_)} => false
        | e =>
          ParsetreeViewer.hasAttributes(e.pexp_attributes) ||
          ParsetreeViewer.isArrayAccess(e)
        }
      
      Doc.concat(list(
        printAttributes(~loc=vb.pvb_loc, vb.pvb_attributes),
        header,
        printPattern(vb.pvb_pat),
        Doc.text(" ="),
        if shouldIndent {
          Doc.indent(Doc.concat(list(Doc.line, printedExpr)))
        } else {
          Doc.concat(list(Doc.space, printedExpr))
        },
      ))
    }
  }
  
  and printPackageType = (
    ~printModuleKeywordAndParens,
    packageType: Parsetree.package_type,
  ) => {
    let doc = switch packageType {
    | /longidentLoc, list()/ =>
      Doc.group(Doc.concat(list(printLongident(longidentLoc.txt))))
    | /longidentLoc, packageConstraints/ =>
      Doc.group(
        Doc.concat(list(
          printLongident(longidentLoc.txt),
          printPackageConstraints(packageConstraints),
          Doc.softLine,
        )),
      )
    }
    
    if printModuleKeywordAndParens {
      Doc.concat(list(Doc.text("module("), doc, Doc.rparen))
    } else {
      doc
    }
  }
  
  and printPackageConstraints = packageConstraints =>
    Doc.concat(list(
      Doc.text(" with"),
      Doc.indent(
        Doc.concat(list(
          Doc.line,
          Doc.join(
            ~sep=Doc.line,
            List.mapi(printPackageconstraint, packageConstraints),
          ),
        )),
      ),
    ))
  
  and printPackageconstraint = (i, /longidentLoc, typ/) => {
    let prefix = if i === 0 {
      Doc.text("type ")
    } else {
      Doc.text("and type ")
    }
    Doc.concat(list(
      prefix,
      printLongident(longidentLoc.Location.txt),
      Doc.text(" = "),
      printTypExpr(typ),
    ))
  }
  
  and printExtension = (/stringLoc, payload/) => {
    let extName = Doc.text("%" ++ stringLoc.Location.txt)
    switch payload {
    | PStr(list({pstr_desc: Pstr_eval(expr, attrs)})) =>
      let exprDoc = printExpression(expr)
      let needsParens = switch attrs {
      | list() => false
      | _ => true
      }
      Doc.group(
        Doc.concat(list(
          extName,
          addParens(
            Doc.concat(list(
              printAttributes(attrs),
              if needsParens {
                addParens(exprDoc)
              } else {
                exprDoc
              },
            )),
          ),
        )),
      )
    | _ => extName
    }
  }
  
  and printPattern = (p: Parsetree.pattern) => {
    let patternWithoutAttributes = switch p.ppat_desc {
    | Ppat_any => Doc.text("_")
    | Ppat_var(stringLoc) => Doc.text(stringLoc.txt)
    | Ppat_constant(c) => printConstant(c)
    | Ppat_tuple(patterns) =>
      Doc.group(
        Doc.concat(list(
          Doc.text("/"),
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.text(","), Doc.line)),
                List.map(printPattern, patterns),
              ),
            )),
          ),
          Doc.softLine,
          Doc.text("/"),
        )),
      )
    | Ppat_array(patterns) =>
      Doc.group(
        Doc.concat(list(
          Doc.text("["),
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.text(","), Doc.line)),
                List.map(printPattern, patterns),
              ),
            )),
          ),
          Doc.ifBreaks(Doc.text(","), Doc.nil),
          Doc.softLine,
          Doc.text("]"),
        )),
      )
    | Ppat_construct({txt: Longident.Lident("[]")}, _) => Doc.text("list()")
    | Ppat_construct({txt: Longident.Lident("::")}, _) =>
      let /patterns, tail/ = collectPatternsFromListConstruct(list(), p)
      let shouldHug = switch /patterns, tail/ {
      | /
          list(pat),
          {ppat_desc: Ppat_construct({txt: Longident.Lident("[]")}, _)}
        / when ParsetreeViewer.isHuggablePattern(pat) =>
        true
      | _ => false
      }
      
      let children = Doc.concat(list(
        if shouldHug {
          Doc.nil
        } else {
          Doc.softLine
        },
        Doc.join(
          ~sep=Doc.concat(list(Doc.text(","), Doc.line)),
          List.map(printPattern, patterns),
        ),
        switch tail.Parsetree.ppat_desc {
        | Ppat_construct({txt: Longident.Lident("[]")}, _) => Doc.nil
        | _ =>
          Doc.concat(list(
            Doc.text(","),
            Doc.line,
            Doc.text("..."),
            printPattern(tail),
          ))
        },
      ))
      Doc.group(
        Doc.concat(list(
          Doc.text("list("),
          if shouldHug {
            children
          } else {
            Doc.concat(list(
              Doc.indent(children),
              Doc.ifBreaks(Doc.text(","), Doc.nil),
              Doc.softLine,
            ))
          },
          Doc.text(")"),
        )),
      )
    | Ppat_construct(constrName, constructorArgs) =>
      let constrName = printLongident(constrName.txt)
      switch constructorArgs {
      | None => constrName
      | Some(args) =>
        let args = switch args.ppat_desc {
        | Ppat_construct({txt: Longident.Lident("()")}, None) => list(Doc.nil)
        | Ppat_tuple(patterns) => List.map(printPattern, patterns)
        | _ => list(printPattern(args))
        }
        
        Doc.group(
          Doc.concat(list(
            constrName,
            Doc.text("("),
            Doc.indent(
              Doc.concat(list(
                Doc.softLine,
                Doc.join(~sep=Doc.concat(list(Doc.text(","), Doc.line)), args),
              )),
            ),
            Doc.ifBreaks(Doc.text(","), Doc.nil),
            Doc.softLine,
            Doc.text(")"),
          )),
        )
      }
    | Ppat_record(rows, openFlag) =>
      Doc.group(
        Doc.concat(list(
          Doc.text("{"),
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.text(","), Doc.line)),
                List.map(printPatternRecordRow, rows),
              ),
              switch openFlag {
              | Open => Doc.concat(list(Doc.text(","), Doc.line, Doc.text("_")))
              | Closed => Doc.nil
              },
            )),
          ),
          Doc.ifBreaks(Doc.text(","), Doc.nil),
          Doc.softLine,
          Doc.text("}"),
        )),
      )
    | Ppat_exception(p) =>
      let needsParens = switch p.ppat_desc {
      | Ppat_or(_, _) | Ppat_alias(_, _) => true
      | _ => false
      }
      
      let pat = {
        let p = printPattern(p)
        if needsParens {
          Doc.concat(list(Doc.text("("), p, Doc.text(")")))
        } else {
          p
        }
      }
      
      Doc.group(Doc.concat(list(Doc.text("exception"), Doc.line, pat)))
    | Ppat_or(p1, p2) =>
      let p1 = {
        let p = printPattern(p1)
        switch p1.ppat_desc {
        | Ppat_or(_, _) => Doc.concat(list(Doc.text("("), p, Doc.text(")")))
        | _ => p
        }
      }
      
      let p2 = {
        let p = printPattern(p2)
        switch p2.ppat_desc {
        | Ppat_or(_, _) => Doc.concat(list(Doc.text("("), p, Doc.text(")")))
        | _ => p
        }
      }
      
      Doc.group(Doc.concat(list(p1, Doc.line, Doc.text("| "), p2)))
    | Ppat_extension(ext) => printExtension(ext)
    | Ppat_lazy(p) =>
      let needsParens = switch p.ppat_desc {
      | Ppat_or(_, _) | Ppat_alias(_, _) => true
      | _ => false
      }
      
      let pat = {
        let p = printPattern(p)
        if needsParens {
          Doc.concat(list(Doc.text("("), p, Doc.text(")")))
        } else {
          p
        }
      }
      
      Doc.concat(list(Doc.text("lazy "), pat))
    | Ppat_alias(p, aliasLoc) =>
      let needsParens = switch p.ppat_desc {
      | Ppat_or(_, _) | Ppat_alias(_, _) => true
      | _ => false
      }
      
      let renderedPattern = {
        let p = printPattern(p)
        if needsParens {
          Doc.concat(list(Doc.text("("), p, Doc.text(")")))
        } else {
          p
        }
      }
      
      Doc.concat(list(
        renderedPattern,
        Doc.text(" as "),
        Doc.text(aliasLoc.txt),
      ))
    | Ppat_constraint(
        {ppat_desc: Ppat_unpack(stringLoc)},
        {ptyp_desc: Ptyp_package(packageType)},
      ) =>
      Doc.concat(list(
        Doc.text("module("),
        Doc.text(stringLoc.txt),
        Doc.text(": "),
        printPackageType(~printModuleKeywordAndParens=false, packageType),
        Doc.rparen,
      ))
    | Ppat_constraint(pattern, typ) =>
      Doc.concat(list(printPattern(pattern), Doc.text(": "), printTypExpr(typ)))
    | Ppat_unpack(stringLoc) =>
      Doc.concat(list(Doc.text("module("), Doc.text(stringLoc.txt), Doc.rparen))
    | _ => failwith("unsupported pattern")
    }
    
    switch p.ppat_attributes {
    | list() => patternWithoutAttributes
    | attrs =>
      Doc.group(
        Doc.concat(list(printAttributes(attrs), patternWithoutAttributes)),
      )
    }
  }
  
  and printPatternRecordRow = row =>
    switch row {
    | /
        {Location.txt: Longident.Lident(ident)},
        {Parsetree.ppat_desc: Ppat_var({txt, _})}
      / when ident == txt =>
      Doc.text(ident)
    | /longident, pattern/ =>
      Doc.group(
        Doc.concat(list(
          printLongident(longident.txt),
          Doc.text(": "),
          Doc.indent(Doc.concat(list(Doc.softLine, printPattern(pattern)))),
        )),
      )
    }
  
  and printExpression = (e: Parsetree.expression) => {
    let printedExpression = switch e.pexp_desc {
    | Parsetree.Pexp_constant(c) => printConstant(c)
    | Pexp_construct(_)
      when ParsetreeViewer.hasJsxAttribute(e.pexp_attributes) =>
      printJsxFragment(e)
    | Pexp_construct({txt: Longident.Lident("()")}, _) => Doc.text("()")
    | Pexp_construct({txt: Longident.Lident("[]")}, _) => Doc.text("list()")
    | Pexp_construct({txt: Longident.Lident("::")}, _) =>
      let /expressions, spread/ = ParsetreeViewer.collectListExpressions(e)
      let spreadDoc = switch spread {
      | Some(expr) =>
        Doc.concat(list(
          Doc.text(","),
          Doc.line,
          Doc.dotdotdot,
          printExpression(expr),
        ))
      | None => Doc.nil
      }
      
      Doc.group(
        Doc.concat(list(
          Doc.text("list("),
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.text(","), Doc.line)),
                List.map(printExpression, expressions),
              ),
              spreadDoc,
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        )),
      )
    | Pexp_construct(longidentLoc, args) =>
      let constr = printLongident(longidentLoc.txt)
      let args = switch args {
      | None => Doc.nil
      | Some({pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, _)}) =>
        Doc.text("()")
      | Some({pexp_desc: Pexp_tuple(args)}) =>
        Doc.concat(list(
          Doc.lparen,
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                List.map(printExpression, args),
              ),
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        ))
      | Some(arg) =>
        let argDoc = printExpression(arg)
        let shouldHug = ParsetreeViewer.isHuggableExpression(arg)
        Doc.concat(list(
          Doc.lparen,
          if shouldHug {
            argDoc
          } else {
            Doc.concat(list(
              Doc.indent(Doc.concat(list(Doc.softLine, argDoc))),
              Doc.trailingComma,
              Doc.softLine,
            ))
          },
          Doc.rparen,
        ))
      }
      
      Doc.group(Doc.concat(list(constr, args)))
    | Pexp_ident(longidentLoc) => printLongident(longidentLoc.txt)
    | Pexp_tuple(exprs) =>
      Doc.group(
        Doc.concat(list(
          Doc.text("/"),
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.text(","), Doc.line)),
                List.map(printExpression, exprs),
              ),
            )),
          ),
          Doc.ifBreaks(Doc.text(","), Doc.nil),
          Doc.softLine,
          Doc.text("/"),
        )),
      )
    | Pexp_array(list()) => Doc.text("[]")
    | Pexp_array(exprs) =>
      Doc.group(
        Doc.concat(list(
          Doc.lbracket,
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.text(","), Doc.line)),
                List.map(printExpression, exprs),
              ),
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rbracket,
        )),
      )
    | Pexp_record(rows, spreadExpr) =>
      let spread = switch spreadExpr {
      | None => Doc.nil
      | Some(expr) =>
        Doc.concat(list(
          Doc.dotdotdot,
          printExpression(expr),
          Doc.comma,
          Doc.line,
        ))
      }
      
      let forceBreak =
        e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum
      
      Doc.breakableGroup(
        ~forceBreak,
        Doc.concat(list(
          Doc.lbrace,
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              spread,
              Doc.join(
                ~sep=Doc.concat(list(Doc.text(","), Doc.line)),
                List.map(printRecordRow, rows),
              ),
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rbrace,
        )),
      )
    | Pexp_extension(extension) =>
      switch extension {
      | /
          {txt: "bs.obj"},
          PStr(
            list({
              pstr_loc: loc,
              pstr_desc: Pstr_eval({pexp_desc: Pexp_record(rows, _)}, list()),
            }),
          )
        / =>
        let forceBreak = loc.loc_start.pos_lnum < loc.loc_end.pos_lnum
        
        Doc.breakableGroup(
          ~forceBreak,
          Doc.concat(list(
            Doc.lbrace,
            Doc.indent(
              Doc.concat(list(
                Doc.softLine,
                Doc.join(
                  ~sep=Doc.concat(list(Doc.text(","), Doc.line)),
                  List.map(printBsObjectRow, rows),
                ),
              )),
            ),
            Doc.trailingComma,
            Doc.softLine,
            Doc.rbrace,
          )),
        )
      | extension => printExtension(extension)
      }
    | Pexp_apply(_) =>
      if ParsetreeViewer.isUnaryExpression(e) {
        printUnaryExpression(e)
      } else if ParsetreeViewer.isBinaryExpression(e) {
        printBinaryExpression(e)
      } else {
        printPexpApply(e)
      }
    | Pexp_unreachable => Doc.dot
    | Pexp_field(expr, longidentLoc) =>
      let lhs = {
        let doc = printExpression(expr)
        if Parens.fieldExpr(expr) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.concat(list(lhs, Doc.dot, printLongident(longidentLoc.txt)))
    | Pexp_setfield(expr1, longidentLoc, expr2) =>
      printSetFieldExpr(e.pexp_attributes, expr1, longidentLoc, expr2)
    | Pexp_ifthenelse(ifExpr, thenExpr, elseExpr) =>
      if ParsetreeViewer.isTernaryExpr(e) {
        let /parts, alternate/ = ParsetreeViewer.collectTernaryParts(e)
        let ternaryDoc = switch parts {
        | list(/condition1, consequent1/, ...rest) =>
          Doc.group(
            Doc.concat(list(
              printTernaryOperand(condition1),
              Doc.indent(
                Doc.concat(list(
                  Doc.line,
                  Doc.indent(
                    Doc.concat(list(
                      Doc.text("? "),
                      printTernaryOperand(consequent1),
                    )),
                  ),
                  Doc.concat(
                    List.map(
                      (/condition, consequent/) =>
                        Doc.concat(list(
                          Doc.line,
                          Doc.text(": "),
                          printTernaryOperand(condition),
                          Doc.line,
                          Doc.text("? "),
                          printTernaryOperand(consequent),
                        )),
                      rest,
                    ),
                  ),
                  Doc.line,
                  Doc.text(": "),
                  Doc.indent(printTernaryOperand(alternate)),
                )),
              ),
            )),
          )
        | _ => Doc.nil
        }
        
        let attrs = ParsetreeViewer.filterTernaryAttributes(e.pexp_attributes)
        let needsParens = switch attrs {
        | list() => false
        | _ => true
        }
        Doc.concat(list(
          printAttributes(attrs),
          if needsParens {
            addParens(ternaryDoc)
          } else {
            ternaryDoc
          },
        ))
      } else {
        let /ifs, elseExpr/ = ParsetreeViewer.collectIfExpressions(e)
        let ifDocs = Doc.join(
          ~sep=Doc.space,
          List.mapi(
            (i, /ifExpr, thenExpr/) => {
              let ifTxt = if i > 0 {
                Doc.text("else if ")
              } else {
                Doc.text("if ")
              }
              let condition = printExpression(ifExpr)
              Doc.concat(list(
                ifTxt,
                Doc.group(Doc.ifBreaks(addParens(condition), condition)),
                Doc.space,
                printExpressionBlock(~braces=true, thenExpr),
              ))
            },
            ifs,
          ),
        )
        let elseDoc = switch elseExpr {
        | None => Doc.nil
        | Some(expr) =>
          Doc.concat(list(
            Doc.text(" else "),
            printExpressionBlock(~braces=true, expr),
          ))
        }
        
        Doc.concat(list(printAttributes(e.pexp_attributes), ifDocs, elseDoc))
      }
    | Pexp_while(expr1, expr2) =>
      let condition = printExpression(expr1)
      Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list(
          Doc.text("while "),
          Doc.group(Doc.ifBreaks(addParens(condition), condition)),
          Doc.space,
          printExpressionBlock(~braces=true, expr2),
        )),
      )
    | Pexp_for(pattern, fromExpr, toExpr, directionFlag, body) =>
      Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list(
          Doc.text("for "),
          printPattern(pattern),
          Doc.text(" in "),
          printExpression(fromExpr),
          printDirectionFlag(directionFlag),
          printExpression(toExpr),
          Doc.space,
          printExpressionBlock(~braces=true, body),
        )),
      )
    | Pexp_constraint(
        {pexp_desc: Pexp_pack(modExpr)},
        {ptyp_desc: Ptyp_package(packageType)},
      ) =>
      Doc.group(
        Doc.concat(list(
          Doc.text("module("),
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              printModExpr(modExpr),
              Doc.text(": "),
              printPackageType(~printModuleKeywordAndParens=false, packageType),
            )),
          ),
          Doc.softLine,
          Doc.rparen,
        )),
      )
    | Pexp_constraint(expr, typ) =>
      Doc.concat(list(printExpression(expr), Doc.text(": "), printTypExpr(typ)))
    | Pexp_letmodule({txt: modName}, modExpr, expr) =>
      printExpressionBlock(~braces=true, e)
    | Pexp_letexception(extensionConstructor, expr) =>
      printExpressionBlock(~braces=true, e)
    | Pexp_assert(expr) =>
      let rhs = {
        let doc = printExpression(expr)
        if Parens.lazyOrAssertExprRhs(expr) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.concat(list(Doc.text("assert "), rhs))
    | Pexp_lazy(expr) =>
      let rhs = {
        let doc = printExpression(expr)
        if Parens.lazyOrAssertExprRhs(expr) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.concat(list(Doc.text("lazy "), rhs))
    | Pexp_open(overrideFlag, longidentLoc, expr) =>
      printExpressionBlock(~braces=true, e)
    | Pexp_pack(modExpr) =>
      Doc.group(
        Doc.concat(list(
          Doc.text("module("),
          Doc.indent(Doc.concat(list(Doc.softLine, printModExpr(modExpr)))),
          Doc.softLine,
          Doc.rparen,
        )),
      )
    | Pexp_sequence(_) => printExpressionBlock(~braces=true, e)
    | Pexp_let(_) => printExpressionBlock(~braces=true, e)
    | Pexp_fun(_) | Pexp_newtype(_) =>
      let /attrsOnArrow, parameters, returnExpr/ = ParsetreeViewer.funExpr(e)
      let /uncurried, attrs/ = ParsetreeViewer.processUncurriedAttribute(
        attrsOnArrow,
      )
      
      let /returnExpr, typConstraint/ = switch returnExpr.pexp_desc {
      | Pexp_constraint(expr, typ) => /expr, Some(typ)/
      | _ => /returnExpr, None/
      }
      
      let parametersDoc = printExprFunParameters(
        ~inCallback=false,
        ~uncurried,
        parameters,
      )
      let returnExprDoc = {
        let shouldInline = switch returnExpr.pexp_desc {
        | ((Pexp_array(_) | Pexp_tuple(_)) | Pexp_construct(_, Some(_)))
          | Pexp_record(_) =>
          true
        | _ => false
        }
        
        let shouldIndent = switch returnExpr.pexp_desc {
        | ((Pexp_sequence(_) | Pexp_let(_)) | Pexp_letmodule(_))
          | Pexp_letexception(_) =>
          false
        | _ => true
        }
        
        let returnDoc = printExpression(returnExpr)
        if shouldInline {
          Doc.concat(list(Doc.space, returnDoc))
        } else {
          Doc.group(
            if shouldIndent {
              Doc.indent(Doc.concat(list(Doc.line, returnDoc)))
            } else {
              Doc.concat(list(Doc.space, returnDoc))
            },
          )
        }
      }
      
      let typConstraintDoc = switch typConstraint {
      | Some(typ) => Doc.concat(list(Doc.text(": "), printTypExpr(typ)))
      | _ => Doc.nil
      }
      
      let attrs = switch attrs {
      | list() => Doc.nil
      | attrs =>
        Doc.concat(list(
          Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
          Doc.space,
        ))
      }
      
      Doc.group(
        Doc.concat(list(
          attrs,
          parametersDoc,
          typConstraintDoc,
          Doc.text(" =>"),
          returnExprDoc,
        )),
      )
    | Pexp_try(expr, cases) =>
      Doc.concat(list(
        Doc.text("try "),
        printExpression(expr),
        Doc.text(" catch "),
        printCases(cases),
      ))
    | Pexp_match(expr, cases) =>
      Doc.concat(list(
        Doc.text("switch "),
        printExpression(expr),
        Doc.space,
        printCases(cases),
      ))
    | _ => failwith("expression not yet implemented in printer")
    }
    
    let shouldPrintItsOwnAttributes = switch e.pexp_desc {
    | (((Pexp_apply(_) | Pexp_fun(_)) | Pexp_newtype(_)) | Pexp_setfield(_))
      | Pexp_ifthenelse(_) =>
      true
    | Pexp_construct(_)
      when ParsetreeViewer.hasJsxAttribute(e.pexp_attributes) =>
      true
    | _ => false
    }
    
    switch e.pexp_attributes {
    | list() => printedExpression
    | attrs when !shouldPrintItsOwnAttributes =>
      Doc.group(Doc.concat(list(printAttributes(attrs), printedExpression)))
    | _ => printedExpression
    }
  }
  
  and printPexpFun = (~inCallback, e) => {
    let /attrsOnArrow, parameters, returnExpr/ = ParsetreeViewer.funExpr(e)
    let /uncurried, attrs/ = ParsetreeViewer.processUncurriedAttribute(
      attrsOnArrow,
    )
    
    let /returnExpr, typConstraint/ = switch returnExpr.pexp_desc {
    | Pexp_constraint(expr, typ) => /expr, Some(typ)/
    | _ => /returnExpr, None/
    }
    
    let parametersDoc = printExprFunParameters(
      ~inCallback,
      ~uncurried,
      parameters,
    )
    let returnShouldIndent = switch returnExpr.pexp_desc {
    | ((Pexp_sequence(_) | Pexp_let(_)) | Pexp_letmodule(_))
      | Pexp_letexception(_) =>
      false
    | _ => true
    }
    
    let returnExprDoc = {
      let shouldInline = switch returnExpr.pexp_desc {
      | ((Pexp_array(_) | Pexp_tuple(_)) | Pexp_construct(_, Some(_)))
        | Pexp_record(_) =>
        true
      | _ => false
      }
      
      let returnDoc = printExpression(returnExpr)
      if shouldInline {
        Doc.concat(list(Doc.space, returnDoc))
      } else {
        Doc.group(
          if returnShouldIndent {
            Doc.concat(list(
              Doc.indent(Doc.concat(list(Doc.line, returnDoc))),
              if inCallback {
                Doc.softLine
              } else {
                Doc.nil
              },
            ))
          } else {
            Doc.concat(list(Doc.space, returnDoc))
          },
        )
      }
    }
    
    let typConstraintDoc = switch typConstraint {
    | Some(typ) => Doc.concat(list(Doc.text(": "), printTypExpr(typ)))
    | _ => Doc.nil
    }
    
    let attrs = switch attrs {
    | list() => Doc.nil
    | attrs =>
      Doc.concat(list(
        Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
        Doc.space,
      ))
    }
    
    Doc.group(
      Doc.concat(list(
        attrs,
        parametersDoc,
        typConstraintDoc,
        Doc.text(" =>"),
        returnExprDoc,
      )),
    )
  }
  
  and printTernaryOperand = expr => {
    let doc = printExpression(expr)
    if Parens.ternaryOperand(expr) {
      addParens(doc)
    } else {
      doc
    }
  }
  
  and printSetFieldExpr = (attrs, lhs, longidentLoc, rhs) => {
    let rhsDoc = {
      let doc = printExpression(rhs)
      if Parens.setFieldExprRhs(rhs) {
        addParens(doc)
      } else {
        doc
      }
    }
    
    let lhsDoc = {
      let doc = printExpression(lhs)
      if Parens.fieldExpr(lhs) {
        addParens(doc)
      } else {
        doc
      }
    }
    
    let shouldIndent = ParsetreeViewer.isBinaryExpression(rhs)
    let doc = Doc.concat(list(
      lhsDoc,
      Doc.dot,
      printLongident(longidentLoc.txt),
      Doc.text(" ="),
      if shouldIndent {
        Doc.group(Doc.indent(Doc.concat(list(Doc.line, rhsDoc))))
      } else {
        Doc.concat(list(Doc.space, rhsDoc))
      },
    ))
    switch attrs {
    | list() => doc
    | attrs => Doc.group(Doc.concat(list(printAttributes(attrs), doc)))
    }
  }
  
  and printUnaryExpression = expr => {
    let printUnaryOperator = op =>
      Doc.text(
        switch op {
        | "~+" => "+"
        | "~+." => "+."
        | "~-" => "-"
        | "~-." => "-."
        | "not" => "!"
        | "!" => "&"
        | _ => assert false
        },
      )
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list(/Nolabel, operand/),
      ) =>
      let printedOperand = {
        let doc = printExpression(operand)
        if Parens.unaryExprOperand(operand) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      Doc.concat(list(printUnaryOperator(operator), printedOperand))
    | _ => assert false
    }
  }
  
  and printBinaryExpression = (expr: Parsetree.expression) => {
    let printBinaryOperator = (~inlineRhs, operator) => {
      let operatorTxt = switch operator {
      | "|." => "->"
      | "^" => "++"
      | "=" => "=="
      | "==" => "==="
      | "<>" => "!="
      | "!=" => "!=="
      | txt => txt
      }
      
      let spacingBeforeOperator = if operator == "|." {
        Doc.softLine
      } else if operator == "|>" {
        Doc.line
      } else {
        Doc.space
      }
      
      let spacingAfterOperator = if operator == "|." {
        Doc.nil
      } else if operator == "|>" {
        Doc.space
      } else if inlineRhs {
        Doc.space
      } else {
        Doc.line
      }
      
      Doc.concat(list(
        spacingBeforeOperator,
        Doc.text(operatorTxt),
        spacingAfterOperator,
      ))
    }
    
    let printOperand = (~isLhs, expr, parentOperator) => {
      let rec flatten = (~isLhs, expr, parentOperator) =>
        if ParsetreeViewer.isBinaryExpression(expr) {
          switch expr {
          | {
              pexp_desc: 
                Pexp_apply(
                  {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
                  list(/_, left/, /_, right/),
                ),
            } =>
            if (
              ParsetreeViewer.flattenableOperators(parentOperator, operator) &&
              !ParsetreeViewer.hasAttributes(expr.pexp_attributes)
            ) {
              let leftPrinted = flatten(~isLhs=true, left, operator)
              let rightPrinted = {
                let /
                  _,
                  rightAttrs
                / = ParsetreeViewer.partitionPrinteableAttributes(
                  right.pexp_attributes,
                )
                
                let doc = printExpression({
                  ...right,
                  pexp_attributes: rightAttrs,
                })
                let doc = if Parens.flattenOperandRhs(parentOperator, right) {
                  Doc.concat(list(Doc.lparen, doc, Doc.rparen))
                } else {
                  doc
                }
                
                let printeableAttrs = ParsetreeViewer.filterPrinteableAttributes(
                  right.pexp_attributes,
                )
                
                Doc.concat(list(printAttributes(printeableAttrs), doc))
              }
              
              Doc.concat(list(
                leftPrinted,
                printBinaryOperator(~inlineRhs=false, operator),
                rightPrinted,
              ))
            } else {
              let doc = printExpression({...expr, pexp_attributes: list()})
              let doc = if (
                Parens.subBinaryExprOperand(parentOperator, operator) ||
                ((expr.pexp_attributes != list()) &&
                  (ParsetreeViewer.isBinaryExpression(expr) ||
                  ParsetreeViewer.isTernaryExpr(expr)))
              ) {
                Doc.concat(list(Doc.lparen, doc, Doc.rparen))
              } else {
                doc
              }
              Doc.concat(list(printAttributes(expr.pexp_attributes), doc))
            }
          | _ => assert false
          }
        } else {
          switch expr.pexp_desc {
          | Pexp_setfield(lhs, field, rhs) =>
            let doc = printSetFieldExpr(expr.pexp_attributes, lhs, field, rhs)
            if isLhs {
              addParens(doc)
            } else {
              doc
            }
          | Pexp_apply(
              {pexp_desc: Pexp_ident({txt: Longident.Lident("#=")})},
              list(/Nolabel, lhs/, /Nolabel, rhs/),
            ) =>
            let rhsDoc = printExpression(rhs)
            let lhsDoc = printExpression(lhs)
            
            let shouldIndent = ParsetreeViewer.isBinaryExpression(rhs)
            let doc = Doc.group(
              Doc.concat(list(
                lhsDoc,
                Doc.text(" ="),
                if shouldIndent {
                  Doc.group(Doc.indent(Doc.concat(list(Doc.line, rhsDoc))))
                } else {
                  Doc.concat(list(Doc.space, rhsDoc))
                },
              )),
            )
            let doc = switch expr.pexp_attributes {
            | list() => doc
            | attrs => Doc.group(Doc.concat(list(printAttributes(attrs), doc)))
            }
            
            if isLhs {
              addParens(doc)
            } else {
              doc
            }
          | _ =>
            let doc = printExpression(expr)
            if Parens.binaryExprOperand(~isLhs, expr, parentOperator) {
              addParens(doc)
            } else {
              doc
            }
          }
        }
      
      flatten(~isLhs, expr, parentOperator)
    }
    
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(("|." | "|>") as op)})},
        list(/Nolabel, lhs/, /Nolabel, rhs/),
      )
      when !(
        ParsetreeViewer.isBinaryExpression(lhs) ||
        ParsetreeViewer.isBinaryExpression(rhs)
      ) =>
      let lhsDoc = printOperand(~isLhs=true, lhs, op)
      let rhsDoc = printOperand(~isLhs=false, rhs, op)
      Doc.concat(list(
        lhsDoc,
        switch op {
        | "|." => Doc.text("->")
        | "|>" => Doc.text(" |> ")
        | _ => assert false
        },
        rhsDoc,
      ))
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident(operator)})},
        list(/Nolabel, lhs/, /Nolabel, rhs/),
      ) =>
      let right = {
        let operatorWithRhs = Doc.concat(list(
          printBinaryOperator(
            ~inlineRhs=ParsetreeViewer.shouldInlineRhsBinaryExpr(rhs),
            operator,
          ),
          printOperand(~isLhs=false, rhs, operator),
        ))
        if ParsetreeViewer.shouldIndentBinaryExpr(expr) {
          Doc.group(Doc.indent(operatorWithRhs))
        } else {
          operatorWithRhs
        }
      }
      
      let doc = Doc.group(
        Doc.concat(list(printOperand(~isLhs=true, lhs, operator), right)),
      )
      Doc.concat(list(
        printAttributes(expr.pexp_attributes),
        if Parens.binaryExpr(expr) {
          addParens(doc)
        } else {
          doc
        },
      ))
    | _ => Doc.nil
    }
  }
  
  and printPexpApply = expr =>
    switch expr.pexp_desc {
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident("##")})},
        list(/Nolabel, parentExpr/, /Nolabel, memberExpr/),
      ) =>
      let member = {
        let memberDoc = printExpression(memberExpr)
        Doc.concat(list(Doc.text("\""), memberDoc, Doc.text("\"")))
      }
      
      Doc.group(
        Doc.concat(list(
          printAttributes(expr.pexp_attributes),
          printExpression(parentExpr),
          Doc.lbracket,
          member,
          Doc.rbracket,
        )),
      )
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Lident("#=")})},
        list(/Nolabel, lhs/, /Nolabel, rhs/),
      ) =>
      let rhsDoc = printExpression(rhs)
      
      let shouldIndent = ParsetreeViewer.isBinaryExpression(rhs)
      let doc = Doc.group(
        Doc.concat(list(
          printExpression(lhs),
          Doc.text(" ="),
          if shouldIndent {
            Doc.group(Doc.indent(Doc.concat(list(Doc.line, rhsDoc))))
          } else {
            Doc.concat(list(Doc.space, rhsDoc))
          },
        )),
      )
      switch expr.pexp_attributes {
      | list() => doc
      | attrs => Doc.group(Doc.concat(list(printAttributes(attrs), doc)))
      }
    | Pexp_apply(
        {pexp_desc: Pexp_ident({txt: Longident.Ldot(Lident("Array"), "get")})},
        list(/Nolabel, parentExpr/, /Nolabel, memberExpr/),
      ) =>
      let member = {
        let memberDoc = printExpression(memberExpr)
        let shouldInline = switch memberExpr.pexp_desc {
        | Pexp_constant(_) | Pexp_ident(_) => true
        | _ => false
        }
        
        if shouldInline {
          memberDoc
        } else {
          Doc.concat(list(
            Doc.indent(Doc.concat(list(Doc.softLine, memberDoc))),
            Doc.softLine,
          ))
        }
      }
      
      Doc.group(
        Doc.concat(list(
          printAttributes(expr.pexp_attributes),
          printExpression(parentExpr),
          Doc.lbracket,
          member,
          Doc.rbracket,
        )),
      )
    | Pexp_apply({pexp_desc: Pexp_ident({txt: lident})}, args)
      when ParsetreeViewer.isJsxExpression(expr) =>
      printJsxExpression(lident, args)
    | Pexp_apply(callExpr, args) =>
      let /uncurried, attrs/ = ParsetreeViewer.processUncurriedAttribute(
        expr.pexp_attributes,
      )
      
      let callExprDoc = printExpression(callExpr)
      if ParsetreeViewer.requiresSpecialCallbackPrinting(args) {
        let argsDoc = printArgumentsWithCallback(~uncurried, args)
        Doc.concat(list(printAttributes(attrs), callExprDoc, argsDoc))
      } else {
        let argsDoc = printArguments(~uncurried, args)
        Doc.concat(list(printAttributes(attrs), callExprDoc, argsDoc))
      }
    | _ => assert false
    }
  
  and printJsxExpression = (lident, args) => {
    let name = printJsxName(lident)
    let /formattedProps, children/ = formatJsxProps(args)
    
    let isSelfClosing = switch children {
    | list() => true
    | _ => false
    }
    Doc.group(
      Doc.concat(list(
        Doc.group(
          Doc.concat(list(
            Doc.lessThan,
            name,
            formattedProps,
            if isSelfClosing {
              Doc.concat(list(Doc.line, Doc.text("/>")))
            } else {
              Doc.nil
            },
          )),
        ),
        if isSelfClosing {
          Doc.nil
        } else {
          Doc.concat(list(
            Doc.greaterThan,
            Doc.indent(Doc.concat(list(Doc.line, printJsxChildren(children)))),
            Doc.line,
            Doc.text("</"),
            name,
            Doc.greaterThan,
          ))
        },
      )),
    )
  }
  
  and printJsxFragment = expr => {
    let opening = Doc.text("<>")
    let closing = Doc.text("</>")
    let /children, _/ = ParsetreeViewer.collectListExpressions(expr)
    Doc.group(
      Doc.concat(list(
        opening,
        switch children {
        | list() => Doc.nil
        | children =>
          Doc.indent(Doc.concat(list(Doc.line, printJsxChildren(children))))
        },
        Doc.line,
        closing,
      )),
    )
  }
  
  and printJsxChildren = (children: list<Parsetree.expression>) =>
    Doc.group(
      Doc.join(
        ~sep=Doc.line,
        List.map(
          expr => {
            let exprDoc = printExpression(expr)
            if Parens.jsxChildExpr(expr) {
              addBraces(exprDoc)
            } else {
              exprDoc
            }
          },
          children,
        ),
      ),
    )
  
  and formatJsxProps = args => {
    let rec loop = (props, args) =>
      switch args {
      | list() => /Doc.nil, list()/
      | list(
          /Asttypes.Labelled("children"), children/,
          /
            Asttypes.Nolabel,
            {
              Parsetree.pexp_desc: 
                Pexp_construct({txt: Longident.Lident("()")}, None),
            }
          /,
        ) =>
        let formattedProps = Doc.indent(
          switch props {
          | list() => Doc.nil
          | props =>
            Doc.concat(list(
              Doc.line,
              Doc.group(Doc.join(~sep=Doc.line, props |> List.rev)),
            ))
          },
        )
        let /children, _/ = ParsetreeViewer.collectListExpressions(children)
        /formattedProps, children/
      | list(arg, ...args) =>
        let propDoc = formatJsxProp(arg)
        loop(list(propDoc, ...props), args)
      }
    
    loop(list(), args)
  }
  
  and formatJsxProp = arg =>
    switch arg {
    | /
        (Asttypes.Labelled(lblTxt) | Optional(lblTxt)) as lbl,
        {
          Parsetree.pexp_attributes: list(),
          pexp_desc: Pexp_ident({txt: Longident.Lident(ident)}),
        }
      / when lblTxt == ident =>
      switch lbl {
      | Nolabel => Doc.nil
      | Labelled(lbl) => Doc.text(lbl)
      | Optional(lbl) => Doc.text("?" ++ lbl)
      }
    | /lbl, expr/ =>
      let lblDoc = switch lbl {
      | Asttypes.Labelled(lbl) => Doc.text(lbl ++ "=")
      | Asttypes.Optional(lbl) => Doc.text(lbl ++ "=?")
      | Nolabel => Doc.nil
      }
      
      let exprDoc = printExpression(expr)
      Doc.concat(list(
        lblDoc,
        if Parens.jsxPropExpr(expr) {
          addBraces(exprDoc)
        } else {
          exprDoc
        },
      ))
    }
  
  and printJsxName = lident => {
    let rec flatten = (acc, lident) =>
      switch lident {
      | Longident.Lident(txt) => list(txt, ...acc)
      | Ldot(lident, txt) =>
        let acc = if txt == "createElement" {
          acc
        } else {
          list(txt, ...acc)
        }
        flatten(acc, lident)
      | _ => acc
      }
    
    switch lident {
    | Longident.Lident(txt) => Doc.text(txt)
    | _ as lident =>
      let segments = flatten(list(), lident)
      Doc.join(~sep=Doc.dot, List.map(Doc.text, segments))
    }
  }
  
  and printArgumentsWithCallback = (~uncurried, args) => {
    let rec loop = (acc, args) =>
      switch args {
      | list() => /Doc.nil, Doc.nil/
      | list(/_lbl, expr/) =>
        let callback = printPexpFun(~inCallback=true, expr)
        /Doc.concat(List.rev(acc)), callback/
      | list(arg, ...args) =>
        let argDoc = printArgument(arg)
        loop(list(Doc.line, Doc.comma, argDoc, ...acc), args)
      }
    
    let /printedArgs, callback/ = loop(list(), args)
    
    let fitsOnOneLine = Doc.concat(list(
      if uncurried {
        Doc.text("(.")
      } else {
        Doc.lparen
      },
      Doc.concat(list(printedArgs, callback)),
      Doc.rparen,
    ))
    
    let arugmentsFitOnOneLine = Doc.concat(list(
      if uncurried {
        Doc.text("(.")
      } else {
        Doc.lparen
      },
      Doc.concat(list(
        Doc.softLine,
        printedArgs,
        Doc.breakableGroup(~forceBreak=true, callback),
      )),
      Doc.softLine,
      Doc.rparen,
    ))
    
    let breakAllArgs = printArguments(~uncurried, args)
    Doc.customLayout(list(fitsOnOneLine, arugmentsFitOnOneLine, breakAllArgs))
  }
  
  and printArguments = (
    ~uncurried,
    args: list</Asttypes.arg_label, Parsetree.expression/>,
  ) =>
    switch args {
    | list(/
        Nolabel,
        {pexp_desc: Pexp_construct({txt: Longident.Lident("()")}, _)}
      /) =>
      if uncurried {
        Doc.text("(.)")
      } else {
        Doc.text("()")
      }
    | list(/Nolabel, arg/) when ParsetreeViewer.isHuggableExpression(arg) =>
      Doc.concat(list(
        if uncurried {
          Doc.text("(.")
        } else {
          Doc.lparen
        },
        printExpression(arg),
        Doc.rparen,
      ))
    | args =>
      Doc.group(
        Doc.concat(list(
          if uncurried {
            Doc.text("(.")
          } else {
            Doc.lparen
          },
          Doc.indent(
            Doc.concat(list(
              if uncurried {
                Doc.line
              } else {
                Doc.softLine
              },
              Doc.join(
                ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                List.map(printArgument, args),
              ),
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        )),
      )
    }
  
  and printArgument = (
    /argLbl, arg/: /Asttypes.arg_label, Parsetree.expression/,
  ) =>
    switch /argLbl, arg/ {
    | /
        Asttypes.Labelled(lbl),
        {pexp_desc: Pexp_ident({txt: Longident.Lident(name)})}
      / when lbl == name =>
      Doc.text("~" ++ lbl)
    | /
        Asttypes.Optional(lbl),
        {pexp_desc: Pexp_ident({txt: Longident.Lident(name)})}
      / when lbl == name =>
      Doc.text("~" ++ lbl ++ "?")
    | /lbl, expr/ =>
      let printedLbl = switch argLbl {
      | Asttypes.Nolabel => Doc.nil
      | Asttypes.Labelled(lbl) => Doc.text("~" ++ lbl ++ "=")
      | Asttypes.Optional(lbl) => Doc.text("~" ++ lbl ++ "=?")
      }
      
      let printedExpr = printExpression(expr)
      Doc.concat(list(printedLbl, printedExpr))
    }
  
  and printCases = (cases: list<Parsetree.case>) =>
    Doc.breakableGroup(
      ~forceBreak=true,
      Doc.concat(list(
        Doc.lbrace,
        Doc.concat(list(
          Doc.line,
          Doc.join(~sep=Doc.line, List.map(printCase, cases)),
        )),
        Doc.line,
        Doc.rbrace,
      )),
    )
  
  and printCase = (case: Parsetree.case) => {
    let rhs = switch case.pc_rhs.pexp_desc {
    | (((Pexp_let(_) | Pexp_letmodule(_)) | Pexp_letexception(_))
      | Pexp_open(_))
      | Pexp_sequence(_) =>
      printExpressionBlock(~braces=false, case.pc_rhs)
    | _ => printExpression(case.pc_rhs)
    }
    
    let guard = switch case.pc_guard {
    | None => Doc.nil
    | Some(expr) =>
      Doc.group(
        Doc.concat(list(Doc.line, Doc.text("when "), printExpression(expr))),
      )
    }
    
    Doc.group(
      Doc.concat(list(
        Doc.text("| "),
        Doc.indent(
          Doc.concat(list(
            printPattern(case.pc_lhs),
            guard,
            Doc.text(" =>"),
            Doc.line,
            rhs,
          )),
        ),
      )),
    )
  }
  
  and printExprFunParameters = (~inCallback, ~uncurried, parameters) =>
    switch parameters {
    | list(/list(), Asttypes.Nolabel, None, {Parsetree.ppat_desc: Ppat_any}/)
      when !uncurried =>
      Doc.text("_")
    | list(/
        list(),
        Asttypes.Nolabel,
        None,
        {Parsetree.ppat_desc: Ppat_var(stringLoc)}
      /) when !uncurried =>
      Doc.text(stringLoc.txt)
    | list(/
        list(),
        Nolabel,
        None,
        {ppat_desc: Ppat_construct({txt: Longident.Lident("()")}, None)}
      /) when !uncurried =>
      Doc.text("()")
    | parameters =>
      let lparen = if uncurried {
        Doc.text("(. ")
      } else {
        Doc.lparen
      }
      let shouldHug = ParsetreeViewer.parametersShouldHug(parameters)
      let printedParamaters = Doc.concat(list(
        if shouldHug || inCallback {
          Doc.nil
        } else {
          Doc.softLine
        },
        Doc.join(
          ~sep=Doc.concat(list(
            Doc.comma,
            if inCallback {
              Doc.space
            } else {
              Doc.line
            },
          )),
          List.map(printExpFunParameter, parameters),
        ),
      ))
      Doc.group(
        Doc.concat(list(
          lparen,
          if shouldHug || inCallback {
            printedParamaters
          } else {
            Doc.indent(printedParamaters)
          },
          if shouldHug || inCallback {
            Doc.nil
          } else {
            Doc.concat(list(Doc.trailingComma, Doc.softLine))
          },
          Doc.rparen,
        )),
      )
    }
  
  and printExpFunParameter = (/attrs, lbl, defaultExpr, pattern/) => {
    let /isUncurried, attrs/ = ParsetreeViewer.processUncurriedAttribute(attrs)
    let uncurried = if isUncurried {
      Doc.concat(list(Doc.dot, Doc.space))
    } else {
      Doc.nil
    }
    let attrs = switch attrs {
    | list() => Doc.nil
    | attrs =>
      Doc.concat(list(
        Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
        Doc.line,
      ))
    }
    
    let defaultExprDoc = switch defaultExpr {
    | Some(expr) => Doc.concat(list(Doc.text("="), printExpression(expr)))
    | None => Doc.nil
    }
    
    let labelWithPattern = switch /lbl, pattern/ {
    | /Asttypes.Nolabel, pattern/ => printPattern(pattern)
    | /Asttypes.Labelled(lbl) | Optional(lbl), {ppat_desc: Ppat_var(stringLoc)}/
      when lbl == stringLoc.txt =>
      Doc.concat(list(Doc.text("~"), Doc.text(lbl)))
    | /Asttypes.Labelled(lbl) | Optional(lbl), pattern/ =>
      Doc.concat(list(
        Doc.text("~"),
        Doc.text(lbl),
        Doc.text(" as "),
        printPattern(pattern),
      ))
    }
    
    let optionalLabelSuffix = switch /lbl, defaultExpr/ {
    | /Asttypes.Optional(_), None/ => Doc.text("=?")
    | _ => Doc.nil
    }
    
    Doc.group(
      Doc.concat(list(
        uncurried,
        attrs,
        labelWithPattern,
        defaultExprDoc,
        optionalLabelSuffix,
      )),
    )
  }
  
  and printExpressionBlock = (~braces, expr) => {
    let rec collectRows = (acc, expr) =>
      switch expr.Parsetree.pexp_desc {
      | Parsetree.Pexp_letmodule({txt: modName, loc: modLoc}, modExpr, expr) =>
        let letModuleDoc = Doc.concat(list(
          Doc.text("module "),
          Doc.text(modName),
          Doc.text(" = "),
          printModExpr(modExpr),
        ))
        let loc = {...modLoc, loc_end: modExpr.pmod_loc.loc_end}
        collectRows(list(/loc, letModuleDoc/, ...acc), expr)
      | Pexp_letexception(extensionConstructor, expr) =>
        let letExceptionDoc = printExceptionDef(extensionConstructor)
        let loc = extensionConstructor.pext_loc
        collectRows(list(/loc, letExceptionDoc/, ...acc), expr)
      | Pexp_open(overrideFlag, longidentLoc, expr) =>
        let openDoc = Doc.concat(list(
          Doc.text("open"),
          printOverrideFlag(overrideFlag),
          Doc.space,
          printLongident(longidentLoc.txt),
        ))
        let loc = longidentLoc.loc
        collectRows(list(/loc, openDoc/, ...acc), expr)
      | Pexp_sequence(expr1, expr2) =>
        let exprDoc = {
          let doc = printExpression(expr1)
          if Parens.blockExpr(expr1) {
            addParens(doc)
          } else {
            doc
          }
        }
        
        let loc = expr1.pexp_loc
        collectRows(list(/loc, exprDoc/, ...acc), expr2)
      | Pexp_let(recFlag, valueBindings, expr) =>
        let recFlag = switch recFlag {
        | Asttypes.Nonrecursive => Doc.nil
        | Asttypes.Recursive => Doc.text("rec ")
        }
        
        let letDoc = printValueBindings(~recFlag, valueBindings)
        let loc = switch /valueBindings, List.rev(valueBindings)/ {
        | /list({pvb_loc: firstLoc}, ..._), list({pvb_loc: lastLoc}, ..._)/ =>
          {...firstLoc, loc_end: lastLoc.loc_end}
        | _ => Location.none
        }
        
        collectRows(list(/loc, letDoc/, ...acc), expr)
      | _ =>
        let exprDoc = {
          let doc = printExpression(expr)
          if Parens.blockExpr(expr) {
            addParens(doc)
          } else {
            doc
          }
        }
        
        List.rev(list(/expr.pexp_loc, exprDoc/, ...acc))
      }
    
    let block =
      collectRows(list(), expr) |> interleaveWhitespace(~forceBreak=true)
    Doc.breakableGroup(
      ~forceBreak=true,
      if braces {
        Doc.concat(list(
          Doc.lbrace,
          Doc.indent(Doc.concat(list(Doc.line, block))),
          Doc.line,
          Doc.rbrace,
        ))
      } else {
        block
      },
    )
  }
  
  and printOverrideFlag = overrideFlag =>
    switch overrideFlag {
    | Asttypes.Override => Doc.text("!")
    | Fresh => Doc.nil
    }
  
  and printDirectionFlag = flag =>
    switch flag {
    | Asttypes.Downto => Doc.text(" downto ")
    | Asttypes.Upto => Doc.text(" to ")
    }
  
  and printRecordRow = (/lbl, expr/) =>
    Doc.concat(list(
      printLongident(lbl.txt),
      Doc.text(": "),
      printExpression(expr),
    ))
  
  and printBsObjectRow = (/lbl, expr/) =>
    Doc.concat(list(
      Doc.text("\""),
      printLongident(lbl.txt),
      Doc.text("\""),
      Doc.text(": "),
      printExpression(expr),
    ))
  
  and printAttributes = (~loc=?, attrs: Parsetree.attributes) =>
    switch attrs {
    | list() => Doc.nil
    | attrs =>
      let lineBreak = switch loc {
      | None => Doc.line
      | Some(loc) =>
        switch List.rev(attrs) {
        | list(/{loc: firstLoc}, _/, ..._)
          when loc.loc_start.pos_lnum > firstLoc.loc_end.pos_lnum =>
          Doc.literalLine
        | _ => Doc.line
        }
      }
      
      Doc.concat(list(
        Doc.group(Doc.join(~sep=Doc.line, List.map(printAttribute, attrs))),
        lineBreak,
      ))
    }
  
  and printAttribute = (/id, payload/: Parsetree.attribute) => {
    let attrName = Doc.text("@" ++ id.txt)
    switch payload {
    | PStr(list({pstr_desc: Pstr_eval(expr, attrs)})) =>
      let exprDoc = printExpression(expr)
      let needsParens = switch attrs {
      | list() => false
      | _ => true
      }
      Doc.group(
        Doc.concat(list(
          attrName,
          addParens(
            Doc.concat(list(
              printAttributes(attrs),
              if needsParens {
                addParens(exprDoc)
              } else {
                exprDoc
              },
            )),
          ),
        )),
      )
    | _ => attrName
    }
  }
  
  and printModExpr = modExpr =>
    switch modExpr.pmod_desc {
    | Pmod_ident(longidentLoc) => printLongident(longidentLoc.txt)
    | Pmod_structure(structure) =>
      Doc.breakableGroup(
        ~forceBreak=true,
        Doc.concat(list(
          Doc.lbrace,
          Doc.indent(Doc.concat(list(Doc.softLine, printStructure(structure)))),
          Doc.softLine,
          Doc.rbrace,
        )),
      )
    | Pmod_unpack(expr) =>
      let shouldHug = switch expr.pexp_desc {
      | Pexp_let(_) => true
      | Pexp_constraint(
          {pexp_desc: Pexp_let(_)},
          {ptyp_desc: Ptyp_package(packageType)},
        ) =>
        true
      | _ => false
      }
      
      let /expr, moduleConstraint/ = switch expr.pexp_desc {
      | Pexp_constraint(expr, {ptyp_desc: Ptyp_package(packageType)}) =>
        let typeDoc = Doc.group(
          Doc.concat(list(
            Doc.text(":"),
            Doc.indent(
              Doc.concat(list(
                Doc.line,
                printPackageType(
                  ~printModuleKeywordAndParens=false,
                  packageType,
                ),
              )),
            ),
          )),
        )
        /expr, typeDoc/
      | _ => /expr, Doc.nil/
      }
      
      let unpackDoc = Doc.group(
        Doc.concat(list(printExpression(expr), moduleConstraint)),
      )
      Doc.group(
        Doc.concat(list(
          Doc.text("unpack("),
          if shouldHug {
            unpackDoc
          } else {
            Doc.concat(list(
              Doc.indent(Doc.concat(list(Doc.softLine, unpackDoc))),
              Doc.softLine,
            ))
          },
          Doc.rparen,
        )),
      )
    | Pmod_extension(extension) => printExtension(extension)
    | Pmod_apply(_) =>
      let /args, callExpr/ = ParsetreeViewer.modExprApply(modExpr)
      let isUnitSugar = switch args {
      | list({pmod_desc: Pmod_structure(list())}) => true
      | _ => false
      }
      
      let shouldHug = switch args {
      | list({pmod_desc: Pmod_structure(_)}) => true
      | _ => false
      }
      
      Doc.group(
        Doc.concat(list(
          printModExpr(callExpr),
          if isUnitSugar {
            printModApplyArg(List.hd(args))
          } else {
            Doc.concat(list(
              Doc.lparen,
              if shouldHug {
                printModApplyArg(List.hd(args))
              } else {
                Doc.indent(
                  Doc.concat(list(
                    Doc.softLine,
                    Doc.join(
                      ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                      List.map(printModApplyArg, args),
                    ),
                  )),
                )
              },
              if !shouldHug {
                Doc.concat(list(Doc.trailingComma, Doc.softLine))
              } else {
                Doc.nil
              },
              Doc.rparen,
            ))
          },
        )),
      )
    | Pmod_constraint(modExpr, modType) =>
      Doc.concat(list(
        printModExpr(modExpr),
        Doc.text(": "),
        printModType(modType),
      ))
    | Pmod_functor(_) => printModFunctor(modExpr)
    }
  
  and printModFunctor = modExpr => {
    let /parameters, returnModExpr/ = ParsetreeViewer.modExprFunctor(modExpr)
    
    let /returnConstraint, returnModExpr/ = switch returnModExpr.pmod_desc {
    | Pmod_constraint(modExpr, modType) =>
      let constraintDoc = {
        let doc = printModType(modType)
        if Parens.modExprFunctorConstraint(modType) {
          addParens(doc)
        } else {
          doc
        }
      }
      
      let modConstraint = Doc.concat(list(Doc.text(": "), constraintDoc))
      /modConstraint, printModExpr(modExpr)/
    | _ => /Doc.nil, printModExpr(returnModExpr)/
    }
    
    let parametersDoc = switch parameters {
    | list(/attrs, {txt: "*"}, None/) =>
      let attrs = switch attrs {
      | list() => Doc.nil
      | attrs =>
        Doc.concat(list(
          Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
          Doc.line,
        ))
      }
      Doc.group(Doc.concat(list(attrs, Doc.text("()"))))
    | list(/list(), {txt: lbl}, None/) => Doc.text(lbl)
    | parameters =>
      Doc.group(
        Doc.concat(list(
          Doc.lparen,
          Doc.indent(
            Doc.concat(list(
              Doc.softLine,
              Doc.join(
                ~sep=Doc.concat(list(Doc.comma, Doc.line)),
                List.map(printModFunctorParam, parameters),
              ),
            )),
          ),
          Doc.trailingComma,
          Doc.softLine,
          Doc.rparen,
        )),
      )
    }
    
    Doc.group(
      Doc.concat(list(
        parametersDoc,
        returnConstraint,
        Doc.text(" => "),
        returnModExpr,
      )),
    )
  }
  
  and printModFunctorParam = (/attrs, lbl, optModType/) => {
    let attrs = switch attrs {
    | list() => Doc.nil
    | attrs =>
      Doc.concat(list(
        Doc.join(~sep=Doc.line, List.map(printAttribute, attrs)),
        Doc.line,
      ))
    }
    Doc.group(
      Doc.concat(list(
        attrs,
        Doc.text(lbl.txt),
        switch optModType {
        | None => Doc.nil
        | Some(modType) =>
          Doc.concat(list(Doc.text(": "), printModType(modType)))
        },
      )),
    )
  }
  
  and printModApplyArg = modExpr =>
    switch modExpr.pmod_desc {
    | Pmod_structure(list()) => Doc.text("()")
    | _ => printModExpr(modExpr)
    }
  
  and printExceptionDef = (constr: Parsetree.extension_constructor) => {
    let kind = switch constr.pext_kind {
    | Pext_rebind({txt: longident}) =>
      Doc.indent(
        Doc.concat(list(Doc.text(" ="), Doc.line, printLongident(longident))),
      )
    | Pext_decl(Pcstr_tuple(list()), None) => Doc.nil
    | Pext_decl(args, gadt) =>
      let gadtDoc = switch gadt {
      | Some(typ) => Doc.concat(list(Doc.text(": "), printTypExpr(typ)))
      | None => Doc.nil
      }
      
      Doc.concat(list(printConstructorArguments(args), gadtDoc))
    }
    
    Doc.group(
      Doc.concat(list(
        printAttributes(constr.pext_attributes),
        Doc.text("exception "),
        Doc.text(constr.pext_name.txt),
        kind,
      )),
    )
  }
  
  and printExtensionConstructor = (
    i,
    constr: Parsetree.extension_constructor,
  ) => {
    let attrs = printAttributes(constr.pext_attributes)
    let bar = if i > 0 {
      Doc.text("| ")
    } else {
      Doc.ifBreaks(Doc.text("| "), Doc.nil)
    }
    
    let kind = switch constr.pext_kind {
    | Pext_rebind({txt: longident}) =>
      Doc.indent(
        Doc.concat(list(Doc.text(" ="), Doc.line, printLongident(longident))),
      )
    | Pext_decl(Pcstr_tuple(list()), None) => Doc.nil
    | Pext_decl(args, gadt) =>
      let gadtDoc = switch gadt {
      | Some(typ) => Doc.concat(list(Doc.text(": "), printTypExpr(typ)))
      | None => Doc.nil
      }
      
      Doc.concat(list(printConstructorArguments(args), gadtDoc))
    }
    
    Doc.concat(list(
      bar,
      Doc.group(Doc.concat(list(attrs, Doc.text(constr.pext_name.txt), kind))),
    ))
  }
  
  let printImplementation = (s: Parsetree.structure, comments, src) => {
    let t = CommentAst.initStructure(s, comments)
    
    let stringDoc = Doc.toString(~width=80, printStructure(s))
    print_endline(stringDoc)
    print_newline()
  }
  
  let printInterface = (s: Parsetree.signature) => {
    let stringDoc = Doc.toString(~width=80, printSignature(s))
    print_endline(stringDoc)
    print_newline()
  }
}



