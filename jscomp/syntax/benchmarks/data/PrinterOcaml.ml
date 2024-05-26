module Printer = struct
  type printer = {
    src: bytes;
    comments: CommentAst.t;
  }


  (* TODO: should this go inside a ast utility module? *)
  let rec collect_patterns_from_list_construct acc pattern =
    let open Parsetree in
    match pattern.ppat_desc with
    | Ppat_construct(
        {txt = Longident.Lident "::"},
        Some {ppat_desc=Ppat_tuple (pat::rest::[])}
      ) ->
      collect_patterns_from_list_construct (pat::acc) rest
    | _ -> List.rev acc, pattern

  let add_parens doc =
    Doc.group (
      Doc.concat [
        Doc.lparen;
        Doc.indent (
          Doc.concat [
            Doc.soft_line;
            doc
          ]
        );
        Doc.soft_line;
        Doc.rparen;
      ]
    )

  let add_braces doc =
    Doc.group (
      Doc.concat [
        Doc.lbrace;
        doc;
        Doc.rbrace;
      ]
    )

  (* This could be done in one pass by collecting locations as we go? *)
  let interleave_whitespace ?(force_break=false) (rows: (Location.t * Doc.t) list) =
    let rec loop prev_loc acc rows =
      match rows with
      | [] -> Doc.concat (List.rev acc)
      | (loc, doc)::rest ->
        if loc.Location.loc_start.pos_lnum - prev_loc.Location.loc_end.pos_lnum > 1 then
          loop loc (doc::Doc.line::Doc.line::acc) rest
        else
          loop loc (doc::Doc.line::acc) rest
    in
    match rows with
    | [] -> Doc.nil
    | (first_loc, first_doc)::rest ->
      (* TODO: perf, reversing the list twice! *)
      let force_break = force_break || (match List.rev rest with
      | (last_loc, _)::_ ->
        first_loc.loc_start.pos_lnum != last_loc.loc_end.pos_lnum
      | _ -> false)
      in
      Doc.breakable_group ~force_break (
        loop first_loc [first_doc] rest
      )

  let print_longident l = match l with
    | Longident.Lident lident -> Doc.text lident
    | Longident.Ldot (lident, txt) as l ->
      let txts = Longident.flatten l in
      Doc.join ~sep:Doc.dot (List.map Doc.text txts)
    | _ -> failwith "unsupported ident"

  (* TODO: better allocation strategy for the buffer *)
  let escape_string_contents s =
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

  let print_constant c = match c with
    | Parsetree.Pconst_integer (s, _) -> Doc.text s
    | Pconst_string (s, _) -> Doc.text ("\"" ^ (escape_string_contents s) ^ "\"")
    | Pconst_float (s, _) -> Doc.text s
    | Pconst_char c -> Doc.text ("'" ^ (Char.escaped c) ^ "'")

  let rec print_structure (s : Parsetree.structure) =
    interleave_whitespace  (
      List.map (fun si -> (si.Parsetree.pstr_loc, print_structure_item si)) s
    )

  and print_structure_item (si: Parsetree.structure_item) =
    match si.pstr_desc with
    | Pstr_value(rec_flag, value_bindings) ->
			let rec_flag = match rec_flag with
			| Asttypes.Nonrecursive -> Doc.nil
			| Asttypes.Recursive -> Doc.text "rec "
			in
      print_value_bindings ~rec_flag value_bindings
    | Pstr_type(rec_flag, type_declarations) ->
      let rec_flag = match rec_flag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      print_type_declarations ~rec_flag type_declarations
    | Pstr_primitive value_description ->
      print_value_description value_description
    | Pstr_eval (expr, attrs) ->
      let needs_parens = match expr with
      | {pexp_attributes=[({txt="res.ternary"},_)]; pexp_desc = Pexp_ifthenelse _} -> false
      | _ when ParsetreeViewer.has_attributes expr.pexp_attributes -> true
      | _ -> false
      in
      let expr_doc =
        let doc = print_expression expr in
        if needs_parens then add_parens doc else doc
      in
      Doc.concat [
        print_attributes attrs;
        expr_doc;
      ]
    | Pstr_attribute attr -> Doc.concat [Doc.text "@"; print_attribute attr]
    | Pstr_extension (extension, attrs) -> Doc.concat [
        print_attributes attrs;
        Doc.concat [Doc.text "%";print_extension extension];
      ]
    | Pstr_include include_declaration ->
      print_include_declaration include_declaration
    | Pstr_open open_description ->
      print_open_description open_description
    | Pstr_modtype mod_type_decl ->
      print_module_type_declaration mod_type_decl
    | Pstr_module module_binding ->
      print_module_binding ~is_rec:false 0 module_binding
    | Pstr_recmodule module_bindings ->
      Doc.join ~sep:Doc.line (List.mapi (fun i mb ->
        print_module_binding ~is_rec:true i mb
      ) module_bindings)
    | Pstr_exception extension_constructor ->
      print_exception_def extension_constructor;
    | Pstr_typext type_extension ->
      print_type_extension type_extension
    | Pstr_class _ | Pstr_class_type _ -> Doc.nil

  and print_type_extension (te : Parsetree.type_extension) =
    let prefix = Doc.text "type " in
    let name = print_longident te.ptyext_path.txt in
    let type_params = match te.ptyext_params with
    | [] -> Doc.nil
    | type_params -> Doc.group (
        Doc.concat [
          Doc.less_than;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_type_param type_params
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.greater_than;
        ]
      )
    in
    let extension_constructors =
      let ecs = te.ptyext_constructors in
      let force_break =
        match (ecs, List.rev ecs) with
        | (first::_, last::_) ->
          first.pext_loc.loc_start.pos_lnum > te.ptyext_path.loc.loc_end.pos_lnum ||
          first.pext_loc.loc_start.pos_lnum < last.pext_loc.loc_end.pos_lnum
        | _ -> false
        in
      let private_flag = match te.ptyext_private with
      | Asttypes.Private -> Doc.concat [
          Doc.text "private";
          Doc.line;
        ]
      | Public -> Doc.nil
      in
      Doc.breakable_group ~force_break (
        Doc.indent (
          Doc.concat [
            Doc.line;
            private_flag;
            Doc.join ~sep:Doc.line (
              List.mapi print_extension_constructor ecs
            )
          ]
        )
      )
    in
    Doc.group (
      Doc.concat [
        print_attributes ~loc: te.ptyext_path.loc te.ptyext_attributes;
        prefix;
        name;
        type_params;
        Doc.text " +=";
        extension_constructors;
      ]
    )

  and print_module_binding ~is_rec i module_binding =
    let prefix = if i = 0 then
      Doc.concat [
        Doc.text "module ";
        if is_rec then Doc.text "rec " else Doc.nil;
      ]
    else
      Doc.text "and "
    in
    let (mod_expr_doc, mod_constraint_doc) =
      match module_binding.pmb_expr with
      | {pmod_desc = Pmod_constraint (mod_expr, mod_type)} ->
        (
          print_mod_expr mod_expr,
          Doc.concat [
            Doc.text ": ";
            print_mod_type mod_type
          ]
        )
      | mod_expr ->
        (print_mod_expr mod_expr, Doc.nil)
    in
    Doc.concat [
      print_attributes ~loc:module_binding.pmb_name.loc module_binding.pmb_attributes;
      prefix;
      Doc.text module_binding.pmb_name.Location.txt;
      mod_constraint_doc;
      Doc.text " = ";
      mod_expr_doc;
    ]

  and print_module_type_declaration (mod_type_decl : Parsetree.module_type_declaration) =
    Doc.concat [
      print_attributes mod_type_decl.pmtd_attributes;
      Doc.text "module type ";
      Doc.text mod_type_decl.pmtd_name.txt;
      (match mod_type_decl.pmtd_type with
      | None -> Doc.nil
      | Some mod_type -> Doc.concat [
          Doc.text " = ";
          print_mod_type mod_type;
        ]);
    ]

  and print_mod_type mod_type =
    let mod_type_doc = match mod_type.pmty_desc with
    | Parsetree.Pmty_ident {txt = longident; loc} ->
      Doc.concat [
        print_attributes ~loc mod_type.pmty_attributes;
        print_longident longident
      ]
    | Pmty_signature signature ->
      let signature_doc = Doc.breakable_group ~force_break:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_signature signature;
            ]
          );
          Doc.line;
          Doc.rbrace;
        ]
      ) in
      Doc.concat [
        print_attributes mod_type.pmty_attributes;
        signature_doc
      ]
    | Pmty_functor _ ->
      let (parameters, return_type) = ParsetreeViewer.functor_type mod_type in
      let parameters_doc = match parameters with
      | [] -> Doc.nil
      | [attrs, {Location.txt = "_"}, Some mod_type] ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map print_attribute attrs);
          Doc.line;
        ] in
        Doc.concat [
          attrs;
          print_mod_type mod_type
        ]
      | params ->
        Doc.group (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map (fun (attrs, lbl, mod_type) ->
                    let attrs = match attrs with
                    | [] -> Doc.nil
                    | attrs -> Doc.concat [
                      Doc.join ~sep:Doc.line (List.map print_attribute attrs);
                      Doc.line;
                    ] in
                    Doc.concat [
                      attrs;
                      if lbl.Location.txt = "_" then Doc.nil else Doc.text lbl.txt;
                      (match mod_type with
                      | None -> Doc.nil
                      | Some mod_type -> Doc.concat [
                        if lbl.txt = "_" then Doc.nil else Doc.text ": ";
                        print_mod_type mod_type;
                      ]);
                    ]
                  ) params
                );
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
            Doc.rparen;
          ]
        )
      in
      let return_doc =
        let doc = print_mod_type return_type in
        if Parens.mod_type_functor_return return_type then add_parens doc else doc
      in
      Doc.group (
        Doc.concat [
          parameters_doc;
          Doc.group (
            Doc.concat [
            Doc.text " =>";
            Doc.line;
            return_doc;
            ]
          )
        ]
      )
    | Pmty_typeof mod_expr -> Doc.concat [
        Doc.text "module type of ";
        print_mod_expr mod_expr;
      ]
    | Pmty_extension extension -> print_extension extension
    | Pmty_alias {txt = longident} -> Doc.concat [
        Doc.text "module ";
        print_longident longident;
      ]
    | Pmty_with (mod_type, with_constraints) ->
      let operand =
        let doc = print_mod_type mod_type in
        if Parens.mod_type_with_operand mod_type then add_parens doc else doc
      in
      Doc.group (
        Doc.concat [
          operand;
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_with_constraints with_constraints;
            ]
          )
        ]
      )
    in
    let attrs_already_printed = match mod_type.pmty_desc with
    | Pmty_functor _ | Pmty_signature _ | Pmty_ident _ -> true
    | _ -> false in
    Doc.concat [
      if attrs_already_printed then Doc.nil else print_attributes mod_type.pmty_attributes;
      mod_type_doc;
    ]

  and print_with_constraints with_constraints =
    let rows =List.mapi (fun i with_constraint  ->
      Doc.group (
        Doc.concat [
          if i == 0 then Doc.text "with " else Doc.text "and ";
          print_with_constraint with_constraint;
        ]
      )
    ) with_constraints
    in
    Doc.join ~sep:Doc.line rows

  and print_with_constraint (with_constraint : Parsetree.with_constraint) =
    match with_constraint with
    (* with type X.t = ... *)
    | Pwith_type ({txt = longident}, type_declaration) ->
      Doc.group (print_type_declaration
        ~name:(print_longident longident)
        ~equal_sign:"="
        ~rec_flag:Doc.nil
        0
        type_declaration)
    (* with module X.Y = Z *)
    | Pwith_module ({txt = longident1}, {txt = longident2}) ->
        Doc.concat [
          Doc.text "module ";
          print_longident longident1;
          Doc.text " =";
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_longident longident2;
            ]
          )
        ]
    (* with type X.t := ..., same format as [Pwith_type] *)
    | Pwith_typesubst ({txt = longident}, type_declaration) ->
      Doc.group(print_type_declaration
        ~name:(print_longident longident)
        ~equal_sign:":="
        ~rec_flag:Doc.nil
        0
        type_declaration)
    | Pwith_modsubst ({txt = longident1}, {txt = longident2}) ->
      Doc.concat [
        Doc.text "module ";
        print_longident longident1;
        Doc.text " :=";
        Doc.indent (
          Doc.concat [
            Doc.line;
            print_longident longident2;
          ]
        )
      ]

  and print_signature signature =
    interleave_whitespace  (
      List.map (fun si -> (si.Parsetree.psig_loc, print_signature_item si)) signature
    )

  and print_signature_item (si : Parsetree.signature_item) =
    match si.psig_desc with
    | Parsetree.Psig_value value_description ->
      print_value_description value_description
    | Psig_type (rec_flag, type_declarations) ->
      let rec_flag = match rec_flag with
      | Asttypes.Nonrecursive -> Doc.nil
      | Asttypes.Recursive -> Doc.text "rec "
      in
      print_type_declarations ~rec_flag type_declarations
    | Psig_typext type_extension ->
      print_type_extension type_extension
    | Psig_exception extension_constructor ->
      print_exception_def extension_constructor
    | Psig_module module_declaration ->
      print_module_declaration module_declaration
    | Psig_recmodule module_declarations ->
      print_rec_module_declarations module_declarations
    | Psig_modtype mod_type_decl ->
      print_module_type_declaration mod_type_decl
    | Psig_open open_description ->
      print_open_description open_description
    | Psig_include include_description ->
      print_include_description include_description
    | Psig_attribute attr -> Doc.concat [Doc.text "@"; print_attribute attr]
    | Psig_extension (extension, attrs) -> Doc.concat [
        print_attributes attrs;
        Doc.concat [Doc.text "%";print_extension extension];
      ]
    | Psig_class _ | Psig_class_type _ -> Doc.nil

  and print_rec_module_declarations module_declarations =
    Doc.group (
      Doc.join ~sep:Doc.line (
        List.mapi (fun i (md: Parsetree.module_declaration) ->
          let body = match md.pmd_type.pmty_desc with
          | Parsetree.Pmty_alias {txt = longident } ->
            Doc.concat [Doc.text " = "; print_longident longident]
          | _ ->
            let needs_parens = match md.pmd_type.pmty_desc with
            | Pmty_with _ -> true
            | _ -> false
            in
            let mod_type_doc =
              let doc = print_mod_type md.pmd_type in
              if needs_parens then add_parens doc else doc
            in
            Doc.concat [Doc.text ": "; mod_type_doc]
          in
          let prefix = if i < 1 then "module rec " else "and " in
          Doc.concat [
            print_attributes ~loc:md.pmd_name.loc md.pmd_attributes;
            Doc.text prefix;
            Doc.text md.pmd_name.txt;
            body
          ]
        ) module_declarations
      )
    )

  and print_module_declaration (md: Parsetree.module_declaration) =
    let body = match md.pmd_type.pmty_desc with
    | Parsetree.Pmty_alias {txt = longident } ->
      Doc.concat [Doc.text " = "; print_longident longident]
    | _ -> Doc.concat [Doc.text ": "; print_mod_type md.pmd_type]
    in
    Doc.concat [
      print_attributes ~loc:md.pmd_name.loc md.pmd_attributes;
      Doc.text "module ";
      Doc.text md.pmd_name.txt;
      body
    ]

  and print_open_description (open_description : Parsetree.open_description) =
    Doc.concat [
      print_attributes open_description.popen_attributes;
      Doc.text "open";
      (match open_description.popen_override with
      | Asttypes.Fresh -> Doc.space
      | Asttypes.Override -> Doc.text "! ");
      print_longident open_description.popen_lid.txt
    ]

  and print_include_description (include_description: Parsetree.include_description) =
    Doc.concat [
      print_attributes include_description.pincl_attributes;
      Doc.text "include ";
      print_mod_type include_description.pincl_mod;
    ]

  and print_include_declaration (include_declaration : Parsetree.include_declaration) =
    Doc.concat [
      print_attributes include_declaration.pincl_attributes;
      Doc.text "include ";
      print_mod_expr include_declaration.pincl_mod;
    ]


  and print_value_bindings ~rec_flag (vbs: Parsetree.value_binding list) =
    let rows = List.mapi (fun i vb ->
      let doc = print_value_binding ~rec_flag i vb in
      (vb.Parsetree.pvb_loc, doc)
    ) vbs
    in
    interleave_whitespace rows

  (*
   * type value_description = {
   *   pval_name : string Asttypes.loc;
   *   pval_type : Parsetree.core_type;
   *   pval_prim : string list;
   *   pval_attributes : Parsetree.attributes;
   *   pval_loc : Location.t;
   * }
   *)
  and print_value_description value_description =
    let is_external =
      match value_description.pval_prim with | [] -> false | _ -> true
    in
    Doc.group (
      Doc.concat [
        Doc.text (if is_external then "external " else "let ");
        Doc.text value_description.pval_name.txt;
        Doc.text ": ";
        print_typ_expr value_description.pval_type;
        if is_external then
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
                    value_description.pval_prim
                  );
                ]
              )
            ]
          )
        else Doc.nil
      ]
    )

  and print_type_declarations ~rec_flag type_declarations =
    let rows = List.mapi (fun i td ->
      let doc = print_type_declaration
        ~name:(Doc.text td.Parsetree.ptype_name.txt)
        ~equal_sign:"="
        ~rec_flag
        i td
      in
      (td.Parsetree.ptype_loc, doc)
    ) type_declarations in
    interleave_whitespace rows

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
  and print_type_declaration ~name ~equal_sign ~rec_flag i (td: Parsetree.type_declaration) =
    let attrs = print_attributes ~loc:td.ptype_loc td.ptype_attributes in
    let prefix = if i > 0 then
      Doc.text "and "
    else
      Doc.concat [Doc.text "type "; rec_flag]
    in
    let type_name = name in
    let type_params = match td.ptype_params with
    | [] -> Doc.nil
    | type_params -> Doc.group (
        Doc.concat [
          Doc.less_than;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_type_param type_params
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.greater_than;
        ]
      )
    in
    let manifest_and_kind = match td.ptype_kind with
    | Ptype_abstract ->
      begin match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) ->
        Doc.concat [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_private_flag td.ptype_private;
          print_typ_expr typ;
        ]
      end
    | Ptype_open -> Doc.concat [
        Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
        print_private_flag td.ptype_private;
        Doc.text "..";
      ]
    | Ptype_record(lds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_typ_expr typ;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
        print_private_flag td.ptype_private;
        print_record_declaration lds;
      ]
    | Ptype_variant(cds) ->
      let manifest = match td.ptype_manifest with
      | None -> Doc.nil
      | Some(typ) -> Doc.concat [
          Doc.concat [Doc.space; Doc.text equal_sign; Doc.space];
          print_typ_expr typ;
        ]
      in
      Doc.concat [
        manifest;
        Doc.concat [Doc.space; Doc.text equal_sign];
        print_constructor_declarations ~private_flag:td.ptype_private cds;
      ]
    in
    let constraints = print_type_definition_constraints td.ptype_cstrs in
    Doc.group (
      Doc.concat [
        attrs;
        prefix;
        type_name;
        type_params;
        manifest_and_kind;
        constraints;
      ]
    )

  and print_type_definition_constraints cstrs =
    match cstrs with
    | [] -> Doc.nil
    | cstrs -> Doc.indent (
        Doc.group (
          Doc.concat [
            Doc.line;
            Doc.group(
              Doc.join ~sep:Doc.line (
                List.map print_type_definition_constraint cstrs
              )
            )
          ]
        )
      )

  and print_type_definition_constraint ((typ1, typ2, _loc ): Parsetree.core_type * Parsetree.core_type * Location.t) =
    Doc.concat [
      Doc.text "constraint ";
      print_typ_expr typ1;
      Doc.text " = ";
      print_typ_expr typ2;
    ]

  and print_private_flag (flag : Asttypes.private_flag) = match flag with
    | Private -> Doc.text "private "
    | Public -> Doc.nil

  and print_type_param (param : (Parsetree.core_type * Asttypes.variance)) =
    let (typ, variance) = param in
    let printed_variance = match variance with
    | Covariant -> Doc.text "+"
    | Contravariant -> Doc.text "-"
    | Invariant -> Doc.nil
    in
    Doc.concat [
      printed_variance;
      print_typ_expr typ
    ]

  and print_record_declaration (lds: Parsetree.label_declaration list) =
    let force_break = match (lds, List.rev lds) with
    | (first::_, last::_) ->
       first.pld_loc.loc_start.pos_lnum < last.pld_loc.loc_end.pos_lnum
    | _ -> false
    in
    Doc.breakable_group ~force_break (
      Doc.concat [
        Doc.lbrace;
        Doc.indent (
          Doc.concat [
            Doc.soft_line;
            Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
              (List.map print_label_declaration lds)
          ]
        );
        Doc.trailing_comma;
        Doc.soft_line;
        Doc.rbrace;
      ]
    )

  and print_constructor_declarations ~private_flag (cds: Parsetree.constructor_declaration list) =
    let force_break = match (cds, List.rev cds) with
    | (first::_, last::_) ->
       first.pcd_loc.loc_start.pos_lnum < last.pcd_loc.loc_end.pos_lnum
    | _ -> false
    in
    let private_flag = match private_flag with
    | Asttypes.Private -> Doc.concat [
        Doc.text "private";
        Doc.line;
      ]
    | Public -> Doc.nil
    in
    Doc.breakable_group ~force_break (
      Doc.indent (
        Doc.concat [
          Doc.line;
          private_flag;
          Doc.join ~sep:Doc.line (
            List.mapi print_constructor_declaration cds
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
  and print_constructor_declaration i (cd : Parsetree.constructor_declaration) =
    let attrs = print_attributes cd.pcd_attributes in
    let bar = if i > 0 then Doc.text "| "
      else Doc.if_breaks (Doc.text "| ") Doc.nil
    in
    let constr_name = Doc.text cd.pcd_name.txt in
    let constr_args = print_constructor_arguments cd.pcd_args in
    let gadt = match cd.pcd_res with
    | None -> Doc.nil
    | Some(typ) -> Doc.indent (
        Doc.concat [
          Doc.text ": ";
          print_typ_expr typ;
        ]
      )
    in
    Doc.concat [
      bar;
      Doc.group (
        Doc.concat [
          attrs; (* TODO: fix parsing of attributes, so when can print them above the bar? *)
          constr_name;
          constr_args;
          gadt;
        ]
      )
    ]

  and print_constructor_arguments (cd_args : Parsetree.constructor_arguments) =
    match cd_args with
    | Pcstr_tuple [] -> Doc.nil
    | Pcstr_tuple types -> Doc.group (
        Doc.indent (
          Doc.concat [
            Doc.lparen;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                  List.map print_typ_expr types
                )
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
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
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line])
                (List.map print_label_declaration lds)
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbrace;
          Doc.rparen;
        ]
      )


  and print_label_declaration (ld : Parsetree.label_declaration) =
    let attrs = print_attributes ~loc:ld.pld_name.loc ld.pld_attributes in
    let mutable_flag = match ld.pld_mutable with
    | Mutable -> Doc.text "mutable "
    | Immutable -> Doc.nil
    in
    let name = Doc.text ld.pld_name.txt in
    Doc.group (
      Doc.concat [
        attrs;
        mutable_flag;
        name;
        Doc.text ": ";
        print_typ_expr ld.pld_type;
      ]
    )

  and print_typ_expr (typ_expr : Parsetree.core_type) =
    let rendered_type = match typ_expr.ptyp_desc with
    | Ptyp_any -> Doc.text "_"
    | Ptyp_var var -> Doc.text ("'" ^ var)
    | Ptyp_extension(extension) ->
      print_extension extension
    | Ptyp_alias(typ, alias) ->
      let typ =
        (* Technically type t = (string, float) => unit as 'x, doesn't require
         * parens around the arrow expression. This is very confusing though.
         * Is the "as" part of "unit" or "(string, float) => unit". By printing
         * parens we guide the user towards its meaning.*)
        let needs_parens = match typ.ptyp_desc with
        | Ptyp_arrow _ -> true
        | _ -> false
        in
        let doc = print_typ_expr typ in
        if needs_parens then
          Doc.concat [Doc.lparen; doc; Doc.rparen]
        else
          doc
      in
      Doc.concat [typ; Doc.text " as "; Doc.text ("'" ^ alias)]
    | Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")}, [typ]) ->
      let bs_object = print_typ_expr typ in
      begin match typ_expr.ptyp_attributes with
      | [] -> bs_object
      | attrs ->
        Doc.concat [
          Doc.group (
            Doc.join ~sep:Doc.line (List.map print_attribute attrs)
          );
          Doc.space;
          print_typ_expr typ;
        ]
      end
    | Ptyp_constr(longident_loc, [{ ptyp_desc = Parsetree.Ptyp_tuple tuple }]) ->
      let constr_name = print_longident longident_loc.txt in
      Doc.group(
        Doc.concat([
          constr_name;
          Doc.less_than;
          print_tuple_type ~inline:true tuple;
          Doc.greater_than;
        ])
      )
    | Ptyp_constr(longident_loc, constr_args) ->
      let constr_name = print_longident longident_loc.txt in
      begin match constr_args with
      | [] -> constr_name
      | [{
          Parsetree.ptyp_desc =
            Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")},
          [{ptyp_desc = Ptyp_object (fields, open_flag)}])
        }] ->
        Doc.concat([
          constr_name;
          Doc.less_than;
          print_bs_object_sugar ~inline:true fields open_flag;
          Doc.greater_than;
        ])
      | args -> Doc.group(
        Doc.concat([
          constr_name;
          Doc.less_than;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_typ_expr constr_args
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.greater_than;
        ])
      )
      end
    | Ptyp_arrow _ ->
      let (attrs_before, args, return_type) = ParsetreeViewer.arrow_type typ_expr in
      let return_type_needs_parens = match return_type.ptyp_desc with
      | Ptyp_alias _ -> true
      | _ -> false
      in
      let return_doc =
        let doc = print_typ_expr return_type in
        if return_type_needs_parens then
          Doc.concat [Doc.lparen; doc; Doc.rparen]
        else doc
      in
      let (is_uncurried, attrs) = ParsetreeViewer.process_uncurried_attribute attrs_before in
      begin match args with
      | [] -> Doc.nil
      | [([], Nolabel, n)] when not is_uncurried ->
          let has_attrs_before = not (attrs = []) in
          let attrs = if has_attrs_before then
            Doc.concat [
              Doc.join ~sep:Doc.line (List.map print_attribute attrs_before);
              Doc.space;
            ]
          else Doc.nil
          in
          Doc.group (
            Doc.concat [
              Doc.group attrs;
              Doc.group (
                if has_attrs_before then
                  Doc.concat [
                    Doc.lparen;
                    Doc.indent (
                      Doc.concat [
                        Doc.soft_line;
                        print_typ_expr n;
                        Doc.text " => ";
                        return_doc;
                      ]
                    );
                    Doc.soft_line;
                    Doc.rparen
                  ]
                else
                Doc.concat [
                  print_typ_expr n;
                  Doc.text " => ";
                  return_doc;
                ]
              )
            ]
          )
      | args ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
            Doc.join ~sep:Doc.line (List.map print_attribute attrs);
            Doc.space;
          ]
        in
        let rendered_args = Doc.concat [
          attrs;
          Doc.text "(";
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              if is_uncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_type_parameter args
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.text ")";
        ] in
        Doc.group (
          Doc.concat [
            rendered_args;
            Doc.text " => ";
            return_doc;
          ]
        )
      end
    | Ptyp_tuple types -> print_tuple_type ~inline:false types
    | Ptyp_object (fields, open_flag) ->
      print_bs_object_sugar ~inline:false fields open_flag
    | Ptyp_poly(string_locs, typ) ->
      Doc.concat [
        Doc.join ~sep:Doc.space (List.map (fun {Location.txt} ->
          Doc.text ("'" ^ txt)) string_locs);
        Doc.dot;
        Doc.space;
        print_typ_expr typ
      ]
    | Ptyp_package package_type ->
      print_package_type ~print_module_keyword_and_parens:true package_type
    | Ptyp_class _ -> failwith "classes are not supported in types"
    | Ptyp_variant _ -> failwith "Polymorphic variants currently not supported"
    in
    let should_print_its_own_attributes = match typ_expr.ptyp_desc with
    | Ptyp_arrow _ (* es6 arrow types print their own attributes *)
    | Ptyp_constr({txt = Longident.Ldot(Longident.Lident "Js", "t")}, _) -> true
    | _ -> false
    in
    begin match typ_expr.ptyp_attributes with
    | _::_ as attrs when not should_print_its_own_attributes ->
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          rendered_type;
        ]
      )
    | _ -> rendered_type
    end

  and print_bs_object_sugar ~inline fields open_flag =
    let flag = match open_flag with
    | Asttypes.Closed -> Doc.nil
    | Open -> Doc.dotdot
    in
    let doc = Doc.concat [
      Doc.lbrace;
      flag;
      Doc.indent (
        Doc.concat [
          Doc.soft_line;
          Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
            List.map print_object_field fields
          )
        ]
      );
      Doc.trailing_comma;
      Doc.soft_line;
      Doc.rbrace;
    ] in
    if inline then doc else Doc.group doc


  and print_tuple_type ~inline (types: Parsetree.core_type list) =
    let tuple = Doc.concat([
      Doc.text "/";
      Doc.indent (
        Doc.concat([
          Doc.soft_line;
          Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
            List.map print_typ_expr types
          )
        ])
      );
      (* Doc.trailingComma; *) (* Trailing comma not supported in tuples right now‚Ä¶¬†*)
      Doc.soft_line;
      Doc.text "/";
    ])
    in
    if inline == false then Doc.group(tuple) else tuple

  and print_object_field (field : Parsetree.object_field) =
    match field with
    | Otag (label_loc, attrs, typ) ->
      Doc.concat [
        Doc.text ("\"" ^ label_loc.txt ^ "\"");
        Doc.text ": ";
        print_typ_expr typ;
      ]
    | _ -> Doc.nil

  (* es6 arrow type arg
   * type t = (~foo: string, ~bar: float=?, unit) => unit
   * i.e. ~foo: string, ~bar: float *)
  and print_type_parameter (attrs, lbl, typ)  =
    let (is_uncurried, attrs) = ParsetreeViewer.process_uncurried_attribute attrs in
    let uncurried = if is_uncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map print_attribute attrs);
      Doc.line;
    ] in
    let label = match lbl with
    | Asttypes.Nolabel -> Doc.nil
    | Labelled lbl -> Doc.text ("~" ^ lbl ^ ": ")
    | Optional lbl -> Doc.text ("~" ^ lbl ^ ": ")
    in
    let optional_indicator = match lbl with
    | Asttypes.Nolabel
    | Labelled _ -> Doc.nil
    | Optional lbl -> Doc.text "=?"
    in
    Doc.group (
      Doc.concat [
        uncurried;
        attrs;
        label;
        print_typ_expr typ;
        optional_indicator;
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
  and print_value_binding ~rec_flag i vb =
    let is_ghost = ParsetreeViewer.is_ghost_unit_binding i vb in
		let header = if is_ghost then Doc.nil else
			if i == 0 then Doc.concat [Doc.text "let "; rec_flag]
			else Doc.text "and "
		in
    let printed_expr =
      let expr_doc = print_expression vb.pvb_expr in
      let needs_parens = match vb.pvb_expr.pexp_desc with
      | Pexp_constraint(
          {pexp_desc = Pexp_pack _},
          {ptyp_desc = Ptyp_package _}
        ) -> false
      | Pexp_constraint _ -> true
      | _ -> false
      in
      if needs_parens then add_parens expr_doc else expr_doc
    in
		if is_ghost then
			printed_expr
		else
      let should_indent =
        ParsetreeViewer.is_binary_expression vb.pvb_expr ||
        (match vb.pvb_expr with
        | {
            pexp_attributes = [({Location.txt="res.ternary"}, _)];
            pexp_desc = Pexp_ifthenelse (if_expr, _, _)
          }  ->
          ParsetreeViewer.is_binary_expression if_expr || ParsetreeViewer.has_attributes if_expr.pexp_attributes
      | { pexp_desc = Pexp_newtype _} -> false
      | e ->
          ParsetreeViewer.has_attributes e.pexp_attributes ||
          ParsetreeViewer.is_array_access e
        )
      in
			Doc.concat [
        print_attributes ~loc:vb.pvb_loc vb.pvb_attributes;
				header;
				print_pattern vb.pvb_pat;
				Doc.text " =";
        if should_indent then
          Doc.indent (
            Doc.concat [
              Doc.line;
              printed_expr;
            ]
          )
        else
          Doc.concat [
            Doc.space;
            printed_expr;
          ]
      ]

  and print_package_type ~print_module_keyword_and_parens (package_type: Parsetree.package_type) =
    let doc = match package_type with
    | (longident_loc, []) -> Doc.group(
        Doc.concat [
          print_longident longident_loc.txt;
        ]
      )
    | (longident_loc, package_constraints) -> Doc.group(
        Doc.concat [
          print_longident longident_loc.txt;
          print_package_constraints package_constraints;
          Doc.soft_line;
        ]
      )
    in
    if print_module_keyword_and_parens then
      Doc.concat[
        Doc.text "module(";
        doc;
        Doc.rparen
      ]
    else
      doc




  and print_package_constraints package_constraints  =
    Doc.concat [
      Doc.text " with";
      Doc.indent (
        Doc.concat [
          Doc.line;
          Doc.join ~sep:Doc.line (
            List.mapi print_packageconstraint package_constraints
          )
        ]
      )
    ]

  and print_packageconstraint i (longident_loc, typ) =
    let prefix = if i == 0 then Doc.text "type " else Doc.text "and type " in
    Doc.concat [
      prefix;
      print_longident longident_loc.Location.txt;
      Doc.text " = ";
      print_typ_expr typ
    ]

  and print_extension (string_loc, payload) =
    let ext_name = Doc.text ("%" ^ string_loc.Location.txt) in
    match payload with
    | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
      let expr_doc = print_expression expr in
      let needs_parens = match attrs with | [] -> false | _ -> true in
      Doc.group (
        Doc.concat [
          ext_name;
          add_parens (
            Doc.concat [
              print_attributes attrs;
              if needs_parens then add_parens expr_doc else expr_doc;
            ]
          )
        ]
      )
    | _ -> ext_name

  and print_pattern (p : Parsetree.pattern) =
    let pattern_without_attributes = match p.ppat_desc with
    | Ppat_any -> Doc.text "_"
    | Ppat_var string_loc -> Doc.text (string_loc.txt)
    | Ppat_constant c -> print_constant c
    | Ppat_tuple patterns ->
      Doc.group(
        Doc.concat([
          Doc.text "/";
          Doc.indent (
            Doc.concat([
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map print_pattern patterns)
            ])
          );
          (* Doc.ifBreaks (Doc.text ",") Doc.nil; *)
          Doc.soft_line;
          Doc.text "/";
        ])
      )
    | Ppat_array patterns ->
      Doc.group(
        Doc.concat([
          Doc.text "[";
          Doc.indent (
            Doc.concat([
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map print_pattern patterns)
            ])
          );
          Doc.if_breaks (Doc.text ",") Doc.nil;
          Doc.soft_line;
          Doc.text "]";
        ])
      )
    | Ppat_construct({txt = Longident.Lident "[]"}, _) ->
        Doc.text "list()"
    | Ppat_construct({txt = Longident.Lident "::"}, _) ->
      let (patterns, tail) = collect_patterns_from_list_construct [] p in
      let should_hug = match (patterns, tail) with
      | ([pat],
        {ppat_desc = Ppat_construct({txt = Longident.Lident "[]"}, _)}) when ParsetreeViewer.is_huggable_pattern pat -> true
      | _ -> false
      in
      let children = Doc.concat([
        if should_hug then Doc.nil else Doc.soft_line;
        Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
          (List.map print_pattern patterns);
        begin match tail.Parsetree.ppat_desc with
        | Ppat_construct({txt = Longident.Lident "[]"}, _) -> Doc.nil
        | _ -> Doc.concat([Doc.text ","; Doc.line; Doc.text "..."; print_pattern tail])
        end;
      ]) in
      Doc.group(
        Doc.concat([
          Doc.text "list(";
          if should_hug then children else Doc.concat [
            Doc.indent children;
            Doc.if_breaks (Doc.text ",") Doc.nil;
            Doc.soft_line;
          ];
          Doc.text ")";
        ])
      )
    | Ppat_construct(constr_name, constructor_args) ->
      let constr_name = print_longident constr_name.txt in
      begin match constructor_args with
      | None -> constr_name
      | Some(args) ->
        let args = match args.ppat_desc with
        | Ppat_construct({txt = Longident.Lident "()"}, None) -> [Doc.nil]
        | Ppat_tuple(patterns) -> List.map print_pattern patterns
        | _ -> [print_pattern args]
        in
        Doc.group(
          Doc.concat([
            constr_name;
            Doc.text "(";
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  args
              ]
            );
            Doc.if_breaks (Doc.text ",") Doc.nil;
            Doc.soft_line;
            Doc.text ")";
          ])
        )
      end
    | Ppat_record(rows, open_flag) ->
        Doc.group(
          Doc.concat([
            Doc.text "{";
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map print_pattern_record_row rows);
                begin match open_flag with
                | Open -> Doc.concat [Doc.text ","; Doc.line; Doc.text "_"]
                | Closed -> Doc.nil
                end;
              ]
            );
            Doc.if_breaks (Doc.text ",") Doc.nil;
            Doc.soft_line;
            Doc.text "}";
          ])
        )

    | Ppat_exception p ->
        let needs_parens = match p.ppat_desc with
        | Ppat_or (_, _) | Ppat_alias (_, _) -> true
        | _ -> false
        in
        let pat =
          let p = print_pattern p in
          if needs_parens then
            Doc.concat [Doc.text "("; p; Doc.text ")"]
          else
            p
        in
        Doc.group (
          Doc.concat [ Doc.text "exception"; Doc.line; pat ]
        )
    | Ppat_or (p1, p2) ->
      let p1 =
        let p = print_pattern p1 in
        match p1.ppat_desc with
        | Ppat_or (_, _) -> Doc.concat [Doc.text "("; p; Doc.text ")"]
        | _ -> p
      in
      let p2 =
        let p = print_pattern p2 in
        match p2.ppat_desc with
        | Ppat_or (_, _) -> Doc.concat [Doc.text "("; p; Doc.text ")"]
        | _ -> p
      in
      Doc.group(
        Doc.concat([p1; Doc.line; Doc.text "| "; p2])
      )
    | Ppat_extension ext ->
      print_extension ext
    | Ppat_lazy p ->
      let needs_parens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let pat =
        let p = print_pattern p in
        if needs_parens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat [Doc.text "lazy "; pat]
    | Ppat_alias (p, alias_loc) ->
      let needs_parens = match p.ppat_desc with
      | Ppat_or (_, _) | Ppat_alias (_, _) -> true
      | _ -> false
      in
      let rendered_pattern =
        let p = print_pattern p in
        if needs_parens then
          Doc.concat [Doc.text "("; p; Doc.text ")"]
        else
          p
      in
      Doc.concat([
        rendered_pattern;
        Doc.text " as ";
        Doc.text alias_loc.txt
      ])

     (* Note: module(P : S) is represented as *)
     (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_constraint ({ppat_desc = Ppat_unpack string_loc}, {ptyp_desc = Ptyp_package package_type}) ->
        Doc.concat [
          Doc.text "module(";
          Doc.text string_loc.txt;
          Doc.text ": ";
          print_package_type ~print_module_keyword_and_parens:false package_type;
          Doc.rparen;
        ]
    | Ppat_constraint (pattern, typ) ->
      Doc.concat [
        print_pattern pattern;
        Doc.text ": ";
        print_typ_expr typ;
      ]

     (* Note: module(P : S) is represented as *)
     (* Ppat_constraint(Ppat_unpack, Ptyp_package) *)
    | Ppat_unpack string_loc ->
        Doc.concat [
          Doc.text "module(";
          Doc.text string_loc.txt;
          Doc.rparen;
        ]
    | _ -> failwith "unsupported pattern"
    in
    begin match p.ppat_attributes with
    | [] -> pattern_without_attributes
    | attrs ->
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          pattern_without_attributes;
        ]
      )
    end

  and print_pattern_record_row row =
    match row with
    (* punned {x}*)
    | ({Location.txt=Longident.Lident ident},
       {Parsetree.ppat_desc=Ppat_var {txt;_}}) when ident = txt ->
        Doc.text ident
    | (longident, pattern) ->
        Doc.group (
          Doc.concat([
            print_longident longident.txt;
            Doc.text ": ";
            Doc.indent(
              Doc.concat [
                Doc.soft_line;
                print_pattern pattern;
              ]
            )
          ])
        )

  and print_expression (e : Parsetree.expression) =
    let printed_expression = match e.pexp_desc with
    | Parsetree.Pexp_constant c -> print_constant c
    | Pexp_construct _ when ParsetreeViewer.has_jsx_attribute e.pexp_attributes ->
      print_jsx_fragment e
    | Pexp_construct ({txt = Longident.Lident "()"}, _) -> Doc.text "()"
    | Pexp_construct ({txt = Longident.Lident "[]"}, _) -> Doc.text "list()"
    | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
      let (expressions, spread) = ParsetreeViewer.collect_list_expressions e in
      let spread_doc = match spread with
      | Some(expr) -> Doc.concat [
          Doc.text ",";
          Doc.line;
          Doc.dotdotdot;
          print_expression expr
        ]
      | None -> Doc.nil
      in
      Doc.group(
        Doc.concat([
          Doc.text "list(";
          Doc.indent (
            Doc.concat([
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map print_expression expressions);
              spread_doc;
            ])
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ])
      )
    | Pexp_construct (longident_loc, args) ->
      let constr = print_longident longident_loc.txt in
      let args = match args with
      | None -> Doc.nil
      | Some({pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}) ->
        Doc.text "()"
      | Some({pexp_desc = Pexp_tuple args }) ->
        Doc.concat [
          Doc.lparen;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_expression args
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      | Some(arg) ->
        let arg_doc = print_expression arg in
        let should_hug = ParsetreeViewer.is_huggable_expression arg in
        Doc.concat [
          Doc.lparen;
          if should_hug then arg_doc
          else Doc.concat [
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                arg_doc;
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
          ];
          Doc.rparen;
        ]
      in
      Doc.group(Doc.concat [constr; args])
    | Pexp_ident(longident_loc) ->
      print_longident longident_loc.txt
    | Pexp_tuple exprs ->
      Doc.group(
        Doc.concat([
          Doc.text "/";
          Doc.indent (
            Doc.concat([
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map print_expression exprs)
            ])
          );
          Doc.if_breaks (Doc.text ",") Doc.nil;
          Doc.soft_line;
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
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map print_expression exprs)
            ])
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbracket;
        ])
      )
    | Pexp_record (rows, spread_expr) ->
      let spread = match spread_expr with
      | None -> Doc.nil
      | Some expr -> Doc.concat [
          Doc.dotdotdot;
          print_expression expr;
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
      let force_break =
        e.pexp_loc.loc_start.pos_lnum < e.pexp_loc.loc_end.pos_lnum
      in
      Doc.breakable_group ~force_break (
        Doc.concat([
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              spread;
              Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                (List.map print_record_row rows)
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rbrace;
        ])
      )
    | Pexp_extension extension ->
      begin match extension with
      | (
          {txt = "obj"},
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
        let force_break =
          loc.loc_start.pos_lnum < loc.loc_end.pos_lnum
        in
        Doc.breakable_group ~force_break (
          Doc.concat([
            Doc.lbrace;
            Doc.indent (
              Doc.concat [
                Doc.soft_line;
                Doc.join ~sep:(Doc.concat [Doc.text ","; Doc.line])
                  (List.map print_bs_object_row rows)
              ]
            );
            Doc.trailing_comma;
            Doc.soft_line;
            Doc.rbrace;
          ])
        )
      | extension ->
        print_extension extension
      end
    | Pexp_apply _ ->
      if ParsetreeViewer.is_unary_expression e then
        print_unary_expression e
      else if ParsetreeViewer.is_binary_expression e then
        print_binary_expression e
      else
        print_pexp_apply e
    | Pexp_unreachable -> Doc.dot
    | Pexp_field (expr, longident_loc) ->
      let lhs =
        let doc = print_expression expr in
        if Parens.field_expr expr then add_parens doc else doc
      in
      Doc.concat [
        lhs;
        Doc.dot;
        print_longident longident_loc.txt;
      ]
    | Pexp_setfield (expr1, longident_loc, expr2) ->
      print_set_field_expr e.pexp_attributes expr1 longident_loc expr2
    | Pexp_ifthenelse (if_expr, then_expr, else_expr) ->
      if ParsetreeViewer.is_ternary_expr e then
        let (parts, alternate) = ParsetreeViewer.collect_ternary_parts e in
        let ternary_doc = match parts with
        | (condition1, consequent1)::rest ->
          Doc.group (Doc.concat [
            print_ternary_operand condition1;
            Doc.indent (
              Doc.concat [
                Doc.line;
                Doc.indent (Doc.concat [Doc.text "? "; print_ternary_operand consequent1]);
                Doc.concat (
                  List.map (fun (condition, consequent) ->
                    Doc.concat [
                      Doc.line;
                      Doc.text ": ";
                      print_ternary_operand condition;
                      Doc.line;
                      Doc.text "? ";
                      print_ternary_operand consequent;
                    ]
                  ) rest
                );
                Doc.line;
                Doc.text ": ";
                Doc.indent (print_ternary_operand alternate);
              ]
            )
          ])
        | _ -> Doc.nil
        in
        let attrs = ParsetreeViewer.filter_ternary_attributes e.pexp_attributes in
        let needs_parens = match attrs with | [] -> false | _ -> true in
        Doc.concat [
          print_attributes attrs;
          if needs_parens then add_parens ternary_doc else ternary_doc;
        ]
      else
      let (ifs, else_expr) = ParsetreeViewer.collect_if_expressions e in
      let if_docs = Doc.join ~sep:Doc.space (
        List.mapi (fun i (if_expr, then_expr) ->
          let if_txt = if i > 0 then Doc.text "else if " else  Doc.text "if " in
          let condition = print_expression if_expr in
          Doc.concat [
            if_txt;
            Doc.group (
              Doc.if_breaks (add_parens condition) condition;
            );
            Doc.space;
            print_expression_block ~braces:true then_expr;
          ]
        ) ifs
      ) in
      let else_doc = match else_expr with
      | None -> Doc.nil
      | Some expr -> Doc.concat [
          Doc.text " else ";
          print_expression_block ~braces:true expr;
        ]
      in
      Doc.concat [
        print_attributes e.pexp_attributes;
        if_docs;
        else_doc;
      ]
    | Pexp_while (expr1, expr2) ->
      let condition = print_expression expr1 in
      Doc.breakable_group ~force_break:true (
        Doc.concat [
          Doc.text "while ";
          Doc.group (
            Doc.if_breaks (add_parens condition) condition
          );
          Doc.space;
          print_expression_block ~braces:true expr2;
        ]
      )
    | Pexp_for (pattern, from_expr, to_expr, direction_flag, body) ->
      Doc.breakable_group ~force_break:true (
        Doc.concat [
          Doc.text "for ";
          print_pattern pattern;
          Doc.text " in ";
          print_expression from_expr;
          print_direction_flag direction_flag;
          print_expression to_expr;
          Doc.space;
          print_expression_block ~braces:true body;
        ]
      )
    | Pexp_constraint(
        {pexp_desc = Pexp_pack mod_expr},
        {ptyp_desc = Ptyp_package package_type}
      ) ->
      Doc.group (
        Doc.concat [
          Doc.text "module(";
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              print_mod_expr mod_expr;
              Doc.text ": ";
              print_package_type ~print_module_keyword_and_parens:false package_type;
            ]
          );
          Doc.soft_line;
          Doc.rparen;
        ]
      )

    | Pexp_constraint (expr, typ) ->
      Doc.concat [
        print_expression expr;
        Doc.text ": ";
        print_typ_expr typ;
      ]
    | Pexp_letmodule ({txt = mod_name}, mod_expr, expr) ->
      print_expression_block ~braces:true e

    | Pexp_letexception (extension_constructor, expr) ->
      print_expression_block ~braces:true e
    | Pexp_assert expr ->
      let rhs =
        let doc = print_expression expr in
        if Parens.lazy_or_assert_expr_rhs expr then add_parens doc else doc
      in
      Doc.concat [
        Doc.text "assert ";
        rhs;
      ]
    | Pexp_lazy expr ->
      let rhs =
        let doc = print_expression expr in
        if Parens.lazy_or_assert_expr_rhs expr then add_parens doc else doc
      in
      Doc.concat [
        Doc.text "lazy ";
        rhs;
      ]
    | Pexp_open (override_flag, longident_loc, expr) ->
      print_expression_block ~braces:true e
    | Pexp_pack (mod_expr) ->
      Doc.group (Doc.concat [
        Doc.text "module(";
        Doc.indent (
          Doc.concat [
            Doc.soft_line;
            print_mod_expr mod_expr;
          ]
        );
        Doc.soft_line;
        Doc.rparen;
      ])
    | Pexp_sequence _ ->
      print_expression_block ~braces:true e
    | Pexp_let _ ->
      print_expression_block ~braces:true e
    | Pexp_fun _ | Pexp_newtype _ ->
      let (attrs_on_arrow, parameters, return_expr) = ParsetreeViewer.fun_expr e in
      let (uncurried, attrs) =
        ParsetreeViewer.process_uncurried_attribute attrs_on_arrow
      in
      let (return_expr, typ_constraint) = match return_expr.pexp_desc with
      | Pexp_constraint (expr, typ) -> (expr, Some typ)
      | _ -> (return_expr, None)
      in
      let parameters_doc = print_expr_fun_parameters ~in_callback:false ~uncurried parameters in
      let return_expr_doc =
        let should_inline = match return_expr.pexp_desc with
        | Pexp_array _
        | Pexp_tuple _
        | Pexp_construct (_, Some _)
        | Pexp_record _ -> true
        | _ -> false
        in
        let should_indent = match return_expr.pexp_desc with
        | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _ -> false
        | _ -> true
        in
        let return_doc = print_expression return_expr in
        if should_inline then Doc.concat [
          Doc.space;
          return_doc;
        ] else
          Doc.group (
            if should_indent then
              Doc.indent (
                Doc.concat [
                  Doc.line;
                  return_doc;
                ]
              )
            else
              Doc.concat [
                Doc.space;
                return_doc
              ]
          )
      in
      let typ_constraint_doc = match typ_constraint with
      | Some(typ) -> Doc.concat [Doc.text ": "; print_typ_expr typ]
      | _ -> Doc.nil
      in
      let attrs = match attrs with
      | [] -> Doc.nil
      | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map print_attribute attrs);
          Doc.space;
        ]
      in
      Doc.group (
        Doc.concat [
          attrs;
          parameters_doc;
          typ_constraint_doc;
          Doc.text " =>";
          return_expr_doc;
        ]
      )
    | Pexp_try (expr, cases) ->
      Doc.concat [
        Doc.text "try ";
        print_expression expr;
        Doc.text " catch ";
        print_cases cases;
      ]
    | Pexp_match (expr, cases) ->
      Doc.concat [
        Doc.text "switch ";
        print_expression expr;
        Doc.space;
        print_cases cases;
      ]
    | _ -> failwith "expression not yet implemented in printer"
    in
    let should_print_its_own_attributes = match e.pexp_desc with
    | Pexp_apply _
    | Pexp_fun _
    | Pexp_newtype _
    | Pexp_setfield _
    | Pexp_ifthenelse _ -> true
    | Pexp_construct _ when ParsetreeViewer.has_jsx_attribute e.pexp_attributes -> true
    | _ -> false
    in
    begin match e.pexp_attributes with
    | [] -> printed_expression
    | attrs when not should_print_its_own_attributes ->
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          printed_expression;
        ]
      )
    | _ -> printed_expression
    end

  and print_pexp_fun ~in_callback e =
      let (attrs_on_arrow, parameters, return_expr) = ParsetreeViewer.fun_expr e in
      let (uncurried, attrs) =
        ParsetreeViewer.process_uncurried_attribute attrs_on_arrow
      in
      let (return_expr, typ_constraint) = match return_expr.pexp_desc with
      | Pexp_constraint (expr, typ) -> (expr, Some typ)
      | _ -> (return_expr, None)
      in
      let parameters_doc = print_expr_fun_parameters ~in_callback  ~uncurried parameters in
      let return_should_indent = match return_expr.pexp_desc with
      | Pexp_sequence _ | Pexp_let _ | Pexp_letmodule _ | Pexp_letexception _ -> false
      | _ -> true
      in
      let return_expr_doc =
        let should_inline = match return_expr.pexp_desc with
        | Pexp_array _
        | Pexp_tuple _
        | Pexp_construct (_, Some _)
        | Pexp_record _ -> true
        | _ -> false
        in
        let return_doc = print_expression return_expr in
        if should_inline then Doc.concat [
          Doc.space;
          return_doc;
        ] else
          Doc.group (
            if return_should_indent then
              Doc.concat [
                Doc.indent (
                  Doc.concat [
                    Doc.line;
                    return_doc;
                  ]
                );
                if in_callback then Doc.soft_line else Doc.nil;
              ]
            else
              Doc.concat [
                Doc.space;
                return_doc;
              ]
          )
      in
      let typ_constraint_doc = match typ_constraint with
      | Some(typ) -> Doc.concat [Doc.text ": "; print_typ_expr typ]
      | _ -> Doc.nil
      in
      let attrs = match attrs with
      | [] -> Doc.nil
      | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map print_attribute attrs);
          Doc.space;
        ]
      in
      Doc.group (
        Doc.concat [
          attrs;
          parameters_doc;
          typ_constraint_doc;
          Doc.text " =>";
          return_expr_doc;
        ]
      )

  and print_ternary_operand expr =
    let doc = print_expression expr in
    if Parens.ternary_operand expr then add_parens doc else doc

  and print_set_field_expr attrs lhs longident_loc rhs =
    let rhs_doc =
      let doc = print_expression rhs in
      if Parens.set_field_expr_rhs rhs then add_parens doc else doc
    in
    let lhs_doc =
      let doc = print_expression lhs in
      if Parens.field_expr lhs then add_parens doc else doc
    in
    let should_indent = ParsetreeViewer.is_binary_expression rhs in
    let doc = Doc.concat [
      lhs_doc;
      Doc.dot;
      print_longident longident_loc.txt;
      Doc.text " =";
      if should_indent then Doc.group (
        Doc.indent (
          (Doc.concat [Doc.line; rhs_doc])
        )
      ) else
        Doc.concat [Doc.space; rhs_doc]
    ] in
    match attrs with
    | [] -> doc
    | attrs ->
      Doc.group (
        Doc.concat [
          print_attributes attrs;
          doc
        ]
      )

  and print_unary_expression expr =
    let print_unary_operator op = Doc.text (
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
      let printed_operand =
        let doc = print_expression operand in
        if Parens.unary_expr_operand operand then add_parens doc else doc
      in
      Doc.concat [
        print_unary_operator operator;
        printed_operand;
      ]
    | _ -> assert false

  and print_binary_expression (expr : Parsetree.expression) =
    let print_binary_operator ~inline_rhs operator =
      let operator_txt = match operator with
      | "|." -> "->"
      | "^" -> "++"
      | "=" -> "=="
      | "==" -> "==="
      | "<>" -> "!="
      | "!=" -> "!=="
      | txt -> txt
      in
      let spacing_before_operator =
        if operator = "|." then Doc.soft_line
        else if operator = "|>" then Doc.line
        else Doc.space;
      in
      let spacing_after_operator =
        if operator = "|." then Doc.nil
        else if operator = "|>" then Doc.space
        else if inline_rhs then Doc.space else Doc.line
      in
      Doc.concat [
        spacing_before_operator;
        Doc.text operator_txt;
        spacing_after_operator;
      ]
    in
    let print_operand ~is_lhs expr parent_operator =
      let rec flatten ~is_lhs expr parent_operator =
        if ParsetreeViewer.is_binary_expression expr then
          begin match expr with
          | {pexp_desc = Pexp_apply (
              {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
              [_, left; _, right]
            )} ->
            if ParsetreeViewer.flattenable_operators parent_operator operator &&
                not (ParsetreeViewer.has_attributes expr.pexp_attributes) then
              let left_printed = flatten ~is_lhs:true left operator in
              let right_printed =
                let (_, right_attrs) =
                  ParsetreeViewer.partition_printeable_attributes right.pexp_attributes
                in
                let doc =
                  print_expression {right with pexp_attributes = right_attrs } in
                let doc = if Parens.flatten_operand_rhs parent_operator right then
                  Doc.concat [Doc.lparen; doc; Doc.rparen]
                else
                  doc
                in
                let printeable_attrs =
                  ParsetreeViewer.filter_printeable_attributes right.pexp_attributes
                in
                Doc.concat [print_attributes printeable_attrs; doc]
              in
              Doc.concat [
                left_printed;
                print_binary_operator ~inline_rhs:false operator;
                right_printed;
              ]
            else
              let doc = print_expression {expr with pexp_attributes = []} in
              let doc = if Parens.sub_binary_expr_operand parent_operator operator ||
                (expr.pexp_attributes <> [] &&
                  (ParsetreeViewer.is_binary_expression expr ||
                ParsetreeViewer.is_ternary_expr expr)) then
                Doc.concat [Doc.lparen; doc; Doc.rparen]
              else doc
              in Doc.concat [
                print_attributes expr.pexp_attributes;
                doc
              ]
          | _ -> assert false
          end
        else
          begin match expr.pexp_desc with
          | Pexp_setfield (lhs, field, rhs) ->
            let doc = print_set_field_expr expr.pexp_attributes lhs field rhs in
            if is_lhs then add_parens doc else doc
          | Pexp_apply(
              {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
              [(Nolabel, lhs); (Nolabel, rhs)]
            ) ->
            let rhs_doc = print_expression rhs in
            let lhs_doc = print_expression lhs in
            (* TODO: unify indentation of "=" *)
            let should_indent = ParsetreeViewer.is_binary_expression rhs in
            let doc = Doc.group(
              Doc.concat [
                lhs_doc;
                Doc.text " =";
                if should_indent then Doc.group (
                  Doc.indent (Doc.concat [Doc.line; rhs_doc])
                ) else
                  Doc.concat [Doc.space; rhs_doc]
              ]
            ) in
            let doc = match expr.pexp_attributes with
            | [] -> doc
            | attrs ->
              Doc.group (
                Doc.concat [
                  print_attributes attrs;
                  doc
                ]
              )
            in
            if is_lhs then add_parens doc else doc
          | _ ->
            let doc = print_expression expr in
            if Parens.binary_expr_operand ~is_lhs expr parent_operator then
              add_parens doc
            else doc
          end
      in
      flatten ~is_lhs expr parent_operator
    in
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident (("|." | "|>") as op)}},
        [Nolabel, lhs; Nolabel, rhs]
      ) when not (
          ParsetreeViewer.is_binary_expression lhs ||
          ParsetreeViewer.is_binary_expression rhs
      ) ->
      let lhs_doc = print_operand ~is_lhs:true lhs op in
      let rhs_doc = print_operand ~is_lhs:false rhs op in
      Doc.concat [
        lhs_doc;
        (match op with
        | "|." -> Doc.text "->"
        | "|>" -> Doc.text " |> "
        | _ -> assert false);
        rhs_doc;
      ]
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident operator}},
        [Nolabel, lhs; Nolabel, rhs]
      ) ->
      let right =
        let operator_with_rhs = Doc.concat [
          print_binary_operator
            ~inline_rhs:(ParsetreeViewer.should_inline_rhs_binary_expr rhs) operator;
          print_operand ~is_lhs:false rhs operator;
        ] in
        if ParsetreeViewer.should_indent_binary_expr expr then
          Doc.group (Doc.indent operator_with_rhs)
        else operator_with_rhs
      in
      let doc = Doc.group (
        Doc.concat [
          print_operand ~is_lhs:true lhs operator;
          right
        ]
      ) in
      Doc.concat [
        print_attributes expr.pexp_attributes;
        if Parens.binary_expr expr then add_parens doc else doc
      ]
    | _ -> Doc.nil

  (* callExpr(arg1, arg2)*)
  and print_pexp_apply expr =
    match expr.pexp_desc with
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "##"}},
        [Nolabel, parent_expr; Nolabel, member_expr]
      ) ->
        let member =
          let member_doc = print_expression member_expr in
          Doc.concat [Doc.text "\""; member_doc; Doc.text "\""]
        in
        Doc.group (Doc.concat [
          print_attributes expr.pexp_attributes;
          print_expression parent_expr;
          Doc.lbracket;
          member;
          Doc.rbracket;
        ])
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Lident "#="}},
        [Nolabel, lhs; Nolabel, rhs]
      ) ->
        let rhs_doc = print_expression rhs in
        (* TODO: unify indentation of "=" *)
        let should_indent = ParsetreeViewer.is_binary_expression rhs in
        let doc = Doc.group(
          Doc.concat [
            print_expression lhs;
            Doc.text " =";
            if should_indent then Doc.group (
              Doc.indent (
                (Doc.concat [Doc.line; rhs_doc])
              )
            ) else
              Doc.concat [Doc.space; rhs_doc]
          ]
        ) in
        begin match expr.pexp_attributes with
        | [] -> doc
        | attrs ->
          Doc.group (
            Doc.concat [
              print_attributes attrs;
              doc
            ]
          )
        end
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = Longident.Ldot (Lident "Array", "get")}},
        [Nolabel, parent_expr; Nolabel, member_expr]
      ) ->
        let member =
          let member_doc = print_expression member_expr in
          let should_inline = match member_expr.pexp_desc with
          | Pexp_constant _ | Pexp_ident _ -> true
          | _ -> false
          in
          if should_inline then member_doc else (
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.soft_line;
                  member_doc;
                ]
              );
              Doc.soft_line
            ]
          )
        in
        Doc.group (Doc.concat [
          print_attributes expr.pexp_attributes;
          print_expression parent_expr;
          Doc.lbracket;
          member;
          Doc.rbracket;
        ])
    (* TODO: cleanup, are those branches even remotely performant? *)
    | Pexp_apply (
        {pexp_desc = Pexp_ident {txt = lident}},
        args
      ) when ParsetreeViewer.is_jsx_expression expr ->
      print_jsx_expression lident args
    | Pexp_apply (call_expr, args) ->
      let (uncurried, attrs) =
        ParsetreeViewer.process_uncurried_attribute expr.pexp_attributes
      in
      let call_expr_doc = print_expression call_expr in
      if ParsetreeViewer.requires_special_callback_printing args then
        let args_doc = print_arguments_with_callback ~uncurried args in
        Doc.concat [
          print_attributes attrs;
          call_expr_doc;
          args_doc;
        ]
      else
        let args_doc = print_arguments ~uncurried args in
        Doc.concat [
          print_attributes attrs;
          call_expr_doc;
          args_doc;
        ]
    | _ -> assert false

  and print_jsx_expression lident args =
    let name = print_jsx_name lident in
    let (formatted_props, children) = format_jsx_props args in
    (* <div className="test" /> *)
    let is_self_closing = match children with | [] -> true | _ -> false in
    Doc.group (
      Doc.concat [
        Doc.group (
          Doc.concat [
            Doc.less_than;
            name;
            formatted_props;
            if is_self_closing then Doc.concat [Doc.line; Doc.text "/>"] else Doc.nil
          ]
        );
        if is_self_closing then Doc.nil
        else
          Doc.concat [
            Doc.greater_than;
            Doc.indent (
              Doc.concat [
                Doc.line;
                print_jsx_children children;
              ]
            );
            Doc.line;
            Doc.text "</";
            name;
            Doc.greater_than;
          ]
      ]
    )

  and print_jsx_fragment expr =
    let opening = Doc.text "<>" in
    let closing = Doc.text "</>" in
    let (children, _) = ParsetreeViewer.collect_list_expressions expr in
    Doc.group (
      Doc.concat [
        opening;
        begin match children with
        | [] -> Doc.nil
        | children ->
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_jsx_children children;
            ]
          )
        end;
        Doc.line;
        closing;
      ]
    )

  and print_jsx_children (children: Parsetree.expression list) =
    Doc.group (
      Doc.join ~sep:Doc.line (
        List.map (fun expr ->
          let expr_doc = print_expression expr in
          if Parens.jsx_child_expr expr then add_braces expr_doc else expr_doc
        ) children
      )
    )

  and format_jsx_props args =
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
        let formatted_props = Doc.indent (
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
        let (children, _) = ParsetreeViewer.collect_list_expressions children in
        (formatted_props, children)
      | arg::args ->
        let prop_doc = format_jsx_prop arg in
        loop (prop_doc::props) args
    in
    loop [] args

  and format_jsx_prop arg =
    match arg with
    | (
        (Asttypes.Labelled lbl_txt | Optional lbl_txt) as lbl,
        {
          Parsetree.pexp_attributes = [];
          pexp_desc = Pexp_ident {txt = Longident.Lident ident}
        }
      ) when lbl_txt = ident (* jsx punning *) ->

      begin match lbl with
      | Nolabel -> Doc.nil
      | Labelled lbl -> Doc.text lbl
      | Optional lbl -> Doc.text ("?" ^ lbl)
      end
    | (lbl, expr) ->
      let lbl_doc = match lbl with
      | Asttypes.Labelled lbl -> Doc.text (lbl ^ "=")
      | Asttypes.Optional lbl -> Doc.text (lbl ^ "=?")
      | Nolabel -> Doc.nil
      in
      let expr_doc = print_expression expr in
      Doc.concat [
        lbl_doc;
        if Parens.jsx_prop_expr expr then add_braces expr_doc else expr_doc;
      ]

  (* div -> div.
   * Navabar.createElement -> Navbar
   * Staff.Users.createElement -> Staff.Users *)
  and print_jsx_name lident =
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

  and print_arguments_with_callback ~uncurried args =
    let rec loop acc args = match args with
    | [] -> (Doc.nil, Doc.nil)
    | [_lbl, expr] ->
      let callback = print_pexp_fun ~in_callback:true expr in
      (Doc.concat (List.rev acc), callback)
    | arg::args ->
      let arg_doc = print_argument arg in
      loop (Doc.line::Doc.comma::arg_doc::acc) args
    in
    let (printed_args, callback) = loop [] args in

    (* Thing.map(foo,(arg1, arg2) => MyModuleBlah.toList(argument)) *)
    let fits_on_one_line = Doc.concat [
      if uncurried then Doc.text "(." else Doc.lparen;
      Doc.concat [
        printed_args;
        callback;
      ];
      Doc.rparen;
    ] in

    (* Thing.map(longArgumet, veryLooooongArgument, (arg1, arg2) =>
     *   MyModuleBlah.toList(argument)
     * )
     *)
    let arugments_fit_on_one_line =
      Doc.concat [
        if uncurried then Doc.text "(." else Doc.lparen;
        Doc.concat [
          Doc.soft_line;
          printed_args;
          Doc.breakable_group ~force_break:true callback;
        ];
        Doc.soft_line;
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
    let break_all_args = print_arguments ~uncurried args in
    Doc.custom_layout [
      fits_on_one_line;
      arugments_fit_on_one_line;
      break_all_args;
    ]

	and print_arguments ~uncurried (args : (Asttypes.arg_label * Parsetree.expression) list) =
		match args with
		| [Nolabel, {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, _)}] ->
      if uncurried then Doc.text "(.)" else Doc.text "()"
    | [(Nolabel, arg)] when ParsetreeViewer.is_huggable_expression arg ->
      Doc.concat [
        if uncurried then Doc.text "(." else Doc.lparen;
        print_expression arg;
        Doc.rparen;
      ]
		| args -> Doc.group (
				Doc.concat [
          if uncurried then Doc.text "(." else Doc.lparen;
					Doc.indent (
						Doc.concat [
              if uncurried then Doc.line else Doc.soft_line;
							Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
								List.map print_argument args
							)
						]
					);
					Doc.trailing_comma;
					Doc.soft_line;
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
	and print_argument ((arg_lbl, arg) : Asttypes.arg_label * Parsetree.expression) =
		match (arg_lbl, arg) with
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
			let printed_lbl = match arg_lbl with
			| Asttypes.Nolabel -> Doc.nil
			| Asttypes.Labelled lbl -> Doc.text ("~" ^ lbl ^ "=")
			| Asttypes.Optional lbl -> Doc.text ("~" ^ lbl ^ "=?")
			in
			let printed_expr = print_expression expr in
			Doc.concat [
				printed_lbl;
				printed_expr;
			]

  and print_cases (cases: Parsetree.case list) =
    Doc.breakable_group ~force_break:true (
      Doc.concat [
        Doc.lbrace;
          Doc.concat [
            Doc.line;
            Doc.join ~sep:Doc.line (
              List.map print_case cases
            )
          ];
        Doc.line;
        Doc.rbrace;
      ]
    )

  and print_case (case: Parsetree.case) =
    let rhs = match case.pc_rhs.pexp_desc with
    | Pexp_let _
    | Pexp_letmodule _
    | Pexp_letexception _
    | Pexp_open _
    | Pexp_sequence _ ->
      print_expression_block ~braces:false case.pc_rhs
    | _ -> print_expression case.pc_rhs
    in
    let guard = match case.pc_guard with
    | None -> Doc.nil
    | Some expr -> Doc.group (
        Doc.concat [
          Doc.line;
          Doc.text "when ";
          print_expression expr;
        ]
      )
    in
    Doc.group (
      Doc.concat [
        Doc.text "| ";
        Doc.indent (
          Doc.concat [
            print_pattern case.pc_lhs;
            guard;
            Doc.text " =>";
            Doc.line;
            rhs;
          ]
        );
      ]
    )

  and print_expr_fun_parameters ~in_callback ~uncurried parameters =
    match parameters with
    (* let f = _ => () *)
    | [([], Asttypes.Nolabel, None, {Parsetree.ppat_desc = Ppat_any})] when not uncurried ->
      Doc.text "_"
    (* let f = a => () *)
    | [([], Asttypes.Nolabel, None, {Parsetree.ppat_desc = Ppat_var string_loc})]  when not uncurried ->
      Doc.text string_loc.txt
    (* let f = () => () *)
    | [([], Nolabel, None, {ppat_desc = Ppat_construct({txt = Longident.Lident "()"}, None)})] when not uncurried ->
      Doc.text "()"
    (* let f = (~greeting, ~from as hometown, ~x=?) => () *)
    | parameters ->
      let lparen = if uncurried then Doc.text "(. " else Doc.lparen in
      let should_hug = ParsetreeViewer.parameters_should_hug parameters in
      let printed_paramaters = Doc.concat [
        if should_hug || in_callback then Doc.nil else Doc.soft_line;
        Doc.join ~sep:(Doc.concat [Doc.comma; if in_callback then Doc.space else Doc.line])
          (List.map print_exp_fun_parameter parameters)
      ] in
      Doc.group (
        Doc.concat [
          lparen;
          if should_hug || in_callback then printed_paramaters else Doc.indent (printed_paramaters);
          if should_hug || in_callback then Doc.nil else Doc.concat [Doc.trailing_comma; Doc.soft_line];
          Doc.rparen;
        ]
      )

  and print_exp_fun_parameter (attrs, lbl, default_expr, pattern) =
    let (is_uncurried, attrs) = ParsetreeViewer.process_uncurried_attribute attrs in
    let uncurried = if is_uncurried then Doc.concat [Doc.dot; Doc.space] else Doc.nil in
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map print_attribute attrs);
      Doc.line;
    ] in
    (* =defaultValue *)
    let default_expr_doc = match default_expr with
    | Some expr -> Doc.concat [
        Doc.text "=";
        print_expression expr
      ]
    | None -> Doc.nil
    in
    (* ~from as hometown
     * ~from                   ->  punning *)
    let label_with_pattern = match (lbl, pattern) with
    | (Asttypes.Nolabel, pattern) -> print_pattern pattern
    | (
        (Asttypes.Labelled lbl | Optional lbl),
        {ppat_desc = Ppat_var string_loc}
      ) when lbl = string_loc.txt ->
      Doc.concat [
        Doc.text "~";
        Doc.text lbl;
      ]
    | ((Asttypes.Labelled lbl | Optional lbl), pattern) ->
      Doc.concat [
        Doc.text "~";
        Doc.text lbl;
        Doc.text " as ";
        print_pattern pattern;
      ]
    in
    let optional_label_suffix = match (lbl, default_expr) with
    | (Asttypes.Optional _, None) -> Doc.text "=?"
    | _ -> Doc.nil
    in
    Doc.group (
      Doc.concat [
        uncurried;
        attrs;
        label_with_pattern;
        default_expr_doc;
        optional_label_suffix;
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
  and print_expression_block ~braces expr =
    let rec collect_rows acc expr = match expr.Parsetree.pexp_desc with
    | Parsetree.Pexp_letmodule ({txt = mod_name; loc = mod_loc}, mod_expr, expr) ->
      let let_module_doc = Doc.concat [
        Doc.text "module ";
        Doc.text mod_name;
        Doc.text " = ";
        print_mod_expr mod_expr;
      ] in
      let loc = {mod_loc with loc_end = mod_expr.pmod_loc.loc_end} in
      collect_rows ((loc, let_module_doc)::acc) expr
    | Pexp_letexception (extension_constructor, expr) ->
      let let_exception_doc = print_exception_def extension_constructor in
      let loc = extension_constructor.pext_loc in
      collect_rows ((loc, let_exception_doc)::acc) expr
    | Pexp_open (override_flag, longident_loc, expr) ->
      let open_doc = Doc.concat [
        Doc.text "open";
        print_override_flag override_flag;
        Doc.space;
        print_longident longident_loc.txt;
      ] in
      let loc = longident_loc.loc in
      collect_rows ((loc, open_doc)::acc) expr
    | Pexp_sequence (expr1, expr2) ->
      let expr_doc =
        let doc = print_expression expr1 in
        if Parens.block_expr expr1 then add_parens doc else doc
      in
      let loc = expr1.pexp_loc in
      collect_rows ((loc, expr_doc)::acc) expr2
    | Pexp_let (rec_flag, value_bindings, expr) ->
			let rec_flag = match rec_flag with
			| Asttypes.Nonrecursive -> Doc.nil
			| Asttypes.Recursive -> Doc.text "rec "
			in
      let let_doc = print_value_bindings ~rec_flag value_bindings in
      let loc = match (value_bindings, List.rev value_bindings) with
      | ({pvb_loc = first_loc}::_,{pvb_loc = last_loc}::_) ->
          {first_loc with loc_end = last_loc.loc_end}
      | _ -> Location.none
      in
      collect_rows((loc, let_doc)::acc) expr
    | _ ->
      let expr_doc =
        let doc = print_expression expr in
        if Parens.block_expr expr then add_parens doc else doc
      in
      List.rev ((expr.pexp_loc, expr_doc)::acc)
    in
    let block = collect_rows [] expr |> interleave_whitespace ~force_break:true in
    Doc.breakable_group ~force_break:true (
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

  and print_override_flag override_flag = match override_flag with
    | Asttypes.Override -> Doc.text "!"
    | Fresh -> Doc.nil

  and print_direction_flag flag = match flag with
    | Asttypes.Downto -> Doc.text " downto "
    | Asttypes.Upto -> Doc.text " to "

  and print_record_row (lbl, expr) =
    Doc.concat [
      print_longident lbl.txt;
      Doc.text ": ";
      print_expression expr;
    ]

  and print_bs_object_row (lbl, expr) =
    Doc.concat [
      Doc.text "\"";
      print_longident lbl.txt;
      Doc.text "\"";
      Doc.text ": ";
      print_expression expr;
    ]
  (* The optional loc indicates whether we need to print the attributes in
   * relation to some location. In practise this means the following:
   *  `@attr type t = string` -> on the same line, print on the same line
   *  `@attr
   *   type t = string` -> attr is on prev line, print the attributes
   *   with a line break between, we respect the users' original layout *)
  and print_attributes ?loc (attrs: Parsetree.attributes) =
    match attrs with
    | [] -> Doc.nil
    | attrs ->
      let line_break = match loc with
      | None -> Doc.line
      | Some loc -> begin match List.rev attrs with
        | ({loc = first_loc}, _)::_ when loc.loc_start.pos_lnum > first_loc.loc_end.pos_lnum ->
          Doc.literal_line;
        | _ -> Doc.line
        end
      in
      Doc.concat [
        Doc.group (Doc.join ~sep:Doc.line (List.map print_attribute attrs));
        line_break;
      ]

  and print_attribute ((id, payload) : Parsetree.attribute) =
      let attr_name = Doc.text ("@" ^ id.txt) in
        match payload with
      | PStr [{pstr_desc = Pstr_eval (expr, attrs)}] ->
        let expr_doc = print_expression expr in
        let needs_parens = match attrs with | [] -> false | _ -> true in
        Doc.group (
          Doc.concat [
            attr_name;
            add_parens (
              Doc.concat [
                print_attributes attrs;
                if needs_parens then add_parens expr_doc else expr_doc;
              ]
            )
          ]
        )
      | _ -> attr_name


  and print_mod_expr mod_expr =
    match mod_expr.pmod_desc with
    | Pmod_ident longident_loc ->
      print_longident longident_loc.txt
    | Pmod_structure structure ->
      Doc.breakable_group ~force_break:true (
        Doc.concat [
          Doc.lbrace;
          Doc.indent (
            Doc.concat [
              Doc.soft_line;
              print_structure structure;
            ];
          );
          Doc.soft_line;
          Doc.rbrace;
        ]
      )
    | Pmod_unpack expr ->
      let should_hug = match expr.pexp_desc with
      | Pexp_let _ -> true
      | Pexp_constraint (
          {pexp_desc = Pexp_let _ },
          {ptyp_desc = Ptyp_package package_type}
        ) -> true
      | _ -> false
      in
      let (expr, module_constraint) = match expr.pexp_desc with
      | Pexp_constraint (
          expr,
          {ptyp_desc = Ptyp_package package_type}
      ) ->
        let type_doc = Doc.group (Doc.concat [
          Doc.text ":";
          Doc.indent (
            Doc.concat [
              Doc.line;
              print_package_type ~print_module_keyword_and_parens:false package_type
            ]
          )
        ]) in
        (expr, type_doc)
      | _ -> (expr, Doc.nil)
      in
      let unpack_doc = Doc.group(Doc.concat [
        print_expression expr;
        module_constraint;
      ]) in
      Doc.group (
        Doc.concat [
          Doc.text "unpack(";
          if should_hug then unpack_doc
          else
            Doc.concat [
              Doc.indent (
                Doc.concat [
                  Doc.soft_line;
                  unpack_doc;
                ]
              );
             Doc.soft_line;
            ];
          Doc.rparen;
        ]
      )
    | Pmod_extension extension ->
      print_extension extension
    | Pmod_apply _ ->
      let (args, call_expr) = ParsetreeViewer.mod_expr_apply mod_expr in
      let is_unit_sugar = match args with
      | [{pmod_desc = Pmod_structure []}] -> true
      | _ -> false
      in
      let should_hug = match args with
      | [{pmod_desc = Pmod_structure _}] -> true
      | _ -> false
      in
      Doc.group (
        Doc.concat [
          print_mod_expr call_expr;
          if is_unit_sugar then
            print_mod_apply_arg (List.hd args)
          else
            Doc.concat [
              Doc.lparen;
              if should_hug then
                print_mod_apply_arg (List.hd args)
              else
                Doc.indent (
                  Doc.concat [
                    Doc.soft_line;
                    Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                      List.map print_mod_apply_arg args
                    )
                  ]
                );
              if not should_hug then
                Doc.concat [
                  Doc.trailing_comma;
                  Doc.soft_line;
                ]
              else Doc.nil;
              Doc.rparen;
            ]
        ]
      )
    | Pmod_constraint (mod_expr, mod_type) ->
      Doc.concat [
        print_mod_expr mod_expr;
        Doc.text ": ";
        print_mod_type mod_type;
      ]
    | Pmod_functor _ ->
      print_mod_functor mod_expr

  and print_mod_functor mod_expr =
    let (parameters, return_mod_expr) = ParsetreeViewer.mod_expr_functor mod_expr in
    (* let shouldInline = match returnModExpr.pmod_desc with *)
    (* | Pmod_structure _ | Pmod_ident _ -> true *)
    (* | Pmod_constraint ({pmod_desc = Pmod_structure _}, _) -> true *)
    (* | _ -> false *)
    (* in *)
    let (return_constraint, return_mod_expr) = match return_mod_expr.pmod_desc with
    | Pmod_constraint (mod_expr, mod_type) ->
      let constraint_doc =
        let doc = print_mod_type mod_type in
        if Parens.mod_expr_functor_constraint mod_type then add_parens doc else doc
      in
      let mod_constraint = Doc.concat [
        Doc.text ": ";
        constraint_doc;
      ] in
      (mod_constraint, print_mod_expr mod_expr)
    | _ -> (Doc.nil, print_mod_expr return_mod_expr)
    in
    let parameters_doc = match parameters with
    | [(attrs, {txt = "*"}, None)] ->
        let attrs = match attrs with
        | [] -> Doc.nil
        | attrs -> Doc.concat [
          Doc.join ~sep:Doc.line (List.map print_attribute attrs);
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
              Doc.soft_line;
              Doc.join ~sep:(Doc.concat [Doc.comma; Doc.line]) (
                List.map print_mod_functor_param parameters
              )
            ]
          );
          Doc.trailing_comma;
          Doc.soft_line;
          Doc.rparen;
        ]
      )
    in
    Doc.group (
      Doc.concat [
        parameters_doc;
        return_constraint;
        Doc.text " => ";
        return_mod_expr

      ]
    )

  and print_mod_functor_param (attrs, lbl, opt_mod_type) =
    let attrs = match attrs with
    | [] -> Doc.nil
    | attrs -> Doc.concat [
      Doc.join ~sep:Doc.line (List.map print_attribute attrs);
      Doc.line;
    ] in
    Doc.group (
      Doc.concat [
        attrs;
        Doc.text lbl.txt;
        (match opt_mod_type with
        | None -> Doc.nil
        | Some mod_type ->
          Doc.concat [
            Doc.text ": ";
            print_mod_type mod_type
          ]);
      ]
    )

  and print_mod_apply_arg mod_expr =
    match mod_expr.pmod_desc with
    | Pmod_structure [] -> Doc.text "()"
    | _ -> print_mod_expr mod_expr


  and print_exception_def (constr : Parsetree.extension_constructor) =
    let kind = match constr.pext_kind with
    | Pext_rebind {txt = longident} -> Doc.indent (
        Doc.concat [
          Doc.text " =";
          Doc.line;
          print_longident longident;
        ]
     )
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadt_doc = match gadt with
      | Some typ -> Doc.concat [
          Doc.text ": ";
          print_typ_expr typ;
        ]
      | None -> Doc.nil
      in
      Doc.concat [
        print_constructor_arguments args;
        gadt_doc
      ]
    in
    Doc.group (
      Doc.concat [
        print_attributes constr.pext_attributes;
        Doc.text "exception ";
        Doc.text constr.pext_name.txt;
        kind
      ]
    )

  and print_extension_constructor i (constr : Parsetree.extension_constructor) =
    let attrs = print_attributes constr.pext_attributes in
    let bar = if i > 0 then Doc.text "| "
      else Doc.if_breaks (Doc.text "| ") Doc.nil
    in
    let kind = match constr.pext_kind with
    | Pext_rebind {txt = longident} -> Doc.indent (
        Doc.concat [
          Doc.text " =";
          Doc.line;
          print_longident longident;
        ]
     )
    | Pext_decl (Pcstr_tuple [], None) -> Doc.nil
    | Pext_decl (args, gadt) ->
      let gadt_doc = match gadt with
      | Some typ -> Doc.concat [
          Doc.text ": ";
          print_typ_expr typ;
        ]
      | None -> Doc.nil
      in
      Doc.concat [
        print_constructor_arguments args;
        gadt_doc
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

  let print_implementation (s: Parsetree.structure) comments src =
    let t = CommentAst.init_structure s comments in

    let string_doc = Doc.to_string ~width:80 (print_structure s) in
    print_endline string_doc;
    print_newline()

  let print_interface (s: Parsetree.signature) =
    let string_doc = Doc.to_string ~width:80 (print_signature s) in
    print_endline string_doc;
    print_newline()
end

