let concat_longidents l1 l2 =
  let parts1 = Longident.flatten l1 in
  let parts2 = Longident.flatten l2 in
  match List.concat [parts1; parts2] |> Longident.unflatten with
  | Some longident -> longident
  | None -> l2

(* TODO: support nested open's ? *)
let rec rewrite_ppat_open longident_open pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_array (first :: rest) ->
    (* Color.[Red, Blue, Green] -> [Color.Red, Blue, Green] *)
    {
      pat with
      ppat_desc = Ppat_array (rewrite_ppat_open longident_open first :: rest);
    }
  | Ppat_tuple (first :: rest) ->
    (* Color.(Red, Blue, Green) -> (Color.Red, Blue, Green) *)
    {
      pat with
      ppat_desc = Ppat_tuple (rewrite_ppat_open longident_open first :: rest);
    }
  | Ppat_construct
      ( ({txt = Longident.Lident "::"} as list_constructor),
        Some ({ppat_desc = Ppat_tuple (pat :: rest)} as element) ) ->
    (* Color.(list[Red, Blue, Green]) -> list[Color.Red, Blue, Green] *)
    {
      pat with
      ppat_desc =
        Ppat_construct
          ( list_constructor,
            Some
              {
                element with
                ppat_desc =
                  Ppat_tuple (rewrite_ppat_open longident_open pat :: rest);
              } );
    }
  | Ppat_construct (({txt = constructor} as longident_loc), opt_pattern) ->
    (* Foo.(Bar(a)) -> Foo.Bar(a) *)
    {
      pat with
      ppat_desc =
        Ppat_construct
          ( {
              longident_loc with
              txt = concat_longidents longident_open constructor;
            },
            opt_pattern );
    }
  | Ppat_record ((({txt = lbl} as longident_loc), first_pat) :: rest, flag) ->
    (* Foo.{x} -> {Foo.x: x} *)
    let first_row =
      ( {longident_loc with txt = concat_longidents longident_open lbl},
        first_pat )
    in
    {pat with ppat_desc = Ppat_record (first_row :: rest, flag)}
  | Ppat_or (pat1, pat2) ->
    {
      pat with
      ppat_desc =
        Ppat_or
          ( rewrite_ppat_open longident_open pat1,
            rewrite_ppat_open longident_open pat2 );
    }
  | Ppat_constraint (pattern, typ) ->
    {
      pat with
      ppat_desc = Ppat_constraint (rewrite_ppat_open longident_open pattern, typ);
    }
  | Ppat_type ({txt = constructor} as longident_loc) ->
    {
      pat with
      ppat_desc =
        Ppat_type
          {
            longident_loc with
            txt = concat_longidents longident_open constructor;
          };
    }
  | Ppat_lazy p ->
    {pat with ppat_desc = Ppat_lazy (rewrite_ppat_open longident_open p)}
  | Ppat_exception p ->
    {pat with ppat_desc = Ppat_exception (rewrite_ppat_open longident_open p)}
  | _ -> pat

let escape_template_literal s =
  let len = String.length s in
  let b = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    let c = (String.get [@doesNotRaise]) s !i in
    if c = '`' then (
      Buffer.add_char b '\\';
      Buffer.add_char b '`';
      incr i)
    else if c = '$' then
      if !i + 1 < len then (
        let c2 = (String.get [@doesNotRaise]) s (!i + 1) in
        if c2 = '{' then (
          Buffer.add_char b '\\';
          Buffer.add_char b '$';
          Buffer.add_char b '{')
        else (
          Buffer.add_char b c;
          Buffer.add_char b c2);
        i := !i + 2)
      else (
        Buffer.add_char b c;
        incr i)
    else if c = '\\' then (
      Buffer.add_char b '\\';
      Buffer.add_char b '\\';
      incr i)
    else (
      Buffer.add_char b c;
      incr i)
  done;
  Buffer.contents b

let escape_string_contents s =
  let len = String.length s in
  let b = Buffer.create len in

  let i = ref 0 in

  while !i < len do
    let c = String.unsafe_get s !i in
    if c = '\\' then (
      incr i;
      Buffer.add_char b c;
      let c = String.unsafe_get s !i in
      if !i < len then
        let () = Buffer.add_char b c in
        incr i
      else ())
    else if c = '"' then (
      Buffer.add_char b '\\';
      Buffer.add_char b c;
      incr i)
    else (
      Buffer.add_char b c;
      incr i)
  done;
  Buffer.contents b

let looks_like_recursive_type_declaration type_declaration =
  let open Parsetree in
  let name = type_declaration.ptype_name.txt in
  let rec check_kind kind =
    match kind with
    | Ptype_abstract | Ptype_open -> false
    | Ptype_variant constructor_declarations ->
      List.exists check_constructor_declaration constructor_declarations
    | Ptype_record label_declarations ->
      List.exists check_label_declaration label_declarations
  and check_constructor_declaration constr_decl =
    check_constructor_arguments constr_decl.pcd_args
    ||
    match constr_decl.pcd_res with
    | Some typexpr -> check_typ_expr typexpr
    | None -> false
  and check_label_declaration label_declaration =
    check_typ_expr label_declaration.pld_type
  and check_constructor_arguments constr_arg =
    match constr_arg with
    | Pcstr_tuple types -> List.exists check_typ_expr types
    | Pcstr_record label_declarations ->
      List.exists check_label_declaration label_declarations
  and check_typ_expr typ =
    match typ.ptyp_desc with
    | Ptyp_any -> false
    | Ptyp_var _ -> false
    | Ptyp_object (fields, _) -> List.exists check_object_field fields
    | Ptyp_class _ -> false
    | Ptyp_package _ -> false
    | Ptyp_extension _ -> false
    | Ptyp_arrow (_lbl, typ1, typ2) ->
      check_typ_expr typ1 || check_typ_expr typ2
    | Ptyp_tuple types -> List.exists check_typ_expr types
    | Ptyp_constr ({txt = longident}, types) ->
      (match longident with
      | Lident ident -> ident = name
      | _ -> false)
      || List.exists check_typ_expr types
    | Ptyp_alias (typ, _) -> check_typ_expr typ
    | Ptyp_variant (row_fields, _, _) -> List.exists check_row_fields row_fields
    | Ptyp_poly (_, typ) -> check_typ_expr typ
  and check_object_field field =
    match field with
    | Otag (_label, _attrs, typ) -> check_typ_expr typ
    | Oinherit typ -> check_typ_expr typ
  and check_row_fields row_field =
    match row_field with
    | Rtag (_, _, _, types) -> List.exists check_typ_expr types
    | Rinherit typexpr -> check_typ_expr typexpr
  and check_manifest manifest =
    match manifest with
    | Some typ -> check_typ_expr typ
    | None -> false
  in
  check_kind type_declaration.ptype_kind
  || check_manifest type_declaration.ptype_manifest

let filter_reason_raw_literal attrs =
  List.filter
    (fun attr ->
      match attr with
      | {Location.txt = "reason.raw_literal"}, _ -> false
      | _ -> true)
    attrs

let string_literal_mapper string_data =
  let is_same_location l1 l2 =
    let open Location in
    l1.loc_start.pos_cnum == l2.loc_start.pos_cnum
  in
  let remaining_string_data = string_data in
  let open Ast_mapper in
  {
    default_mapper with
    expr =
      (fun mapper expr ->
        match expr.pexp_desc with
        | Pexp_constant (Pconst_string (_txt, None)) -> (
          match
            List.find_opt
              (fun (_stringData, string_loc) ->
                is_same_location string_loc expr.pexp_loc)
              remaining_string_data
          with
          | Some (string_data, _) ->
            let string_data =
              let attr =
                List.find_opt
                  (fun attr ->
                    match attr with
                    | {Location.txt = "reason.raw_literal"}, _ -> true
                    | _ -> false)
                  expr.pexp_attributes
              in
              match attr with
              | Some
                  ( _,
                    PStr
                      [
                        {
                          pstr_desc =
                            Pstr_eval
                              ( {
                                  pexp_desc =
                                    Pexp_constant (Pconst_string (raw, _));
                                },
                                _ );
                        };
                      ] ) ->
                raw
              | _ ->
                (String.sub [@doesNotRaise]) string_data 1
                  (String.length string_data - 2)
            in
            {
              expr with
              pexp_attributes = filter_reason_raw_literal expr.pexp_attributes;
              pexp_desc = Pexp_constant (Pconst_string (string_data, None));
            }
          | None -> default_mapper.expr mapper expr)
        | _ -> default_mapper.expr mapper expr);
  }

let has_uncurried_attribute attrs =
  List.exists
    (fun attr ->
      match attr with
      | {Asttypes.txt = "bs"}, Parsetree.PStr [] -> true
      | _ -> false)
    attrs

let template_literal_attr = (Location.mknoloc "res.template", Parsetree.PStr [])

let normalize =
  let open Ast_mapper in
  {
    default_mapper with
    attributes =
      (fun mapper attrs ->
        attrs
        |> List.filter (fun attr ->
               match attr with
               | ( {
                     Location.txt =
                       ( "reason.preserve_braces" | "explicit_arity"
                       | "implicity_arity" );
                   },
                   _ ) ->
                 false
               | _ -> true)
        |> default_mapper.attributes mapper);
    pat =
      (fun mapper p ->
        match p.ppat_desc with
        | Ppat_open ({txt = longident_open}, pattern) ->
          let p = rewrite_ppat_open longident_open pattern in
          default_mapper.pat mapper p
        | Ppat_constant (Pconst_string (txt, tag)) ->
          let new_tag =
            match tag with
            (* transform {|abc|} into {js|abc|js}, because `template string` is interpreted as {js||js} *)
            | Some "" -> Some "js"
            | tag -> tag
          in
          let s =
            Parsetree.Pconst_string (escape_template_literal txt, new_tag)
          in
          {
            p with
            ppat_attributes =
              template_literal_attr
              :: mapper.attributes mapper p.ppat_attributes;
            ppat_desc = Ppat_constant s;
          }
        | _ -> default_mapper.pat mapper p);
    typ =
      (fun mapper typ ->
        match typ.ptyp_desc with
        | Ptyp_constr
            ({txt = Longident.Ldot (Longident.Lident "Js", "t")}, [arg]) ->
          (* Js.t({"a": b}) -> {"a": b}
             Since compiler >9.0.1 objects don't need Js.t wrapping anymore *)
          mapper.typ mapper arg
        | _ -> default_mapper.typ mapper typ);
    expr =
      (fun mapper expr ->
        match expr.pexp_desc with
        | Pexp_constant (Pconst_string (txt, None)) ->
          let raw = escape_string_contents txt in
          let s = Parsetree.Pconst_string (raw, None) in
          {expr with pexp_desc = Pexp_constant s}
        | Pexp_constant (Pconst_string (txt, tag)) ->
          let new_tag =
            match tag with
            (* transform {|abc|} into {js|abc|js}, we want to preserve unicode by default *)
            | Some "" -> Some "js"
            | tag -> tag
          in
          let s =
            Parsetree.Pconst_string (escape_template_literal txt, new_tag)
          in
          {
            expr with
            pexp_attributes =
              template_literal_attr
              :: mapper.attributes mapper expr.pexp_attributes;
            pexp_desc = Pexp_constant s;
          }
        | Pexp_apply
            ( call_expr,
              [
                ( Nolabel,
                  ({
                     pexp_desc =
                       Pexp_construct ({txt = Longident.Lident "()"}, None);
                     pexp_attributes = [];
                   } as unit_expr) );
              ] )
          when has_uncurried_attribute expr.pexp_attributes ->
          {
            expr with
            pexp_attributes = mapper.attributes mapper expr.pexp_attributes;
            pexp_desc =
              Pexp_apply
                ( call_expr,
                  [
                    ( Nolabel,
                      {
                        unit_expr with
                        pexp_loc = {unit_expr.pexp_loc with loc_ghost = true};
                      } );
                  ] );
          }
        | Pexp_function cases ->
          let loc =
            match (cases, List.rev cases) with
            | first :: _, last :: _ ->
              {
                first.pc_lhs.ppat_loc with
                loc_end = last.pc_rhs.pexp_loc.loc_end;
              }
            | _ -> Location.none
          in
          let var =
            {
              Parsetree.ppat_loc = Location.none;
              ppat_attributes = [];
              ppat_desc = Ppat_var (Location.mknoloc "x");
            }
          in
          {
            pexp_loc = loc;
            pexp_attributes = [];
            pexp_desc =
              Pexp_fun
                ( Asttypes.Nolabel,
                  None,
                  var,
                  {
                    pexp_loc = loc;
                    pexp_attributes = [];
                    pexp_desc =
                      Pexp_match
                        ( {
                            pexp_loc = Location.none;
                            pexp_attributes = [];
                            pexp_desc =
                              Pexp_ident
                                (Location.mknoloc (Longident.Lident "x"));
                          },
                          mapper.cases mapper cases );
                  } );
          }
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Longident.Lident "!"}},
              [(Asttypes.Nolabel, operand)] ) ->
          (* turn `!foo` into `foo.contents` *)
          {
            pexp_loc = expr.pexp_loc;
            pexp_attributes = expr.pexp_attributes;
            pexp_desc =
              Pexp_field
                ( mapper.expr mapper operand,
                  Location.mknoloc (Longident.Lident "contents") );
          }
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Longident.Lident "##"}},
              [
                (Asttypes.Nolabel, lhs);
                ( Nolabel,
                  {
                    pexp_desc =
                      ( Pexp_constant (Pconst_string (txt, None))
                      | Pexp_ident {txt = Longident.Lident txt} );
                    pexp_loc = label_loc;
                  } );
              ] ) ->
          let label = Location.mkloc txt label_loc in
          {
            pexp_loc = expr.pexp_loc;
            pexp_attributes = expr.pexp_attributes;
            pexp_desc = Pexp_send (mapper.expr mapper lhs, label);
          }
        | Pexp_match
            ( condition,
              [
                {
                  pc_lhs =
                    {
                      ppat_desc =
                        Ppat_construct ({txt = Longident.Lident "true"}, None);
                    };
                  pc_rhs = then_expr;
                };
                {
                  pc_lhs =
                    {
                      ppat_desc =
                        Ppat_construct ({txt = Longident.Lident "false"}, None);
                    };
                  pc_rhs = else_expr;
                };
              ] ) ->
          let ternary_marker =
            (Location.mknoloc "res.ternary", Parsetree.PStr [])
          in
          {
            Parsetree.pexp_loc = expr.pexp_loc;
            pexp_desc =
              Pexp_ifthenelse
                ( mapper.expr mapper condition,
                  mapper.expr mapper then_expr,
                  Some (mapper.expr mapper else_expr) );
            pexp_attributes = ternary_marker :: expr.pexp_attributes;
          }
        | _ -> default_mapper.expr mapper expr);
    structure_item =
      (fun mapper structure_item ->
        match structure_item.pstr_desc with
        (* heuristic: if we have multiple type declarations, mark them recursive *)
        | Pstr_type ((Recursive as rec_flag), type_declarations) ->
          let flag =
            match type_declarations with
            | [td] ->
              if looks_like_recursive_type_declaration td then
                Asttypes.Recursive
              else Asttypes.Nonrecursive
            | _ -> rec_flag
          in
          {
            structure_item with
            pstr_desc =
              Pstr_type
                ( flag,
                  List.map
                    (fun type_declaration ->
                      default_mapper.type_declaration mapper type_declaration)
                    type_declarations );
          }
        | _ -> default_mapper.structure_item mapper structure_item);
    signature_item =
      (fun mapper signature_item ->
        match signature_item.psig_desc with
        (* heuristic: if we have multiple type declarations, mark them recursive *)
        | Psig_type ((Recursive as rec_flag), type_declarations) ->
          let flag =
            match type_declarations with
            | [td] ->
              if looks_like_recursive_type_declaration td then
                Asttypes.Recursive
              else Asttypes.Nonrecursive
            | _ -> rec_flag
          in
          {
            signature_item with
            psig_desc =
              Psig_type
                ( flag,
                  List.map
                    (fun type_declaration ->
                      default_mapper.type_declaration mapper type_declaration)
                    type_declarations );
          }
        | _ -> default_mapper.signature_item mapper signature_item);
    value_binding =
      (fun mapper vb ->
        match vb with
        | {
         pvb_pat = {ppat_desc = Ppat_var _} as pat;
         pvb_expr =
           {pexp_loc = expr_loc; pexp_desc = Pexp_constraint (expr, typ)};
        }
          when expr_loc.loc_ghost ->
          (* let t: t = (expr : t) -> let t: t = expr *)
          let typ = default_mapper.typ mapper typ in
          let pat = default_mapper.pat mapper pat in
          let expr = mapper.expr mapper expr in
          let new_pattern =
            {
              Parsetree.ppat_loc =
                {pat.ppat_loc with loc_end = typ.ptyp_loc.loc_end};
              ppat_attributes = [];
              ppat_desc = Ppat_constraint (pat, typ);
            }
          in
          {
            vb with
            pvb_pat = new_pattern;
            pvb_expr = expr;
            pvb_attributes = default_mapper.attributes mapper vb.pvb_attributes;
          }
        | {
         pvb_pat =
           {ppat_desc = Ppat_constraint (pat, {ptyp_desc = Ptyp_poly ([], _)})};
         pvb_expr =
           {pexp_loc = expr_loc; pexp_desc = Pexp_constraint (expr, typ)};
        }
          when expr_loc.loc_ghost ->
          (* let t: . t = (expr : t) -> let t: t = expr *)
          let typ = default_mapper.typ mapper typ in
          let pat = default_mapper.pat mapper pat in
          let expr = mapper.expr mapper expr in
          let new_pattern =
            {
              Parsetree.ppat_loc =
                {pat.ppat_loc with loc_end = typ.ptyp_loc.loc_end};
              ppat_attributes = [];
              ppat_desc = Ppat_constraint (pat, typ);
            }
          in
          {
            vb with
            pvb_pat = new_pattern;
            pvb_expr = expr;
            pvb_attributes = default_mapper.attributes mapper vb.pvb_attributes;
          }
        | _ -> default_mapper.value_binding mapper vb);
  }

let structure s = normalize.Ast_mapper.structure normalize s
let signature s = normalize.Ast_mapper.signature normalize s

let replace_string_literal_structure string_data structure =
  let mapper = string_literal_mapper string_data in
  mapper.Ast_mapper.structure mapper structure

let replace_string_literal_signature string_data signature =
  let mapper = string_literal_mapper string_data in
  mapper.Ast_mapper.signature mapper signature
