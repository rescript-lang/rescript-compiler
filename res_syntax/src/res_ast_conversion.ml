let concatLongidents l1 l2 =
  let parts1 = Longident.flatten l1 in
  let parts2 = Longident.flatten l2 in
  match List.concat [parts1; parts2] |> Longident.unflatten with
  | Some longident -> longident
  | None -> l2

(* TODO: support nested open's ? *)
let rec rewritePpatOpen longidentOpen pat =
  match pat.Parsetree.ppat_desc with
  | Ppat_array (first :: rest) ->
    (* Color.[Red, Blue, Green] -> [Color.Red, Blue, Green] *)
    {
      pat with
      ppat_desc = Ppat_array (rewritePpatOpen longidentOpen first :: rest);
    }
  | Ppat_tuple (first :: rest) ->
    (* Color.(Red, Blue, Green) -> (Color.Red, Blue, Green) *)
    {
      pat with
      ppat_desc = Ppat_tuple (rewritePpatOpen longidentOpen first :: rest);
    }
  | Ppat_construct
      ( ({txt = Longident.Lident "::"} as listConstructor),
        Some ({ppat_desc = Ppat_tuple (pat :: rest)} as element) ) ->
    (* Color.(list[Red, Blue, Green]) -> list[Color.Red, Blue, Green] *)
    {
      pat with
      ppat_desc =
        Ppat_construct
          ( listConstructor,
            Some
              {
                element with
                ppat_desc =
                  Ppat_tuple (rewritePpatOpen longidentOpen pat :: rest);
              } );
    }
  | Ppat_construct (({txt = constructor} as longidentLoc), optPattern) ->
    (* Foo.(Bar(a)) -> Foo.Bar(a) *)
    {
      pat with
      ppat_desc =
        Ppat_construct
          ( {longidentLoc with txt = concatLongidents longidentOpen constructor},
            optPattern );
    }
  | Ppat_record ((({txt = lbl} as longidentLoc), firstPat) :: rest, flag) ->
    (* Foo.{x} -> {Foo.x: x} *)
    let firstRow =
      ({longidentLoc with txt = concatLongidents longidentOpen lbl}, firstPat)
    in
    {pat with ppat_desc = Ppat_record (firstRow :: rest, flag)}
  | Ppat_or (pat1, pat2) ->
    {
      pat with
      ppat_desc =
        Ppat_or
          ( rewritePpatOpen longidentOpen pat1,
            rewritePpatOpen longidentOpen pat2 );
    }
  | Ppat_constraint (pattern, typ) ->
    {
      pat with
      ppat_desc = Ppat_constraint (rewritePpatOpen longidentOpen pattern, typ);
    }
  | Ppat_type ({txt = constructor} as longidentLoc) ->
    {
      pat with
      ppat_desc =
        Ppat_type
          {longidentLoc with txt = concatLongidents longidentOpen constructor};
    }
  | Ppat_lazy p ->
    {pat with ppat_desc = Ppat_lazy (rewritePpatOpen longidentOpen p)}
  | Ppat_exception p ->
    {pat with ppat_desc = Ppat_exception (rewritePpatOpen longidentOpen p)}
  | _ -> pat

let rec rewriteReasonFastPipe expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_apply
      ( {
          pexp_desc =
            Pexp_apply
              ( ({pexp_desc = Pexp_ident {txt = Longident.Lident "|."}} as op),
                [(Asttypes.Nolabel, lhs); (Nolabel, rhs)] );
          pexp_attributes = subAttrs;
        },
        args ) ->
    let rhsLoc = {rhs.pexp_loc with loc_end = expr.pexp_loc.loc_end} in
    let newLhs =
      let expr = rewriteReasonFastPipe lhs in
      {expr with pexp_attributes = List.concat [lhs.pexp_attributes; subAttrs]}
    in
    let newRhs =
      {
        pexp_loc = rhsLoc;
        pexp_attributes = [];
        pexp_desc = Pexp_apply (rhs, args);
      }
    in
    let allArgs = (Asttypes.Nolabel, newLhs) :: [(Asttypes.Nolabel, newRhs)] in
    {expr with pexp_desc = Pexp_apply (op, allArgs)}
  | _ -> expr

let makeReasonArityMapper ~forPrinter =
  let open Ast_mapper in
  {
    default_mapper with
    expr =
      (fun mapper expr ->
        match expr with
        (* Don't mind this case, Reason doesn't handle this. *)
        (* | {pexp_desc = Pexp_variant (lbl, args); pexp_loc; pexp_attributes} -> *)
        (* let newArgs = match args with *)
        (* | (Some {pexp_desc = Pexp_tuple [{pexp_desc = Pexp_tuple _ } as sp]}) as args-> *)
        (* if forPrinter then args else Some sp *)
        (* | Some {pexp_desc = Pexp_tuple [sp]} -> Some sp *)
        (* | _ -> args *)
        (* in *)
        (* default_mapper.expr mapper {pexp_desc=Pexp_variant(lbl, newArgs); pexp_loc; pexp_attributes} *)
        | {pexp_desc = Pexp_construct (lid, args); pexp_loc; pexp_attributes} ->
          let newArgs =
            match args with
            | Some {pexp_desc = Pexp_tuple [({pexp_desc = Pexp_tuple _} as sp)]}
              as args ->
              if forPrinter then args else Some sp
            | Some {pexp_desc = Pexp_tuple [sp]} -> Some sp
            | _ -> args
          in
          default_mapper.expr mapper
            {
              pexp_desc = Pexp_construct (lid, newArgs);
              pexp_loc;
              pexp_attributes;
            }
        | expr -> default_mapper.expr mapper (rewriteReasonFastPipe expr));
    pat =
      (fun mapper pattern ->
        match pattern with
        (* Don't mind this case, Reason doesn't handle this. *)
        (* | {ppat_desc = Ppat_variant (lbl, args); ppat_loc; ppat_attributes} -> *)
        (* let newArgs = match args with *)
        (* | (Some {ppat_desc = Ppat_tuple [{ppat_desc = Ppat_tuple _} as sp]}) as args -> *)
        (* if forPrinter then args else Some sp *)
        (* | Some {ppat_desc = Ppat_tuple [sp]} -> Some sp *)
        (* | _ -> args *)
        (* in *)
        (* default_mapper.pat mapper {ppat_desc = Ppat_variant (lbl, newArgs); ppat_loc; ppat_attributes;} *)
        | {ppat_desc = Ppat_construct (lid, args); ppat_loc; ppat_attributes} ->
          let new_args =
            match args with
            | Some {ppat_desc = Ppat_tuple [({ppat_desc = Ppat_tuple _} as sp)]}
              as args ->
              if forPrinter then args else Some sp
            | Some {ppat_desc = Ppat_tuple [sp]} -> Some sp
            | _ -> args
          in
          default_mapper.pat mapper
            {
              ppat_desc = Ppat_construct (lid, new_args);
              ppat_loc;
              ppat_attributes;
            }
        | x -> default_mapper.pat mapper x);
  }

let escapeTemplateLiteral s =
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

let escapeStringContents s =
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

let looksLikeRecursiveTypeDeclaration typeDeclaration =
  let open Parsetree in
  let name = typeDeclaration.ptype_name.txt in
  let rec checkKind kind =
    match kind with
    | Ptype_abstract | Ptype_open -> false
    | Ptype_variant constructorDeclarations ->
      List.exists checkConstructorDeclaration constructorDeclarations
    | Ptype_record labelDeclarations ->
      List.exists checkLabelDeclaration labelDeclarations
  and checkConstructorDeclaration constrDecl =
    checkConstructorArguments constrDecl.pcd_args
    ||
    match constrDecl.pcd_res with
    | Some typexpr -> checkTypExpr typexpr
    | None -> false
  and checkLabelDeclaration labelDeclaration =
    checkTypExpr labelDeclaration.pld_type
  and checkConstructorArguments constrArg =
    match constrArg with
    | Pcstr_tuple types -> List.exists checkTypExpr types
    | Pcstr_record labelDeclarations ->
      List.exists checkLabelDeclaration labelDeclarations
  and checkTypExpr typ =
    match typ.ptyp_desc with
    | Ptyp_any -> false
    | Ptyp_var _ -> false
    | Ptyp_object (fields, _) -> List.exists checkObjectField fields
    | Ptyp_class _ -> false
    | Ptyp_package _ -> false
    | Ptyp_extension _ -> false
    | Ptyp_arrow (_lbl, typ1, typ2) -> checkTypExpr typ1 || checkTypExpr typ2
    | Ptyp_tuple types -> List.exists checkTypExpr types
    | Ptyp_constr ({txt = longident}, types) ->
      (match longident with
      | Lident ident -> ident = name
      | _ -> false)
      || List.exists checkTypExpr types
    | Ptyp_alias (typ, _) -> checkTypExpr typ
    | Ptyp_variant (rowFields, _, _) -> List.exists checkRowFields rowFields
    | Ptyp_poly (_, typ) -> checkTypExpr typ
  and checkObjectField field =
    match field with
    | Otag (_label, _attrs, typ) -> checkTypExpr typ
    | Oinherit typ -> checkTypExpr typ
  and checkRowFields rowField =
    match rowField with
    | Rtag (_, _, _, types) -> List.exists checkTypExpr types
    | Rinherit typexpr -> checkTypExpr typexpr
  and checkManifest manifest =
    match manifest with
    | Some typ -> checkTypExpr typ
    | None -> false
  in
  checkKind typeDeclaration.ptype_kind
  || checkManifest typeDeclaration.ptype_manifest

let filterReasonRawLiteral attrs =
  List.filter
    (fun attr ->
      match attr with
      | {Location.txt = "reason.raw_literal"}, _ -> false
      | _ -> true)
    attrs

let stringLiteralMapper stringData =
  let isSameLocation l1 l2 =
    let open Location in
    l1.loc_start.pos_cnum == l2.loc_start.pos_cnum
  in
  let remainingStringData = stringData in
  let open Ast_mapper in
  {
    default_mapper with
    expr =
      (fun mapper expr ->
        match expr.pexp_desc with
        | Pexp_constant (Pconst_string (_txt, None)) -> (
          match
            List.find_opt
              (fun (_stringData, stringLoc) ->
                isSameLocation stringLoc expr.pexp_loc)
              remainingStringData
          with
          | Some (stringData, _) ->
            let stringData =
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
                (String.sub [@doesNotRaise]) stringData 1
                  (String.length stringData - 2)
            in
            {
              expr with
              pexp_attributes = filterReasonRawLiteral expr.pexp_attributes;
              pexp_desc = Pexp_constant (Pconst_string (stringData, None));
            }
          | None -> default_mapper.expr mapper expr)
        | _ -> default_mapper.expr mapper expr);
  }

let hasUncurriedAttribute attrs =
  List.exists
    (fun attr ->
      match attr with
      | {Asttypes.txt = "bs"}, Parsetree.PStr [] -> true
      | _ -> false)
    attrs

let templateLiteralAttr = (Location.mknoloc "res.template", Parsetree.PStr [])

let normalize =
  let open Ast_mapper in
  {
    default_mapper with
    extension =
      (fun mapper ext ->
        match ext with
        | id, payload ->
          ( {id with txt = Res_printer.convertBsExtension id.txt},
            default_mapper.payload mapper payload ));
    attribute =
      (fun mapper attr ->
        match attr with
        | id, payload ->
          ( {id with txt = Res_printer.convertBsExternalAttribute id.txt},
            default_mapper.payload mapper payload ));
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
        | Ppat_open ({txt = longidentOpen}, pattern) ->
          let p = rewritePpatOpen longidentOpen pattern in
          default_mapper.pat mapper p
        | Ppat_constant (Pconst_string (txt, tag)) ->
          let newTag =
            match tag with
            (* transform {|abc|} into {js|abc|js}, because `template string` is interpreted as {js||js} *)
            | Some "" -> Some "js"
            | tag -> tag
          in
          let s = Parsetree.Pconst_string (escapeTemplateLiteral txt, newTag) in
          {
            p with
            ppat_attributes =
              templateLiteralAttr :: mapper.attributes mapper p.ppat_attributes;
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
          let raw = escapeStringContents txt in
          let s = Parsetree.Pconst_string (raw, None) in
          {expr with pexp_desc = Pexp_constant s}
        | Pexp_constant (Pconst_string (txt, tag)) ->
          let newTag =
            match tag with
            (* transform {|abc|} into {js|abc|js}, we want to preserve unicode by default *)
            | Some "" -> Some "js"
            | tag -> tag
          in
          let s = Parsetree.Pconst_string (escapeTemplateLiteral txt, newTag) in
          {
            expr with
            pexp_attributes =
              templateLiteralAttr
              :: mapper.attributes mapper expr.pexp_attributes;
            pexp_desc = Pexp_constant s;
          }
        | Pexp_apply
            ( callExpr,
              [
                ( Nolabel,
                  ({
                     pexp_desc =
                       Pexp_construct ({txt = Longident.Lident "()"}, None);
                     pexp_attributes = [];
                   } as unitExpr) );
              ] )
          when hasUncurriedAttribute expr.pexp_attributes ->
          {
            expr with
            pexp_attributes = mapper.attributes mapper expr.pexp_attributes;
            pexp_desc =
              Pexp_apply
                ( callExpr,
                  [
                    ( Nolabel,
                      {
                        unitExpr with
                        pexp_loc = {unitExpr.pexp_loc with loc_ghost = true};
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
                    pexp_loc = labelLoc;
                  } );
              ] ) ->
          let label = Location.mkloc txt labelLoc in
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
                  pc_rhs = thenExpr;
                };
                {
                  pc_lhs =
                    {
                      ppat_desc =
                        Ppat_construct ({txt = Longident.Lident "false"}, None);
                    };
                  pc_rhs = elseExpr;
                };
              ] ) ->
          let ternaryMarker =
            (Location.mknoloc "ns.ternary", Parsetree.PStr [])
          in
          {
            Parsetree.pexp_loc = expr.pexp_loc;
            pexp_desc =
              Pexp_ifthenelse
                ( mapper.expr mapper condition,
                  mapper.expr mapper thenExpr,
                  Some (mapper.expr mapper elseExpr) );
            pexp_attributes = ternaryMarker :: expr.pexp_attributes;
          }
        | _ -> default_mapper.expr mapper expr);
    structure_item =
      (fun mapper structureItem ->
        match structureItem.pstr_desc with
        (* heuristic: if we have multiple type declarations, mark them recursive *)
        | Pstr_type ((Recursive as recFlag), typeDeclarations) ->
          let flag =
            match typeDeclarations with
            | [td] ->
              if looksLikeRecursiveTypeDeclaration td then Asttypes.Recursive
              else Asttypes.Nonrecursive
            | _ -> recFlag
          in
          {
            structureItem with
            pstr_desc =
              Pstr_type
                ( flag,
                  List.map
                    (fun typeDeclaration ->
                      default_mapper.type_declaration mapper typeDeclaration)
                    typeDeclarations );
          }
        | _ -> default_mapper.structure_item mapper structureItem);
    signature_item =
      (fun mapper signatureItem ->
        match signatureItem.psig_desc with
        (* heuristic: if we have multiple type declarations, mark them recursive *)
        | Psig_type ((Recursive as recFlag), typeDeclarations) ->
          let flag =
            match typeDeclarations with
            | [td] ->
              if looksLikeRecursiveTypeDeclaration td then Asttypes.Recursive
              else Asttypes.Nonrecursive
            | _ -> recFlag
          in
          {
            signatureItem with
            psig_desc =
              Psig_type
                ( flag,
                  List.map
                    (fun typeDeclaration ->
                      default_mapper.type_declaration mapper typeDeclaration)
                    typeDeclarations );
          }
        | _ -> default_mapper.signature_item mapper signatureItem);
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
          let newPattern =
            {
              Parsetree.ppat_loc =
                {pat.ppat_loc with loc_end = typ.ptyp_loc.loc_end};
              ppat_attributes = [];
              ppat_desc = Ppat_constraint (pat, typ);
            }
          in
          {
            vb with
            pvb_pat = newPattern;
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
          let newPattern =
            {
              Parsetree.ppat_loc =
                {pat.ppat_loc with loc_end = typ.ptyp_loc.loc_end};
              ppat_attributes = [];
              ppat_desc = Ppat_constraint (pat, typ);
            }
          in
          {
            vb with
            pvb_pat = newPattern;
            pvb_expr = expr;
            pvb_attributes = default_mapper.attributes mapper vb.pvb_attributes;
          }
        | _ -> default_mapper.value_binding mapper vb);
  }

let normalizeReasonArityStructure ~forPrinter s =
  let mapper = makeReasonArityMapper ~forPrinter in
  mapper.Ast_mapper.structure mapper s

let normalizeReasonAritySignature ~forPrinter s =
  let mapper = makeReasonArityMapper ~forPrinter in
  mapper.Ast_mapper.signature mapper s

let structure s = normalize.Ast_mapper.structure normalize s
let signature s = normalize.Ast_mapper.signature normalize s

let replaceStringLiteralStructure stringData structure =
  let mapper = stringLiteralMapper stringData in
  mapper.Ast_mapper.structure mapper structure

let replaceStringLiteralSignature stringData signature =
  let mapper = stringLiteralMapper stringData in
  mapper.Ast_mapper.signature mapper signature
