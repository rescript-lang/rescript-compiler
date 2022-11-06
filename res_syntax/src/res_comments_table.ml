module Comment = Res_comment
module Doc = Res_doc
module ParsetreeViewer = Res_parsetree_viewer

type t = {
  leading: (Location.t, Comment.t list) Hashtbl.t;
  inside: (Location.t, Comment.t list) Hashtbl.t;
  trailing: (Location.t, Comment.t list) Hashtbl.t;
}

let make () =
  {
    leading = Hashtbl.create 100;
    inside = Hashtbl.create 100;
    trailing = Hashtbl.create 100;
  }

let copy tbl =
  {
    leading = Hashtbl.copy tbl.leading;
    inside = Hashtbl.copy tbl.inside;
    trailing = Hashtbl.copy tbl.trailing;
  }

let empty = make ()

let printEntries tbl =
  let open Location in
  Hashtbl.fold
    (fun (k : Location.t) (v : Comment.t list) acc ->
      let loc =
        Doc.concat
          [
            Doc.lbracket;
            Doc.text (string_of_int k.loc_start.pos_lnum);
            Doc.text ":";
            Doc.text
              (string_of_int (k.loc_start.pos_cnum - k.loc_start.pos_bol));
            Doc.text "-";
            Doc.text (string_of_int k.loc_end.pos_lnum);
            Doc.text ":";
            Doc.text (string_of_int (k.loc_end.pos_cnum - k.loc_end.pos_bol));
            Doc.rbracket;
          ]
      in
      let doc =
        Doc.breakableGroup ~forceBreak:true
          (Doc.concat
             [
               loc;
               Doc.indent
                 (Doc.concat
                    [
                      Doc.line;
                      Doc.join
                        ~sep:(Doc.concat [Doc.comma; Doc.line])
                        (List.map (fun c -> Doc.text (Comment.txt c)) v);
                    ]);
               Doc.line;
             ])
      in
      doc :: acc)
    tbl []

let log t =
  let leadingStuff = printEntries t.leading in
  let trailingStuff = printEntries t.trailing in
  let stuffInside = printEntries t.inside in
  Doc.breakableGroup ~forceBreak:true
    (Doc.concat
       [
         Doc.text "leading comments:";
         Doc.indent (Doc.concat [Doc.line; Doc.concat leadingStuff]);
         Doc.line;
         Doc.text "comments inside:";
         Doc.indent (Doc.concat [Doc.line; Doc.concat stuffInside]);
         Doc.line;
         Doc.text "trailing comments:";
         Doc.indent (Doc.concat [Doc.line; Doc.concat trailingStuff]);
         Doc.line;
       ])
  |> Doc.toString ~width:80 |> print_endline

let attach tbl loc comments =
  match comments with
  | [] -> ()
  | comments -> Hashtbl.replace tbl loc comments

let partitionByLoc comments loc =
  let rec loop (leading, inside, trailing) comments =
    let open Location in
    match comments with
    | comment :: rest ->
      let cmtLoc = Comment.loc comment in
      if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum then
        loop (comment :: leading, inside, trailing) rest
      else if cmtLoc.loc_start.pos_cnum >= loc.loc_end.pos_cnum then
        loop (leading, inside, comment :: trailing) rest
      else loop (leading, comment :: inside, trailing) rest
    | [] -> (List.rev leading, List.rev inside, List.rev trailing)
  in
  loop ([], [], []) comments

let partitionLeadingTrailing comments loc =
  let rec loop (leading, trailing) comments =
    let open Location in
    match comments with
    | comment :: rest ->
      let cmtLoc = Comment.loc comment in
      if cmtLoc.loc_end.pos_cnum <= loc.loc_start.pos_cnum then
        loop (comment :: leading, trailing) rest
      else loop (leading, comment :: trailing) rest
    | [] -> (List.rev leading, List.rev trailing)
  in
  loop ([], []) comments

let partitionByOnSameLine loc comments =
  let rec loop (onSameLine, onOtherLine) comments =
    let open Location in
    match comments with
    | [] -> (List.rev onSameLine, List.rev onOtherLine)
    | comment :: rest ->
      let cmtLoc = Comment.loc comment in
      if cmtLoc.loc_start.pos_lnum == loc.loc_end.pos_lnum then
        loop (comment :: onSameLine, onOtherLine) rest
      else loop (onSameLine, comment :: onOtherLine) rest
  in
  loop ([], []) comments

let partitionAdjacentTrailing loc1 comments =
  let open Location in
  let open Lexing in
  let rec loop ~prevEndPos afterLoc1 comments =
    match comments with
    | [] -> (List.rev afterLoc1, [])
    | comment :: rest as comments ->
      let cmtPrevEndPos = Comment.prevTokEndPos comment in
      if prevEndPos.Lexing.pos_cnum == cmtPrevEndPos.pos_cnum then
        let commentEnd = (Comment.loc comment).loc_end in
        loop ~prevEndPos:commentEnd (comment :: afterLoc1) rest
      else (List.rev afterLoc1, comments)
  in
  loop ~prevEndPos:loc1.loc_end [] comments

let rec collectListPatterns acc pattern =
  let open Parsetree in
  match pattern.ppat_desc with
  | Ppat_construct
      ({txt = Longident.Lident "::"}, Some {ppat_desc = Ppat_tuple [pat; rest]})
    ->
    collectListPatterns (pat :: acc) rest
  | Ppat_construct ({txt = Longident.Lident "[]"}, None) -> List.rev acc
  | _ -> List.rev (pattern :: acc)

let rec collectListExprs acc expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_construct
      ({txt = Longident.Lident "::"}, Some {pexp_desc = Pexp_tuple [expr; rest]})
    ->
    collectListExprs (expr :: acc) rest
  | Pexp_construct ({txt = Longident.Lident "[]"}, _) -> List.rev acc
  | _ -> List.rev (expr :: acc)

(* TODO: use ParsetreeViewer *)
let arrowType ct =
  let open Parsetree in
  let rec process attrsBefore acc typ =
    match typ with
    | {
     ptyp_desc = Ptyp_arrow ((Nolabel as lbl), typ1, typ2);
     ptyp_attributes = [];
    } ->
      let arg = ([], lbl, typ1) in
      process attrsBefore (arg :: acc) typ2
    | {
     ptyp_desc = Ptyp_arrow ((Nolabel as lbl), typ1, typ2);
     ptyp_attributes = [({txt = "bs"}, _)] as attrs;
    } ->
      let arg = (attrs, lbl, typ1) in
      process attrsBefore (arg :: acc) typ2
    | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = _attrs}
      as returnType ->
      let args = List.rev acc in
      (attrsBefore, args, returnType)
    | {
     ptyp_desc = Ptyp_arrow (((Labelled _ | Optional _) as lbl), typ1, typ2);
     ptyp_attributes = attrs;
    } ->
      let arg = (attrs, lbl, typ1) in
      process attrsBefore (arg :: acc) typ2
    | typ -> (attrsBefore, List.rev acc, typ)
  in
  match ct with
  | {ptyp_desc = Ptyp_arrow (Nolabel, _typ1, _typ2); ptyp_attributes = attrs} as
    typ ->
    process attrs [] {typ with ptyp_attributes = []}
  | typ -> process [] [] typ

(* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? *)
let modExprApply modExpr =
  let rec loop acc modExpr =
    match modExpr with
    | {Parsetree.pmod_desc = Pmod_apply (next, arg)} -> loop (arg :: acc) next
    | _ -> modExpr :: acc
  in
  loop [] modExpr

(* TODO: avoiding the dependency on ParsetreeViewer here, is this a good idea? *)
let modExprFunctor modExpr =
  let rec loop acc modExpr =
    match modExpr with
    | {
     Parsetree.pmod_desc = Pmod_functor (lbl, modType, returnModExpr);
     pmod_attributes = attrs;
    } ->
      let param = (attrs, lbl, modType) in
      loop (param :: acc) returnModExpr
    | returnModExpr -> (List.rev acc, returnModExpr)
  in
  loop [] modExpr

let functorType modtype =
  let rec process acc modtype =
    match modtype with
    | {
     Parsetree.pmty_desc = Pmty_functor (lbl, argType, returnType);
     pmty_attributes = attrs;
    } ->
      let arg = (attrs, lbl, argType) in
      process (arg :: acc) returnType
    | modType -> (List.rev acc, modType)
  in
  process [] modtype

let funExpr expr =
  let open Parsetree in
  (* Turns (type t, type u, type z) into "type t u z" *)
  let rec collectNewTypes acc returnExpr =
    match returnExpr with
    | {pexp_desc = Pexp_newtype (stringLoc, returnExpr); pexp_attributes = []}
      ->
      collectNewTypes (stringLoc :: acc) returnExpr
    | returnExpr ->
      let loc =
        match (acc, List.rev acc) with
        | _startLoc :: _, endLoc :: _ ->
          {endLoc.loc with loc_end = endLoc.loc.loc_end}
        | _ -> Location.none
      in
      let txt =
        List.fold_right
          (fun curr acc -> acc ^ " " ^ curr.Location.txt)
          acc "type"
      in
      (Location.mkloc txt loc, returnExpr)
  in
  (* For simplicity reason Pexp_newtype gets converted to a Nolabel parameter,
   * otherwise this function would need to return a variant:
   * | NormalParamater(...)
   * | NewType(...)
   * This complicates printing with an extra variant/boxing/allocation for a code-path
   * that is not often used. Lets just keep it simple for now *)
  let rec collect attrsBefore acc expr =
    match expr with
    | {
     pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr);
     pexp_attributes = [];
    } ->
      let parameter = ([], lbl, defaultExpr, pattern) in
      collect attrsBefore (parameter :: acc) returnExpr
    | {pexp_desc = Pexp_newtype (stringLoc, rest); pexp_attributes = attrs} ->
      let var, returnExpr = collectNewTypes [stringLoc] rest in
      let parameter =
        ( attrs,
          Asttypes.Nolabel,
          None,
          Ast_helper.Pat.var ~loc:stringLoc.loc var )
      in
      collect attrsBefore (parameter :: acc) returnExpr
    | {
     pexp_desc = Pexp_fun (lbl, defaultExpr, pattern, returnExpr);
     pexp_attributes = [({txt = "bs"}, _)] as attrs;
    } ->
      let parameter = (attrs, lbl, defaultExpr, pattern) in
      collect attrsBefore (parameter :: acc) returnExpr
    | {
     pexp_desc =
       Pexp_fun
         (((Labelled _ | Optional _) as lbl), defaultExpr, pattern, returnExpr);
     pexp_attributes = attrs;
    } ->
      let parameter = (attrs, lbl, defaultExpr, pattern) in
      collect attrsBefore (parameter :: acc) returnExpr
    | expr -> (attrsBefore, List.rev acc, expr)
  in
  match expr with
  | {
      pexp_desc = Pexp_fun (Nolabel, _defaultExpr, _pattern, _returnExpr);
      pexp_attributes = attrs;
    } as expr ->
    collect attrs [] {expr with pexp_attributes = []}
  | expr -> collect [] [] expr

let rec isBlockExpr expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_letmodule _ | Pexp_letexception _ | Pexp_let _ | Pexp_open _
  | Pexp_sequence _ ->
    true
  | Pexp_apply (callExpr, _) when isBlockExpr callExpr -> true
  | Pexp_constraint (expr, _) when isBlockExpr expr -> true
  | Pexp_field (expr, _) when isBlockExpr expr -> true
  | Pexp_setfield (expr, _, _) when isBlockExpr expr -> true
  | _ -> false

let isIfThenElseExpr expr =
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_ifthenelse _ -> true
  | _ -> false

type node =
  | Case of Parsetree.case
  | CoreType of Parsetree.core_type
  | ExprArgument of Parsetree.expression
  | Expression of Parsetree.expression
  | ExprRecordRow of Longident.t Asttypes.loc * Parsetree.expression
  | ExtensionConstructor of Parsetree.extension_constructor
  | LabelDeclaration of Parsetree.label_declaration
  | ModuleBinding of Parsetree.module_binding
  | ModuleDeclaration of Parsetree.module_declaration
  | ModuleExpr of Parsetree.module_expr
  | ObjectField of Parsetree.object_field
  | PackageConstraint of Longident.t Asttypes.loc * Parsetree.core_type
  | Pattern of Parsetree.pattern
  | PatternRecordRow of Longident.t Asttypes.loc * Parsetree.pattern
  | RowField of Parsetree.row_field
  | SignatureItem of Parsetree.signature_item
  | StructureItem of Parsetree.structure_item
  | TypeDeclaration of Parsetree.type_declaration
  | ValueBinding of Parsetree.value_binding

let getLoc node =
  let open Parsetree in
  match node with
  | Case case ->
    {case.pc_lhs.ppat_loc with loc_end = case.pc_rhs.pexp_loc.loc_end}
  | CoreType ct -> ct.ptyp_loc
  | ExprArgument expr -> (
    match expr.Parsetree.pexp_attributes with
    | ({Location.txt = "ns.namedArgLoc"; loc}, _) :: _attrs ->
      {loc with loc_end = expr.pexp_loc.loc_end}
    | _ -> expr.pexp_loc)
  | Expression e -> (
    match e.pexp_attributes with
    | ({txt = "ns.braces"; loc}, _) :: _ -> loc
    | _ -> e.pexp_loc)
  | ExprRecordRow (li, e) -> {li.loc with loc_end = e.pexp_loc.loc_end}
  | ExtensionConstructor ec -> ec.pext_loc
  | LabelDeclaration ld -> ld.pld_loc
  | ModuleBinding mb -> mb.pmb_loc
  | ModuleDeclaration md -> md.pmd_loc
  | ModuleExpr me -> me.pmod_loc
  | ObjectField field -> (
    match field with
    | Parsetree.Otag (lbl, _, typ) ->
      {lbl.loc with loc_end = typ.ptyp_loc.loc_end}
    | _ -> Location.none)
  | PackageConstraint (li, te) -> {li.loc with loc_end = te.ptyp_loc.loc_end}
  | Pattern p -> p.ppat_loc
  | PatternRecordRow (li, p) -> {li.loc with loc_end = p.ppat_loc.loc_end}
  | RowField rf -> (
    match rf with
    | Parsetree.Rtag ({loc}, _, _, _) -> loc
    | Rinherit {ptyp_loc} -> ptyp_loc)
  | SignatureItem si -> si.psig_loc
  | StructureItem si -> si.pstr_loc
  | TypeDeclaration td -> td.ptype_loc
  | ValueBinding vb -> vb.pvb_loc

let rec walkStructure s t comments =
  match s with
  | _ when comments = [] -> ()
  | [] -> attach t.inside Location.none comments
  | s -> walkList (s |> List.map (fun si -> StructureItem si)) t comments

and walkStructureItem si t comments =
  match si.Parsetree.pstr_desc with
  | _ when comments = [] -> ()
  | Pstr_primitive valueDescription ->
    walkValueDescription valueDescription t comments
  | Pstr_open openDescription -> walkOpenDescription openDescription t comments
  | Pstr_value (_, valueBindings) -> walkValueBindings valueBindings t comments
  | Pstr_type (_, typeDeclarations) ->
    walkTypeDeclarations typeDeclarations t comments
  | Pstr_eval (expr, _) -> walkExpression expr t comments
  | Pstr_module moduleBinding -> walkModuleBinding moduleBinding t comments
  | Pstr_recmodule moduleBindings ->
    walkList
      (moduleBindings |> List.map (fun mb -> ModuleBinding mb))
      t comments
  | Pstr_modtype modTypDecl -> walkModuleTypeDeclaration modTypDecl t comments
  | Pstr_attribute attribute -> walkAttribute attribute t comments
  | Pstr_extension (extension, _) -> walkExtension extension t comments
  | Pstr_include includeDeclaration ->
    walkIncludeDeclaration includeDeclaration t comments
  | Pstr_exception extensionConstructor ->
    walkExtensionConstructor extensionConstructor t comments
  | Pstr_typext typeExtension -> walkTypeExtension typeExtension t comments
  | Pstr_class_type _ | Pstr_class _ -> ()

and walkValueDescription vd t comments =
  let leading, trailing = partitionLeadingTrailing comments vd.pval_name.loc in
  attach t.leading vd.pval_name.loc leading;
  let afterName, rest = partitionAdjacentTrailing vd.pval_name.loc trailing in
  attach t.trailing vd.pval_name.loc afterName;
  let before, inside, after = partitionByLoc rest vd.pval_type.ptyp_loc in
  attach t.leading vd.pval_type.ptyp_loc before;
  walkCoreType vd.pval_type t inside;
  attach t.trailing vd.pval_type.ptyp_loc after

and walkTypeExtension te t comments =
  let leading, trailing =
    partitionLeadingTrailing comments te.ptyext_path.loc
  in
  attach t.leading te.ptyext_path.loc leading;
  let afterPath, rest = partitionAdjacentTrailing te.ptyext_path.loc trailing in
  attach t.trailing te.ptyext_path.loc afterPath;

  (* type params *)
  let rest =
    match te.ptyext_params with
    | [] -> rest
    | typeParams ->
      visitListButContinueWithRemainingComments
        ~getLoc:(fun (typexpr, _variance) -> typexpr.Parsetree.ptyp_loc)
        ~walkNode:walkTypeParam ~newlineDelimited:false typeParams t rest
  in
  walkList
    (te.ptyext_constructors |> List.map (fun ec -> ExtensionConstructor ec))
    t rest

and walkIncludeDeclaration inclDecl t comments =
  let before, inside, after =
    partitionByLoc comments inclDecl.pincl_mod.pmod_loc
  in
  attach t.leading inclDecl.pincl_mod.pmod_loc before;
  walkModuleExpr inclDecl.pincl_mod t inside;
  attach t.trailing inclDecl.pincl_mod.pmod_loc after

and walkModuleTypeDeclaration mtd t comments =
  let leading, trailing = partitionLeadingTrailing comments mtd.pmtd_name.loc in
  attach t.leading mtd.pmtd_name.loc leading;
  match mtd.pmtd_type with
  | None -> attach t.trailing mtd.pmtd_name.loc trailing
  | Some modType ->
    let afterName, rest =
      partitionAdjacentTrailing mtd.pmtd_name.loc trailing
    in
    attach t.trailing mtd.pmtd_name.loc afterName;
    let before, inside, after = partitionByLoc rest modType.pmty_loc in
    attach t.leading modType.pmty_loc before;
    walkModType modType t inside;
    attach t.trailing modType.pmty_loc after

and walkModuleBinding mb t comments =
  let leading, trailing = partitionLeadingTrailing comments mb.pmb_name.loc in
  attach t.leading mb.pmb_name.loc leading;
  let afterName, rest = partitionAdjacentTrailing mb.pmb_name.loc trailing in
  attach t.trailing mb.pmb_name.loc afterName;
  let leading, inside, trailing = partitionByLoc rest mb.pmb_expr.pmod_loc in
  (match mb.pmb_expr.pmod_desc with
  | Pmod_constraint _ ->
    walkModuleExpr mb.pmb_expr t (List.concat [leading; inside])
  | _ ->
    attach t.leading mb.pmb_expr.pmod_loc leading;
    walkModuleExpr mb.pmb_expr t inside);
  attach t.trailing mb.pmb_expr.pmod_loc trailing

and walkSignature signature t comments =
  match signature with
  | _ when comments = [] -> ()
  | [] -> attach t.inside Location.none comments
  | _s ->
    walkList (signature |> List.map (fun si -> SignatureItem si)) t comments

and walkSignatureItem (si : Parsetree.signature_item) t comments =
  match si.psig_desc with
  | _ when comments = [] -> ()
  | Psig_value valueDescription ->
    walkValueDescription valueDescription t comments
  | Psig_type (_, typeDeclarations) ->
    walkTypeDeclarations typeDeclarations t comments
  | Psig_typext typeExtension -> walkTypeExtension typeExtension t comments
  | Psig_exception extensionConstructor ->
    walkExtensionConstructor extensionConstructor t comments
  | Psig_module moduleDeclaration ->
    walkModuleDeclaration moduleDeclaration t comments
  | Psig_recmodule moduleDeclarations ->
    walkList
      (moduleDeclarations |> List.map (fun md -> ModuleDeclaration md))
      t comments
  | Psig_modtype moduleTypeDeclaration ->
    walkModuleTypeDeclaration moduleTypeDeclaration t comments
  | Psig_open openDescription -> walkOpenDescription openDescription t comments
  | Psig_include includeDescription ->
    walkIncludeDescription includeDescription t comments
  | Psig_attribute attribute -> walkAttribute attribute t comments
  | Psig_extension (extension, _) -> walkExtension extension t comments
  | Psig_class _ | Psig_class_type _ -> ()

and walkIncludeDescription id t comments =
  let before, inside, after = partitionByLoc comments id.pincl_mod.pmty_loc in
  attach t.leading id.pincl_mod.pmty_loc before;
  walkModType id.pincl_mod t inside;
  attach t.trailing id.pincl_mod.pmty_loc after

and walkModuleDeclaration md t comments =
  let leading, trailing = partitionLeadingTrailing comments md.pmd_name.loc in
  attach t.leading md.pmd_name.loc leading;
  let afterName, rest = partitionAdjacentTrailing md.pmd_name.loc trailing in
  attach t.trailing md.pmd_name.loc afterName;
  let leading, inside, trailing = partitionByLoc rest md.pmd_type.pmty_loc in
  attach t.leading md.pmd_type.pmty_loc leading;
  walkModType md.pmd_type t inside;
  attach t.trailing md.pmd_type.pmty_loc trailing

and walkNode node tbl comments =
  match node with
  | Case c -> walkCase c tbl comments
  | CoreType ct -> walkCoreType ct tbl comments
  | ExprArgument ea -> walkExprArgument ea tbl comments
  | Expression e -> walkExpression e tbl comments
  | ExprRecordRow (ri, e) -> walkExprRecordRow (ri, e) tbl comments
  | ExtensionConstructor ec -> walkExtensionConstructor ec tbl comments
  | LabelDeclaration ld -> walkLabelDeclaration ld tbl comments
  | ModuleBinding mb -> walkModuleBinding mb tbl comments
  | ModuleDeclaration md -> walkModuleDeclaration md tbl comments
  | ModuleExpr me -> walkModuleExpr me tbl comments
  | ObjectField f -> walkObjectField f tbl comments
  | PackageConstraint (li, te) -> walkPackageConstraint (li, te) tbl comments
  | Pattern p -> walkPattern p tbl comments
  | PatternRecordRow (li, p) -> walkPatternRecordRow (li, p) tbl comments
  | RowField rf -> walkRowField rf tbl comments
  | SignatureItem si -> walkSignatureItem si tbl comments
  | StructureItem si -> walkStructureItem si tbl comments
  | TypeDeclaration td -> walkTypeDeclaration td tbl comments
  | ValueBinding vb -> walkValueBinding vb tbl comments

and walkList : ?prevLoc:Location.t -> node list -> t -> Comment.t list -> unit =
 fun ?prevLoc l t comments ->
  match l with
  | _ when comments = [] -> ()
  | [] -> (
    match prevLoc with
    | Some loc -> attach t.trailing loc comments
    | None -> ())
  | node :: rest ->
    let currLoc = getLoc node in
    let leading, inside, trailing = partitionByLoc comments currLoc in
    (match prevLoc with
    | None ->
      (* first node, all leading comments attach here *)
      attach t.leading currLoc leading
    | Some prevLoc ->
      (* Same line *)
      if prevLoc.loc_end.pos_lnum == currLoc.loc_start.pos_lnum then (
        let afterPrev, beforeCurr = partitionAdjacentTrailing prevLoc leading in
        attach t.trailing prevLoc afterPrev;
        attach t.leading currLoc beforeCurr)
      else
        let onSameLineAsPrev, afterPrev =
          partitionByOnSameLine prevLoc leading
        in
        attach t.trailing prevLoc onSameLineAsPrev;
        let leading, _inside, _trailing = partitionByLoc afterPrev currLoc in
        attach t.leading currLoc leading);
    walkNode node t inside;
    walkList ~prevLoc:currLoc rest t trailing

(* The parsetree doesn't always contain location info about the opening or
 * closing token of a "list-of-things". This routine visits the whole list,
 * but returns any remaining comments that likely fall after the whole list. *)
and visitListButContinueWithRemainingComments :
      'node.
      ?prevLoc:Location.t ->
      newlineDelimited:bool ->
      getLoc:('node -> Location.t) ->
      walkNode:('node -> t -> Comment.t list -> unit) ->
      'node list ->
      t ->
      Comment.t list ->
      Comment.t list =
 fun ?prevLoc ~newlineDelimited ~getLoc ~walkNode l t comments ->
  let open Location in
  match l with
  | _ when comments = [] -> []
  | [] -> (
    match prevLoc with
    | Some loc ->
      let afterPrev, rest =
        if newlineDelimited then partitionByOnSameLine loc comments
        else partitionAdjacentTrailing loc comments
      in
      attach t.trailing loc afterPrev;
      rest
    | None -> comments)
  | node :: rest ->
    let currLoc = getLoc node in
    let leading, inside, trailing = partitionByLoc comments currLoc in
    let () =
      match prevLoc with
      | None ->
        (* first node, all leading comments attach here *)
        attach t.leading currLoc leading;
        ()
      | Some prevLoc ->
        (* Same line *)
        if prevLoc.loc_end.pos_lnum == currLoc.loc_start.pos_lnum then
          let afterPrev, beforeCurr =
            partitionAdjacentTrailing prevLoc leading
          in
          let () = attach t.trailing prevLoc afterPrev in
          let () = attach t.leading currLoc beforeCurr in
          ()
        else
          let onSameLineAsPrev, afterPrev =
            partitionByOnSameLine prevLoc leading
          in
          let () = attach t.trailing prevLoc onSameLineAsPrev in
          let leading, _inside, _trailing = partitionByLoc afterPrev currLoc in
          let () = attach t.leading currLoc leading in
          ()
    in
    walkNode node t inside;
    visitListButContinueWithRemainingComments ~prevLoc:currLoc ~getLoc ~walkNode
      ~newlineDelimited rest t trailing

and walkValueBindings vbs t comments =
  walkList (vbs |> List.map (fun vb -> ValueBinding vb)) t comments

and walkOpenDescription openDescription t comments =
  let loc = openDescription.popen_lid.loc in
  let leading, trailing = partitionLeadingTrailing comments loc in
  attach t.leading loc leading;
  attach t.trailing loc trailing

and walkTypeDeclarations typeDeclarations t comments =
  walkList
    (typeDeclarations |> List.map (fun td -> TypeDeclaration td))
    t comments

and walkTypeParam (typexpr, _variance) t comments =
  walkCoreType typexpr t comments

and walkTypeDeclaration (td : Parsetree.type_declaration) t comments =
  let beforeName, rest = partitionLeadingTrailing comments td.ptype_name.loc in
  attach t.leading td.ptype_name.loc beforeName;

  let afterName, rest = partitionAdjacentTrailing td.ptype_name.loc rest in
  attach t.trailing td.ptype_name.loc afterName;

  (* type params *)
  let rest =
    match td.ptype_params with
    | [] -> rest
    | typeParams ->
      visitListButContinueWithRemainingComments
        ~getLoc:(fun (typexpr, _variance) -> typexpr.Parsetree.ptyp_loc)
        ~walkNode:walkTypeParam ~newlineDelimited:false typeParams t rest
  in

  (* manifest:  = typexpr *)
  let rest =
    match td.ptype_manifest with
    | Some typexpr ->
      let beforeTyp, insideTyp, afterTyp =
        partitionByLoc rest typexpr.ptyp_loc
      in
      attach t.leading typexpr.ptyp_loc beforeTyp;
      walkCoreType typexpr t insideTyp;
      let afterTyp, rest =
        partitionAdjacentTrailing typexpr.Parsetree.ptyp_loc afterTyp
      in
      attach t.trailing typexpr.ptyp_loc afterTyp;
      rest
    | None -> rest
  in

  let rest =
    match td.ptype_kind with
    | Ptype_abstract | Ptype_open -> rest
    | Ptype_record labelDeclarations ->
      let () =
        if labelDeclarations = [] then attach t.inside td.ptype_loc rest
        else
          walkList
            (labelDeclarations |> List.map (fun ld -> LabelDeclaration ld))
            t rest
      in
      []
    | Ptype_variant constructorDeclarations ->
      walkConstructorDeclarations constructorDeclarations t rest
  in
  attach t.trailing td.ptype_loc rest

and walkLabelDeclarations lds t comments =
  visitListButContinueWithRemainingComments
    ~getLoc:(fun ld -> ld.Parsetree.pld_loc)
    ~walkNode:walkLabelDeclaration ~newlineDelimited:false lds t comments

and walkLabelDeclaration ld t comments =
  let beforeName, rest = partitionLeadingTrailing comments ld.pld_name.loc in
  attach t.leading ld.pld_name.loc beforeName;
  let afterName, rest = partitionAdjacentTrailing ld.pld_name.loc rest in
  attach t.trailing ld.pld_name.loc afterName;
  let beforeTyp, insideTyp, afterTyp =
    partitionByLoc rest ld.pld_type.ptyp_loc
  in
  attach t.leading ld.pld_type.ptyp_loc beforeTyp;
  walkCoreType ld.pld_type t insideTyp;
  attach t.trailing ld.pld_type.ptyp_loc afterTyp

and walkConstructorDeclarations cds t comments =
  visitListButContinueWithRemainingComments
    ~getLoc:(fun cd -> cd.Parsetree.pcd_loc)
    ~walkNode:walkConstructorDeclaration ~newlineDelimited:false cds t comments

and walkConstructorDeclaration cd t comments =
  let beforeName, rest = partitionLeadingTrailing comments cd.pcd_name.loc in
  attach t.leading cd.pcd_name.loc beforeName;
  let afterName, rest = partitionAdjacentTrailing cd.pcd_name.loc rest in
  attach t.trailing cd.pcd_name.loc afterName;
  let rest = walkConstructorArguments cd.pcd_args t rest in

  let rest =
    match cd.pcd_res with
    | Some typexpr ->
      let beforeTyp, insideTyp, afterTyp =
        partitionByLoc rest typexpr.ptyp_loc
      in
      attach t.leading typexpr.ptyp_loc beforeTyp;
      walkCoreType typexpr t insideTyp;
      let afterTyp, rest =
        partitionAdjacentTrailing typexpr.Parsetree.ptyp_loc afterTyp
      in
      attach t.trailing typexpr.ptyp_loc afterTyp;
      rest
    | None -> rest
  in
  attach t.trailing cd.pcd_loc rest

and walkConstructorArguments args t comments =
  match args with
  | Pcstr_tuple typexprs ->
    visitListButContinueWithRemainingComments
      ~getLoc:(fun n -> n.Parsetree.ptyp_loc)
      ~walkNode:walkCoreType ~newlineDelimited:false typexprs t comments
  | Pcstr_record labelDeclarations ->
    walkLabelDeclarations labelDeclarations t comments

and walkValueBinding vb t comments =
  let open Location in
  let vb =
    let open Parsetree in
    match (vb.pvb_pat, vb.pvb_expr) with
    | ( {ppat_desc = Ppat_constraint (pat, {ptyp_desc = Ptyp_poly ([], t)})},
        {pexp_desc = Pexp_constraint (expr, _typ)} ) ->
      {
        vb with
        pvb_pat =
          Ast_helper.Pat.constraint_
            ~loc:{pat.ppat_loc with loc_end = t.Parsetree.ptyp_loc.loc_end}
            pat t;
        pvb_expr = expr;
      }
    | ( {ppat_desc = Ppat_constraint (pat, {ptyp_desc = Ptyp_poly (_ :: _, t)})},
        {pexp_desc = Pexp_fun _} ) ->
      {
        vb with
        pvb_pat =
          {
            vb.pvb_pat with
            ppat_loc = {pat.ppat_loc with loc_end = t.ptyp_loc.loc_end};
          };
      }
    | ( ({
           ppat_desc =
             Ppat_constraint (pat, ({ptyp_desc = Ptyp_poly (_ :: _, t)} as typ));
         } as constrainedPattern),
        {pexp_desc = Pexp_newtype (_, {pexp_desc = Pexp_constraint (expr, _)})}
      ) ->
      (*
       * The location of the Ptyp_poly on the pattern is the whole thing.
       * let x:
       *   type t. (int, int) => int =
       *   (a, b) => {
       *     // comment
       *     a + b
       *   }
       *)
      {
        vb with
        pvb_pat =
          {
            constrainedPattern with
            ppat_desc = Ppat_constraint (pat, typ);
            ppat_loc =
              {constrainedPattern.ppat_loc with loc_end = t.ptyp_loc.loc_end};
          };
        pvb_expr = expr;
      }
    | _ -> vb
  in
  let patternLoc = vb.Parsetree.pvb_pat.ppat_loc in
  let exprLoc = vb.Parsetree.pvb_expr.pexp_loc in
  let expr = vb.pvb_expr in

  let leading, inside, trailing = partitionByLoc comments patternLoc in

  (* everything before start of pattern can only be leading on the pattern:
   *   let |* before *| a = 1 *)
  attach t.leading patternLoc leading;
  walkPattern vb.Parsetree.pvb_pat t inside;
  let afterPat, surroundingExpr =
    partitionAdjacentTrailing patternLoc trailing
  in
  attach t.trailing patternLoc afterPat;
  let beforeExpr, insideExpr, afterExpr =
    partitionByLoc surroundingExpr exprLoc
  in
  if isBlockExpr expr then
    walkExpression expr t (List.concat [beforeExpr; insideExpr; afterExpr])
  else (
    attach t.leading exprLoc beforeExpr;
    walkExpression expr t insideExpr;
    attach t.trailing exprLoc afterExpr)

and walkExpression expr t comments =
  let open Location in
  match expr.Parsetree.pexp_desc with
  | _ when comments = [] -> ()
  | Pexp_constant _ ->
    let leading, trailing = partitionLeadingTrailing comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    attach t.trailing expr.pexp_loc trailing
  | Pexp_ident longident ->
    let leading, trailing = partitionLeadingTrailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pexp_let
      ( _recFlag,
        valueBindings,
        {pexp_desc = Pexp_construct ({txt = Longident.Lident "()"}, None)} ) ->
    walkValueBindings valueBindings t comments
  | Pexp_let (_recFlag, valueBindings, expr2) ->
    let comments =
      visitListButContinueWithRemainingComments
        ~getLoc:(fun n ->
          if n.Parsetree.pvb_pat.ppat_loc.loc_ghost then n.pvb_expr.pexp_loc
          else n.Parsetree.pvb_loc)
        ~walkNode:walkValueBinding ~newlineDelimited:true valueBindings t
        comments
    in
    if isBlockExpr expr2 then walkExpression expr2 t comments
    else
      let leading, inside, trailing = partitionByLoc comments expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walkExpression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_sequence (expr1, expr2) ->
    let leading, inside, trailing = partitionByLoc comments expr1.pexp_loc in
    let comments =
      if isBlockExpr expr1 then (
        let afterExpr, comments =
          partitionByOnSameLine expr1.pexp_loc trailing
        in
        walkExpression expr1 t (List.concat [leading; inside; afterExpr]);
        comments)
      else (
        attach t.leading expr1.pexp_loc leading;
        walkExpression expr1 t inside;
        let afterExpr, comments =
          partitionByOnSameLine expr1.pexp_loc trailing
        in
        attach t.trailing expr1.pexp_loc afterExpr;
        comments)
    in
    if isBlockExpr expr2 then walkExpression expr2 t comments
    else
      let leading, inside, trailing = partitionByLoc comments expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walkExpression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_open (_override, longident, expr2) ->
    let leading, comments = partitionLeadingTrailing comments expr.pexp_loc in
    attach t.leading
      {expr.pexp_loc with loc_end = longident.loc.loc_end}
      leading;
    let leading, trailing = partitionLeadingTrailing comments longident.loc in
    attach t.leading longident.loc leading;
    let afterLongident, rest = partitionByOnSameLine longident.loc trailing in
    attach t.trailing longident.loc afterLongident;
    if isBlockExpr expr2 then walkExpression expr2 t rest
    else
      let leading, inside, trailing = partitionByLoc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walkExpression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_extension
      ( {txt = "bs.obj" | "obj"},
        PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_record (rows, _)}, [])}]
      ) ->
    walkList
      (rows |> List.map (fun (li, e) -> ExprRecordRow (li, e)))
      t comments
  | Pexp_extension extension -> walkExtension extension t comments
  | Pexp_letexception (extensionConstructor, expr2) ->
    let leading, comments = partitionLeadingTrailing comments expr.pexp_loc in
    attach t.leading
      {expr.pexp_loc with loc_end = extensionConstructor.pext_loc.loc_end}
      leading;
    let leading, inside, trailing =
      partitionByLoc comments extensionConstructor.pext_loc
    in
    attach t.leading extensionConstructor.pext_loc leading;
    walkExtensionConstructor extensionConstructor t inside;
    let afterExtConstr, rest =
      partitionByOnSameLine extensionConstructor.pext_loc trailing
    in
    attach t.trailing extensionConstructor.pext_loc afterExtConstr;
    if isBlockExpr expr2 then walkExpression expr2 t rest
    else
      let leading, inside, trailing = partitionByLoc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walkExpression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_letmodule (stringLoc, modExpr, expr2) ->
    let leading, comments = partitionLeadingTrailing comments expr.pexp_loc in
    attach t.leading
      {expr.pexp_loc with loc_end = modExpr.pmod_loc.loc_end}
      leading;
    let leading, trailing = partitionLeadingTrailing comments stringLoc.loc in
    attach t.leading stringLoc.loc leading;
    let afterString, rest = partitionAdjacentTrailing stringLoc.loc trailing in
    attach t.trailing stringLoc.loc afterString;
    let beforeModExpr, insideModExpr, afterModExpr =
      partitionByLoc rest modExpr.pmod_loc
    in
    attach t.leading modExpr.pmod_loc beforeModExpr;
    walkModuleExpr modExpr t insideModExpr;
    let afterModExpr, rest =
      partitionByOnSameLine modExpr.pmod_loc afterModExpr
    in
    attach t.trailing modExpr.pmod_loc afterModExpr;
    if isBlockExpr expr2 then walkExpression expr2 t rest
    else
      let leading, inside, trailing = partitionByLoc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walkExpression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_assert expr | Pexp_lazy expr ->
    if isBlockExpr expr then walkExpression expr t comments
    else
      let leading, inside, trailing = partitionByLoc comments expr.pexp_loc in
      attach t.leading expr.pexp_loc leading;
      walkExpression expr t inside;
      attach t.trailing expr.pexp_loc trailing
  | Pexp_coerce (expr, optTypexpr, typexpr) ->
    let leading, inside, trailing = partitionByLoc comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    walkExpression expr t inside;
    let afterExpr, rest = partitionAdjacentTrailing expr.pexp_loc trailing in
    attach t.trailing expr.pexp_loc afterExpr;
    let rest =
      match optTypexpr with
      | Some typexpr ->
        let leading, inside, trailing =
          partitionByLoc comments typexpr.ptyp_loc
        in
        attach t.leading typexpr.ptyp_loc leading;
        walkCoreType typexpr t inside;
        let afterTyp, rest =
          partitionAdjacentTrailing typexpr.ptyp_loc trailing
        in
        attach t.trailing typexpr.ptyp_loc afterTyp;
        rest
      | None -> rest
    in
    let leading, inside, trailing = partitionByLoc rest typexpr.ptyp_loc in
    attach t.leading typexpr.ptyp_loc leading;
    walkCoreType typexpr t inside;
    attach t.trailing typexpr.ptyp_loc trailing
  | Pexp_constraint (expr, typexpr) ->
    let leading, inside, trailing = partitionByLoc comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    walkExpression expr t inside;
    let afterExpr, rest = partitionAdjacentTrailing expr.pexp_loc trailing in
    attach t.trailing expr.pexp_loc afterExpr;
    let leading, inside, trailing = partitionByLoc rest typexpr.ptyp_loc in
    attach t.leading typexpr.ptyp_loc leading;
    walkCoreType typexpr t inside;
    attach t.trailing typexpr.ptyp_loc trailing
  | Pexp_tuple []
  | Pexp_array []
  | Pexp_construct ({txt = Longident.Lident "[]"}, _) ->
    attach t.inside expr.pexp_loc comments
  | Pexp_construct ({txt = Longident.Lident "::"}, _) ->
    walkList
      (collectListExprs [] expr |> List.map (fun e -> Expression e))
      t comments
  | Pexp_construct (longident, args) -> (
    let leading, trailing = partitionLeadingTrailing comments longident.loc in
    attach t.leading longident.loc leading;
    match args with
    | Some expr ->
      let afterLongident, rest =
        partitionAdjacentTrailing longident.loc trailing
      in
      attach t.trailing longident.loc afterLongident;
      walkExpression expr t rest
    | None -> attach t.trailing longident.loc trailing)
  | Pexp_variant (_label, None) -> ()
  | Pexp_variant (_label, Some expr) -> walkExpression expr t comments
  | Pexp_array exprs | Pexp_tuple exprs ->
    walkList (exprs |> List.map (fun e -> Expression e)) t comments
  | Pexp_record (rows, spreadExpr) ->
    if rows = [] then attach t.inside expr.pexp_loc comments
    else
      let comments =
        match spreadExpr with
        | None -> comments
        | Some expr ->
          let leading, inside, trailing =
            partitionByLoc comments expr.pexp_loc
          in
          attach t.leading expr.pexp_loc leading;
          walkExpression expr t inside;
          let afterExpr, rest =
            partitionAdjacentTrailing expr.pexp_loc trailing
          in
          attach t.trailing expr.pexp_loc afterExpr;
          rest
      in
      walkList
        (rows |> List.map (fun (li, e) -> ExprRecordRow (li, e)))
        t comments
  | Pexp_field (expr, longident) ->
    let leading, inside, trailing = partitionByLoc comments expr.pexp_loc in
    let trailing =
      if isBlockExpr expr then (
        let afterExpr, rest =
          partitionAdjacentTrailing expr.pexp_loc trailing
        in
        walkExpression expr t (List.concat [leading; inside; afterExpr]);
        rest)
      else (
        attach t.leading expr.pexp_loc leading;
        walkExpression expr t inside;
        trailing)
    in
    let afterExpr, rest = partitionAdjacentTrailing expr.pexp_loc trailing in
    attach t.trailing expr.pexp_loc afterExpr;
    let leading, trailing = partitionLeadingTrailing rest longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pexp_setfield (expr1, longident, expr2) ->
    let leading, inside, trailing = partitionByLoc comments expr1.pexp_loc in
    let rest =
      if isBlockExpr expr1 then (
        let afterExpr, rest =
          partitionAdjacentTrailing expr1.pexp_loc trailing
        in
        walkExpression expr1 t (List.concat [leading; inside; afterExpr]);
        rest)
      else
        let afterExpr, rest =
          partitionAdjacentTrailing expr1.pexp_loc trailing
        in
        attach t.leading expr1.pexp_loc leading;
        walkExpression expr1 t inside;
        attach t.trailing expr1.pexp_loc afterExpr;
        rest
    in
    let beforeLongident, afterLongident =
      partitionLeadingTrailing rest longident.loc
    in
    attach t.leading longident.loc beforeLongident;
    let afterLongident, rest =
      partitionAdjacentTrailing longident.loc afterLongident
    in
    attach t.trailing longident.loc afterLongident;
    if isBlockExpr expr2 then walkExpression expr2 t rest
    else
      let leading, inside, trailing = partitionByLoc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walkExpression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_ifthenelse (ifExpr, thenExpr, elseExpr) -> (
    let leading, rest = partitionLeadingTrailing comments expr.pexp_loc in
    attach t.leading expr.pexp_loc leading;
    let leading, inside, trailing = partitionByLoc rest ifExpr.pexp_loc in
    let comments =
      if isBlockExpr ifExpr then (
        let afterExpr, comments =
          partitionAdjacentTrailing ifExpr.pexp_loc trailing
        in
        walkExpression ifExpr t (List.concat [leading; inside; afterExpr]);
        comments)
      else (
        attach t.leading ifExpr.pexp_loc leading;
        walkExpression ifExpr t inside;
        let afterExpr, comments =
          partitionAdjacentTrailing ifExpr.pexp_loc trailing
        in
        attach t.trailing ifExpr.pexp_loc afterExpr;
        comments)
    in
    let leading, inside, trailing = partitionByLoc comments thenExpr.pexp_loc in
    let comments =
      if isBlockExpr thenExpr then (
        let afterExpr, trailing =
          partitionAdjacentTrailing thenExpr.pexp_loc trailing
        in
        walkExpression thenExpr t (List.concat [leading; inside; afterExpr]);
        trailing)
      else (
        attach t.leading thenExpr.pexp_loc leading;
        walkExpression thenExpr t inside;
        let afterExpr, comments =
          partitionAdjacentTrailing thenExpr.pexp_loc trailing
        in
        attach t.trailing thenExpr.pexp_loc afterExpr;
        comments)
    in
    match elseExpr with
    | None -> ()
    | Some expr ->
      if isBlockExpr expr || isIfThenElseExpr expr then
        walkExpression expr t comments
      else
        let leading, inside, trailing = partitionByLoc comments expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        walkExpression expr t inside;
        attach t.trailing expr.pexp_loc trailing)
  | Pexp_while (expr1, expr2) ->
    let leading, inside, trailing = partitionByLoc comments expr1.pexp_loc in
    let rest =
      if isBlockExpr expr1 then (
        let afterExpr, rest =
          partitionAdjacentTrailing expr1.pexp_loc trailing
        in
        walkExpression expr1 t (List.concat [leading; inside; afterExpr]);
        rest)
      else (
        attach t.leading expr1.pexp_loc leading;
        walkExpression expr1 t inside;
        let afterExpr, rest =
          partitionAdjacentTrailing expr1.pexp_loc trailing
        in
        attach t.trailing expr1.pexp_loc afterExpr;
        rest)
    in
    if isBlockExpr expr2 then walkExpression expr2 t rest
    else
      let leading, inside, trailing = partitionByLoc rest expr2.pexp_loc in
      attach t.leading expr2.pexp_loc leading;
      walkExpression expr2 t inside;
      attach t.trailing expr2.pexp_loc trailing
  | Pexp_for (pat, expr1, expr2, _, expr3) ->
    let leading, inside, trailing = partitionByLoc comments pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    walkPattern pat t inside;
    let afterPat, rest = partitionAdjacentTrailing pat.ppat_loc trailing in
    attach t.trailing pat.ppat_loc afterPat;
    let leading, inside, trailing = partitionByLoc rest expr1.pexp_loc in
    attach t.leading expr1.pexp_loc leading;
    walkExpression expr1 t inside;
    let afterExpr, rest = partitionAdjacentTrailing expr1.pexp_loc trailing in
    attach t.trailing expr1.pexp_loc afterExpr;
    let leading, inside, trailing = partitionByLoc rest expr2.pexp_loc in
    attach t.leading expr2.pexp_loc leading;
    walkExpression expr2 t inside;
    let afterExpr, rest = partitionAdjacentTrailing expr2.pexp_loc trailing in
    attach t.trailing expr2.pexp_loc afterExpr;
    if isBlockExpr expr3 then walkExpression expr3 t rest
    else
      let leading, inside, trailing = partitionByLoc rest expr3.pexp_loc in
      attach t.leading expr3.pexp_loc leading;
      walkExpression expr3 t inside;
      attach t.trailing expr3.pexp_loc trailing
  | Pexp_pack modExpr ->
    let before, inside, after = partitionByLoc comments modExpr.pmod_loc in
    attach t.leading modExpr.pmod_loc before;
    walkModuleExpr modExpr t inside;
    attach t.trailing modExpr.pmod_loc after
  | Pexp_match (expr1, [case; elseBranch])
    when Res_parsetree_viewer.hasIfLetAttribute expr.pexp_attributes ->
    let before, inside, after = partitionByLoc comments case.pc_lhs.ppat_loc in
    attach t.leading case.pc_lhs.ppat_loc before;
    walkPattern case.pc_lhs t inside;
    let afterPat, rest = partitionAdjacentTrailing case.pc_lhs.ppat_loc after in
    attach t.trailing case.pc_lhs.ppat_loc afterPat;
    let before, inside, after = partitionByLoc rest expr1.pexp_loc in
    attach t.leading expr1.pexp_loc before;
    walkExpression expr1 t inside;
    let afterExpr, rest = partitionAdjacentTrailing expr1.pexp_loc after in
    attach t.trailing expr1.pexp_loc afterExpr;
    let before, inside, after = partitionByLoc rest case.pc_rhs.pexp_loc in
    let after =
      if isBlockExpr case.pc_rhs then (
        let afterExpr, rest =
          partitionAdjacentTrailing case.pc_rhs.pexp_loc after
        in
        walkExpression case.pc_rhs t (List.concat [before; inside; afterExpr]);
        rest)
      else (
        attach t.leading case.pc_rhs.pexp_loc before;
        walkExpression case.pc_rhs t inside;
        after)
    in
    let afterExpr, rest =
      partitionAdjacentTrailing case.pc_rhs.pexp_loc after
    in
    attach t.trailing case.pc_rhs.pexp_loc afterExpr;
    let before, inside, after =
      partitionByLoc rest elseBranch.pc_rhs.pexp_loc
    in
    let after =
      if isBlockExpr elseBranch.pc_rhs then (
        let afterExpr, rest =
          partitionAdjacentTrailing elseBranch.pc_rhs.pexp_loc after
        in
        walkExpression elseBranch.pc_rhs t
          (List.concat [before; inside; afterExpr]);
        rest)
      else (
        attach t.leading elseBranch.pc_rhs.pexp_loc before;
        walkExpression elseBranch.pc_rhs t inside;
        after)
    in
    attach t.trailing elseBranch.pc_rhs.pexp_loc after
  | Pexp_match (expr, cases) | Pexp_try (expr, cases) ->
    let before, inside, after = partitionByLoc comments expr.pexp_loc in
    let after =
      if isBlockExpr expr then (
        let afterExpr, rest = partitionAdjacentTrailing expr.pexp_loc after in
        walkExpression expr t (List.concat [before; inside; afterExpr]);
        rest)
      else (
        attach t.leading expr.pexp_loc before;
        walkExpression expr t inside;
        after)
    in
    let afterExpr, rest = partitionAdjacentTrailing expr.pexp_loc after in
    attach t.trailing expr.pexp_loc afterExpr;
    walkList (cases |> List.map (fun case -> Case case)) t rest
    (* unary expression: todo use parsetreeviewer *)
  | Pexp_apply
      ( {
          pexp_desc =
            Pexp_ident
              {
                txt =
                  Longident.Lident ("~+" | "~+." | "~-" | "~-." | "not" | "!");
              };
        },
        [(Nolabel, argExpr)] ) ->
    let before, inside, after = partitionByLoc comments argExpr.pexp_loc in
    attach t.leading argExpr.pexp_loc before;
    walkExpression argExpr t inside;
    attach t.trailing argExpr.pexp_loc after
  (* binary expression *)
  | Pexp_apply
      ( {
          pexp_desc =
            Pexp_ident
              {
                txt =
                  Longident.Lident
                    ( ":=" | "||" | "&&" | "=" | "==" | "<" | ">" | "!=" | "!=="
                    | "<=" | ">=" | "|>" | "+" | "+." | "-" | "-." | "++" | "^"
                    | "*" | "*." | "/" | "/." | "**" | "|." | "<>" );
              };
        },
        [(Nolabel, operand1); (Nolabel, operand2)] ) ->
    let before, inside, after = partitionByLoc comments operand1.pexp_loc in
    attach t.leading operand1.pexp_loc before;
    walkExpression operand1 t inside;
    let afterOperand1, rest =
      partitionAdjacentTrailing operand1.pexp_loc after
    in
    attach t.trailing operand1.pexp_loc afterOperand1;
    let before, inside, after = partitionByLoc rest operand2.pexp_loc in
    attach t.leading operand2.pexp_loc before;
    walkExpression operand2 t inside;
    (* (List.concat [inside; after]); *)
    attach t.trailing operand2.pexp_loc after
  | Pexp_apply (callExpr, arguments) ->
    let before, inside, after = partitionByLoc comments callExpr.pexp_loc in
    let after =
      if isBlockExpr callExpr then (
        let afterExpr, rest =
          partitionAdjacentTrailing callExpr.pexp_loc after
        in
        walkExpression callExpr t (List.concat [before; inside; afterExpr]);
        rest)
      else (
        attach t.leading callExpr.pexp_loc before;
        walkExpression callExpr t inside;
        after)
    in
    if ParsetreeViewer.isJsxExpression expr then (
      let props =
        arguments
        |> List.filter (fun (label, _) ->
               match label with
               | Asttypes.Labelled "children" -> false
               | Asttypes.Nolabel -> false
               | _ -> true)
      in
      let maybeChildren =
        arguments
        |> List.find_opt (fun (label, _) ->
               label = Asttypes.Labelled "children")
      in
      match maybeChildren with
      (* There is no need to deal with this situation as the children cannot be NONE *)
      | None -> ()
      | Some (_, children) ->
        let leading, inside, _ = partitionByLoc after children.pexp_loc in
        if props = [] then
          (* All comments inside a tag are trailing comments of the tag if there are no props
             <A
             // comment
             // comment
             />
          *)
          let afterExpr, _ =
            partitionAdjacentTrailing callExpr.pexp_loc after
          in
          attach t.trailing callExpr.pexp_loc afterExpr
        else
          walkList (props |> List.map (fun (_, e) -> ExprArgument e)) t leading;
        walkExpression children t inside)
    else
      let afterExpr, rest = partitionAdjacentTrailing callExpr.pexp_loc after in
      attach t.trailing callExpr.pexp_loc afterExpr;
      walkList (arguments |> List.map (fun (_, e) -> ExprArgument e)) t rest
  | Pexp_fun (_, _, _, _) | Pexp_newtype _ -> (
    let _, parameters, returnExpr = funExpr expr in
    let comments =
      visitListButContinueWithRemainingComments ~newlineDelimited:false
        ~walkNode:walkExprPararameter
        ~getLoc:(fun (_attrs, _argLbl, exprOpt, pattern) ->
          let open Parsetree in
          let startPos =
            match pattern.ppat_attributes with
            | ({Location.txt = "ns.namedArgLoc"; loc}, _) :: _attrs ->
              loc.loc_start
            | _ -> pattern.ppat_loc.loc_start
          in
          match exprOpt with
          | None -> {pattern.ppat_loc with loc_start = startPos}
          | Some expr ->
            {
              pattern.ppat_loc with
              loc_start = startPos;
              loc_end = expr.pexp_loc.loc_end;
            })
        parameters t comments
    in
    match returnExpr.pexp_desc with
    | Pexp_constraint (expr, typ)
      when expr.pexp_loc.loc_start.pos_cnum >= typ.ptyp_loc.loc_end.pos_cnum ->
      let leading, inside, trailing = partitionByLoc comments typ.ptyp_loc in
      attach t.leading typ.ptyp_loc leading;
      walkCoreType typ t inside;
      let afterTyp, comments =
        partitionAdjacentTrailing typ.ptyp_loc trailing
      in
      attach t.trailing typ.ptyp_loc afterTyp;
      if isBlockExpr expr then walkExpression expr t comments
      else
        let leading, inside, trailing = partitionByLoc comments expr.pexp_loc in
        attach t.leading expr.pexp_loc leading;
        walkExpression expr t inside;
        attach t.trailing expr.pexp_loc trailing
    | _ ->
      if isBlockExpr returnExpr then walkExpression returnExpr t comments
      else
        let leading, inside, trailing =
          partitionByLoc comments returnExpr.pexp_loc
        in
        attach t.leading returnExpr.pexp_loc leading;
        walkExpression returnExpr t inside;
        attach t.trailing returnExpr.pexp_loc trailing)
  | _ -> ()

and walkExprPararameter (_attrs, _argLbl, exprOpt, pattern) t comments =
  let leading, inside, trailing = partitionByLoc comments pattern.ppat_loc in
  attach t.leading pattern.ppat_loc leading;
  walkPattern pattern t inside;
  match exprOpt with
  | Some expr ->
    let _afterPat, rest = partitionAdjacentTrailing pattern.ppat_loc trailing in
    attach t.trailing pattern.ppat_loc trailing;
    if isBlockExpr expr then walkExpression expr t rest
    else
      let leading, inside, trailing = partitionByLoc rest expr.pexp_loc in
      attach t.leading expr.pexp_loc leading;
      walkExpression expr t inside;
      attach t.trailing expr.pexp_loc trailing
  | None -> attach t.trailing pattern.ppat_loc trailing

and walkExprArgument expr t comments =
  match expr.Parsetree.pexp_attributes with
  | ({Location.txt = "ns.namedArgLoc"; loc}, _) :: _attrs ->
    let leading, trailing = partitionLeadingTrailing comments loc in
    attach t.leading loc leading;
    let afterLabel, rest = partitionAdjacentTrailing loc trailing in
    attach t.trailing loc afterLabel;
    let before, inside, after = partitionByLoc rest expr.pexp_loc in
    attach t.leading expr.pexp_loc before;
    walkExpression expr t inside;
    attach t.trailing expr.pexp_loc after
  | _ ->
    let before, inside, after = partitionByLoc comments expr.pexp_loc in
    attach t.leading expr.pexp_loc before;
    walkExpression expr t inside;
    attach t.trailing expr.pexp_loc after

and walkCase (case : Parsetree.case) t comments =
  let before, inside, after = partitionByLoc comments case.pc_lhs.ppat_loc in
  (* cases don't have a location on their own, leading comments should go
   * after the bar on the pattern *)
  walkPattern case.pc_lhs t (List.concat [before; inside]);
  let afterPat, rest = partitionAdjacentTrailing case.pc_lhs.ppat_loc after in
  attach t.trailing case.pc_lhs.ppat_loc afterPat;
  let comments =
    match case.pc_guard with
    | Some expr ->
      let before, inside, after = partitionByLoc rest expr.pexp_loc in
      let afterExpr, rest = partitionAdjacentTrailing expr.pexp_loc after in
      if isBlockExpr expr then
        walkExpression expr t (List.concat [before; inside; afterExpr])
      else (
        attach t.leading expr.pexp_loc before;
        walkExpression expr t inside;
        attach t.trailing expr.pexp_loc afterExpr);
      rest
    | None -> rest
  in
  if isBlockExpr case.pc_rhs then walkExpression case.pc_rhs t comments
  else
    let before, inside, after = partitionByLoc comments case.pc_rhs.pexp_loc in
    attach t.leading case.pc_rhs.pexp_loc before;
    walkExpression case.pc_rhs t inside;
    attach t.trailing case.pc_rhs.pexp_loc after

and walkExprRecordRow (longident, expr) t comments =
  let beforeLongident, afterLongident =
    partitionLeadingTrailing comments longident.loc
  in
  attach t.leading longident.loc beforeLongident;
  let afterLongident, rest =
    partitionAdjacentTrailing longident.loc afterLongident
  in
  attach t.trailing longident.loc afterLongident;
  let leading, inside, trailing = partitionByLoc rest expr.pexp_loc in
  attach t.leading expr.pexp_loc leading;
  walkExpression expr t inside;
  attach t.trailing expr.pexp_loc trailing

and walkExtensionConstructor extConstr t comments =
  let leading, trailing =
    partitionLeadingTrailing comments extConstr.pext_name.loc
  in
  attach t.leading extConstr.pext_name.loc leading;
  let afterName, rest =
    partitionAdjacentTrailing extConstr.pext_name.loc trailing
  in
  attach t.trailing extConstr.pext_name.loc afterName;
  walkExtensionConstructorKind extConstr.pext_kind t rest

and walkExtensionConstructorKind kind t comments =
  match kind with
  | Pext_rebind longident ->
    let leading, trailing = partitionLeadingTrailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pext_decl (constructorArguments, maybeTypExpr) -> (
    let rest = walkConstructorArguments constructorArguments t comments in
    match maybeTypExpr with
    | None -> ()
    | Some typexpr ->
      let before, inside, after = partitionByLoc rest typexpr.ptyp_loc in
      attach t.leading typexpr.ptyp_loc before;
      walkCoreType typexpr t inside;
      attach t.trailing typexpr.ptyp_loc after)

and walkModuleExpr modExpr t comments =
  match modExpr.pmod_desc with
  | Pmod_ident longident ->
    let before, after = partitionLeadingTrailing comments longident.loc in
    attach t.leading longident.loc before;
    attach t.trailing longident.loc after
  | Pmod_structure [] -> attach t.inside modExpr.pmod_loc comments
  | Pmod_structure structure -> walkStructure structure t comments
  | Pmod_extension extension -> walkExtension extension t comments
  | Pmod_unpack expr ->
    let before, inside, after = partitionByLoc comments expr.pexp_loc in
    attach t.leading expr.pexp_loc before;
    walkExpression expr t inside;
    attach t.trailing expr.pexp_loc after
  | Pmod_constraint (modexpr, modtype) ->
    if modtype.pmty_loc.loc_start >= modexpr.pmod_loc.loc_end then (
      let before, inside, after = partitionByLoc comments modexpr.pmod_loc in
      attach t.leading modexpr.pmod_loc before;
      walkModuleExpr modexpr t inside;
      let after, rest = partitionAdjacentTrailing modexpr.pmod_loc after in
      attach t.trailing modexpr.pmod_loc after;
      let before, inside, after = partitionByLoc rest modtype.pmty_loc in
      attach t.leading modtype.pmty_loc before;
      walkModType modtype t inside;
      attach t.trailing modtype.pmty_loc after)
    else
      let before, inside, after = partitionByLoc comments modtype.pmty_loc in
      attach t.leading modtype.pmty_loc before;
      walkModType modtype t inside;
      let after, rest = partitionAdjacentTrailing modtype.pmty_loc after in
      attach t.trailing modtype.pmty_loc after;
      let before, inside, after = partitionByLoc rest modexpr.pmod_loc in
      attach t.leading modexpr.pmod_loc before;
      walkModuleExpr modexpr t inside;
      attach t.trailing modexpr.pmod_loc after
  | Pmod_apply (_callModExpr, _argModExpr) ->
    let modExprs = modExprApply modExpr in
    walkList (modExprs |> List.map (fun me -> ModuleExpr me)) t comments
  | Pmod_functor _ -> (
    let parameters, returnModExpr = modExprFunctor modExpr in
    let comments =
      visitListButContinueWithRemainingComments
        ~getLoc:(fun (_, lbl, modTypeOption) ->
          match modTypeOption with
          | None -> lbl.Asttypes.loc
          | Some modType ->
            {lbl.loc with loc_end = modType.Parsetree.pmty_loc.loc_end})
        ~walkNode:walkModExprParameter ~newlineDelimited:false parameters t
        comments
    in
    match returnModExpr.pmod_desc with
    | Pmod_constraint (modExpr, modType)
      when modType.pmty_loc.loc_end.pos_cnum
           <= modExpr.pmod_loc.loc_start.pos_cnum ->
      let before, inside, after = partitionByLoc comments modType.pmty_loc in
      attach t.leading modType.pmty_loc before;
      walkModType modType t inside;
      let after, rest = partitionAdjacentTrailing modType.pmty_loc after in
      attach t.trailing modType.pmty_loc after;
      let before, inside, after = partitionByLoc rest modExpr.pmod_loc in
      attach t.leading modExpr.pmod_loc before;
      walkModuleExpr modExpr t inside;
      attach t.trailing modExpr.pmod_loc after
    | _ ->
      let before, inside, after =
        partitionByLoc comments returnModExpr.pmod_loc
      in
      attach t.leading returnModExpr.pmod_loc before;
      walkModuleExpr returnModExpr t inside;
      attach t.trailing returnModExpr.pmod_loc after)

and walkModExprParameter parameter t comments =
  let _attrs, lbl, modTypeOption = parameter in
  let leading, trailing = partitionLeadingTrailing comments lbl.loc in
  attach t.leading lbl.loc leading;
  match modTypeOption with
  | None -> attach t.trailing lbl.loc trailing
  | Some modType ->
    let afterLbl, rest = partitionAdjacentTrailing lbl.loc trailing in
    attach t.trailing lbl.loc afterLbl;
    let before, inside, after = partitionByLoc rest modType.pmty_loc in
    attach t.leading modType.pmty_loc before;
    walkModType modType t inside;
    attach t.trailing modType.pmty_loc after

and walkModType modType t comments =
  match modType.pmty_desc with
  | Pmty_ident longident | Pmty_alias longident ->
    let leading, trailing = partitionLeadingTrailing comments longident.loc in
    attach t.leading longident.loc leading;
    attach t.trailing longident.loc trailing
  | Pmty_signature [] -> attach t.inside modType.pmty_loc comments
  | Pmty_signature signature -> walkSignature signature t comments
  | Pmty_extension extension -> walkExtension extension t comments
  | Pmty_typeof modExpr ->
    let before, inside, after = partitionByLoc comments modExpr.pmod_loc in
    attach t.leading modExpr.pmod_loc before;
    walkModuleExpr modExpr t inside;
    attach t.trailing modExpr.pmod_loc after
  | Pmty_with (modType, _withConstraints) ->
    let before, inside, after = partitionByLoc comments modType.pmty_loc in
    attach t.leading modType.pmty_loc before;
    walkModType modType t inside;
    attach t.trailing modType.pmty_loc after
    (* TODO: withConstraints*)
  | Pmty_functor _ ->
    let parameters, returnModType = functorType modType in
    let comments =
      visitListButContinueWithRemainingComments
        ~getLoc:(fun (_, lbl, modTypeOption) ->
          match modTypeOption with
          | None -> lbl.Asttypes.loc
          | Some modType ->
            if lbl.txt = "_" then modType.Parsetree.pmty_loc
            else {lbl.loc with loc_end = modType.Parsetree.pmty_loc.loc_end})
        ~walkNode:walkModTypeParameter ~newlineDelimited:false parameters t
        comments
    in
    let before, inside, after =
      partitionByLoc comments returnModType.pmty_loc
    in
    attach t.leading returnModType.pmty_loc before;
    walkModType returnModType t inside;
    attach t.trailing returnModType.pmty_loc after

and walkModTypeParameter (_, lbl, modTypeOption) t comments =
  let leading, trailing = partitionLeadingTrailing comments lbl.loc in
  attach t.leading lbl.loc leading;
  match modTypeOption with
  | None -> attach t.trailing lbl.loc trailing
  | Some modType ->
    let afterLbl, rest = partitionAdjacentTrailing lbl.loc trailing in
    attach t.trailing lbl.loc afterLbl;
    let before, inside, after = partitionByLoc rest modType.pmty_loc in
    attach t.leading modType.pmty_loc before;
    walkModType modType t inside;
    attach t.trailing modType.pmty_loc after

and walkPattern pat t comments =
  let open Location in
  match pat.Parsetree.ppat_desc with
  | _ when comments = [] -> ()
  | Ppat_alias (pat, alias) ->
    let leading, inside, trailing = partitionByLoc comments pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    walkPattern pat t inside;
    let afterPat, rest = partitionAdjacentTrailing pat.ppat_loc trailing in
    attach t.leading pat.ppat_loc leading;
    attach t.trailing pat.ppat_loc afterPat;
    let beforeAlias, afterAlias = partitionLeadingTrailing rest alias.loc in
    attach t.leading alias.loc beforeAlias;
    attach t.trailing alias.loc afterAlias
  | Ppat_tuple []
  | Ppat_array []
  | Ppat_construct ({txt = Longident.Lident "()"}, _)
  | Ppat_construct ({txt = Longident.Lident "[]"}, _) ->
    attach t.inside pat.ppat_loc comments
  | Ppat_array patterns ->
    walkList (patterns |> List.map (fun p -> Pattern p)) t comments
  | Ppat_tuple patterns ->
    walkList (patterns |> List.map (fun p -> Pattern p)) t comments
  | Ppat_construct ({txt = Longident.Lident "::"}, _) ->
    walkList
      (collectListPatterns [] pat |> List.map (fun p -> Pattern p))
      t comments
  | Ppat_construct (constr, None) ->
    let beforeConstr, afterConstr =
      partitionLeadingTrailing comments constr.loc
    in
    attach t.leading constr.loc beforeConstr;
    attach t.trailing constr.loc afterConstr
  | Ppat_construct (constr, Some pat) ->
    let leading, trailing = partitionLeadingTrailing comments constr.loc in
    attach t.leading constr.loc leading;
    let afterConstructor, rest =
      partitionAdjacentTrailing constr.loc trailing
    in
    attach t.trailing constr.loc afterConstructor;
    let leading, inside, trailing = partitionByLoc rest pat.ppat_loc in
    attach t.leading pat.ppat_loc leading;
    walkPattern pat t inside;
    attach t.trailing pat.ppat_loc trailing
  | Ppat_variant (_label, None) -> ()
  | Ppat_variant (_label, Some pat) -> walkPattern pat t comments
  | Ppat_type _ -> ()
  | Ppat_record (recordRows, _) ->
    walkList
      (recordRows |> List.map (fun (li, p) -> PatternRecordRow (li, p)))
      t comments
  | Ppat_or _ ->
    walkList
      (Res_parsetree_viewer.collectOrPatternChain pat
      |> List.map (fun pat -> Pattern pat))
      t comments
  | Ppat_constraint (pattern, typ) ->
    let beforePattern, insidePattern, afterPattern =
      partitionByLoc comments pattern.ppat_loc
    in
    attach t.leading pattern.ppat_loc beforePattern;
    walkPattern pattern t insidePattern;
    let afterPattern, rest =
      partitionAdjacentTrailing pattern.ppat_loc afterPattern
    in
    attach t.trailing pattern.ppat_loc afterPattern;
    let beforeTyp, insideTyp, afterTyp = partitionByLoc rest typ.ptyp_loc in
    attach t.leading typ.ptyp_loc beforeTyp;
    walkCoreType typ t insideTyp;
    attach t.trailing typ.ptyp_loc afterTyp
  | Ppat_lazy pattern | Ppat_exception pattern ->
    let leading, inside, trailing = partitionByLoc comments pattern.ppat_loc in
    attach t.leading pattern.ppat_loc leading;
    walkPattern pattern t inside;
    attach t.trailing pattern.ppat_loc trailing
  | Ppat_unpack stringLoc ->
    let leading, trailing = partitionLeadingTrailing comments stringLoc.loc in
    attach t.leading stringLoc.loc leading;
    attach t.trailing stringLoc.loc trailing
  | Ppat_extension extension -> walkExtension extension t comments
  | _ -> ()

(* name: firstName *)
and walkPatternRecordRow row t comments =
  match row with
  (* punned {x}*)
  | ( {Location.txt = Longident.Lident ident; loc = longidentLoc},
      {Parsetree.ppat_desc = Ppat_var {txt; _}} )
    when ident = txt ->
    let beforeLbl, afterLbl = partitionLeadingTrailing comments longidentLoc in
    attach t.leading longidentLoc beforeLbl;
    attach t.trailing longidentLoc afterLbl
  | longident, pattern ->
    let beforeLbl, afterLbl = partitionLeadingTrailing comments longident.loc in
    attach t.leading longident.loc beforeLbl;
    let afterLbl, rest = partitionAdjacentTrailing longident.loc afterLbl in
    attach t.trailing longident.loc afterLbl;
    let leading, inside, trailing = partitionByLoc rest pattern.ppat_loc in
    attach t.leading pattern.ppat_loc leading;
    walkPattern pattern t inside;
    attach t.trailing pattern.ppat_loc trailing

and walkRowField (rowField : Parsetree.row_field) t comments =
  match rowField with
  | Parsetree.Rtag ({loc}, _, _, _) ->
    let before, after = partitionLeadingTrailing comments loc in
    attach t.leading loc before;
    attach t.trailing loc after
  | Rinherit _ -> ()

and walkCoreType typ t comments =
  match typ.Parsetree.ptyp_desc with
  | _ when comments = [] -> ()
  | Ptyp_tuple typexprs ->
    walkList (typexprs |> List.map (fun ct -> CoreType ct)) t comments
  | Ptyp_extension extension -> walkExtension extension t comments
  | Ptyp_package packageType -> walkPackageType packageType t comments
  | Ptyp_alias (typexpr, _alias) ->
    let beforeTyp, insideTyp, afterTyp =
      partitionByLoc comments typexpr.ptyp_loc
    in
    attach t.leading typexpr.ptyp_loc beforeTyp;
    walkCoreType typexpr t insideTyp;
    attach t.trailing typexpr.ptyp_loc afterTyp
  | Ptyp_poly (strings, typexpr) ->
    let comments =
      visitListButContinueWithRemainingComments
        ~getLoc:(fun n -> n.Asttypes.loc)
        ~walkNode:(fun longident t comments ->
          let beforeLongident, afterLongident =
            partitionLeadingTrailing comments longident.loc
          in
          attach t.leading longident.loc beforeLongident;
          attach t.trailing longident.loc afterLongident)
        ~newlineDelimited:false strings t comments
    in
    let beforeTyp, insideTyp, afterTyp =
      partitionByLoc comments typexpr.ptyp_loc
    in
    attach t.leading typexpr.ptyp_loc beforeTyp;
    walkCoreType typexpr t insideTyp;
    attach t.trailing typexpr.ptyp_loc afterTyp
  | Ptyp_variant (rowFields, _, _) ->
    walkList (rowFields |> List.map (fun rf -> RowField rf)) t comments
  | Ptyp_constr (longident, typexprs) ->
    let beforeLongident, _afterLongident =
      partitionLeadingTrailing comments longident.loc
    in
    let afterLongident, rest =
      partitionAdjacentTrailing longident.loc comments
    in
    attach t.leading longident.loc beforeLongident;
    attach t.trailing longident.loc afterLongident;
    walkList (typexprs |> List.map (fun ct -> CoreType ct)) t rest
  | Ptyp_arrow _ ->
    let _, parameters, typexpr = arrowType typ in
    let comments = walkTypeParameters parameters t comments in
    let beforeTyp, insideTyp, afterTyp =
      partitionByLoc comments typexpr.ptyp_loc
    in
    attach t.leading typexpr.ptyp_loc beforeTyp;
    walkCoreType typexpr t insideTyp;
    attach t.trailing typexpr.ptyp_loc afterTyp
  | Ptyp_object (fields, _) -> walkTypObjectFields fields t comments
  | _ -> ()

and walkTypObjectFields fields t comments =
  walkList (fields |> List.map (fun f -> ObjectField f)) t comments

and walkObjectField field t comments =
  match field with
  | Otag (lbl, _, typexpr) ->
    let beforeLbl, afterLbl = partitionLeadingTrailing comments lbl.loc in
    attach t.leading lbl.loc beforeLbl;
    let afterLbl, rest = partitionAdjacentTrailing lbl.loc afterLbl in
    attach t.trailing lbl.loc afterLbl;
    let beforeTyp, insideTyp, afterTyp = partitionByLoc rest typexpr.ptyp_loc in
    attach t.leading typexpr.ptyp_loc beforeTyp;
    walkCoreType typexpr t insideTyp;
    attach t.trailing typexpr.ptyp_loc afterTyp
  | _ -> ()

and walkTypeParameters typeParameters t comments =
  visitListButContinueWithRemainingComments
    ~getLoc:(fun (_, _, typexpr) ->
      match typexpr.Parsetree.ptyp_attributes with
      | ({Location.txt = "ns.namedArgLoc"; loc}, _) :: _attrs ->
        {loc with loc_end = typexpr.ptyp_loc.loc_end}
      | _ -> typexpr.ptyp_loc)
    ~walkNode:walkTypeParameter ~newlineDelimited:false typeParameters t
    comments

and walkTypeParameter (_attrs, _lbl, typexpr) t comments =
  let beforeTyp, insideTyp, afterTyp =
    partitionByLoc comments typexpr.ptyp_loc
  in
  attach t.leading typexpr.ptyp_loc beforeTyp;
  walkCoreType typexpr t insideTyp;
  attach t.trailing typexpr.ptyp_loc afterTyp

and walkPackageType packageType t comments =
  let longident, packageConstraints = packageType in
  let beforeLongident, afterLongident =
    partitionLeadingTrailing comments longident.loc
  in
  attach t.leading longident.loc beforeLongident;
  let afterLongident, rest =
    partitionAdjacentTrailing longident.loc afterLongident
  in
  attach t.trailing longident.loc afterLongident;
  walkPackageConstraints packageConstraints t rest

and walkPackageConstraints packageConstraints t comments =
  walkList
    (packageConstraints |> List.map (fun (li, te) -> PackageConstraint (li, te)))
    t comments

and walkPackageConstraint packageConstraint t comments =
  let longident, typexpr = packageConstraint in
  let beforeLongident, afterLongident =
    partitionLeadingTrailing comments longident.loc
  in
  attach t.leading longident.loc beforeLongident;
  let afterLongident, rest =
    partitionAdjacentTrailing longident.loc afterLongident
  in
  attach t.trailing longident.loc afterLongident;
  let beforeTyp, insideTyp, afterTyp = partitionByLoc rest typexpr.ptyp_loc in
  attach t.leading typexpr.ptyp_loc beforeTyp;
  walkCoreType typexpr t insideTyp;
  attach t.trailing typexpr.ptyp_loc afterTyp

and walkExtension extension t comments =
  let id, payload = extension in
  let beforeId, afterId = partitionLeadingTrailing comments id.loc in
  attach t.leading id.loc beforeId;
  let afterId, rest = partitionAdjacentTrailing id.loc afterId in
  attach t.trailing id.loc afterId;
  walkPayload payload t rest

and walkAttribute (id, payload) t comments =
  let beforeId, afterId = partitionLeadingTrailing comments id.loc in
  attach t.leading id.loc beforeId;
  let afterId, rest = partitionAdjacentTrailing id.loc afterId in
  attach t.trailing id.loc afterId;
  walkPayload payload t rest

and walkPayload payload t comments =
  match payload with
  | PStr s -> walkStructure s t comments
  | _ -> ()
