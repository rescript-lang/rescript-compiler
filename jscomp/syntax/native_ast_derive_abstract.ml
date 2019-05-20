module U = Ast_derive_util
open Ast_helper
type tdcls = Parsetree.type_declaration list

let deprecated name =
  Ast_attributes.deprecated
    ("use " ^ name ^ "Get instead or use {abstract = light} explicitly")

let strip_option arg_name =
   if arg_name.[0] = '?' then
     String.sub arg_name 1 (String.length arg_name - 1)
   else arg_name

let handleTdcl light (tdcl : Parsetree.type_declaration) =
   let core_type = U.core_type_of_type_declaration tdcl in
   let loc = tdcl.ptype_loc in
   let type_name = tdcl.ptype_name.txt in
   match tdcl.ptype_kind with
   | Ptype_record label_declarations ->
     let is_private = tdcl.ptype_private = Private in
     let (has_optional_field, new_label_declarations) =
       Ext_list.fold_right label_declarations (false, []) (fun ({pld_type; pld_loc; pld_attributes} as dcl : Parsetree.label_declaration) (has_optional_field, acc) ->
           let has_optional_field_local = Ast_attributes.has_bs_optional pld_attributes in
           let acc = if has_optional_field_local then
             { dcl with
              pld_type = {
                dcl.pld_type with
                ptyp_desc = Ptyp_constr({txt = Lident "option"; loc = pld_loc}, [pld_type]);
                ptyp_loc = pld_loc;
              };
              pld_attributes = Ext_list.exclude pld_attributes (fun x -> (Ast_attributes.is_optional x) || (Ast_attributes.is_bs_as x))
            } :: acc
           else dcl :: acc in
             (has_optional_field || has_optional_field_local, acc)
         ) in
     let newTdcl = {
       tdcl with
       ptype_kind = Ptype_record new_label_declarations;
       ptype_attributes = [];
     } in
     let setter_accessor, makeType, labels =
       Ext_list.fold_right
         label_declarations
         ([],
          (if has_optional_field then
             Ast_compatible.arrow ~loc  (Ast_literal.type_unit ()) core_type
           else  core_type),
          [])
         (fun
           ({pld_name =
               {txt = label_name; loc = label_loc} as pld_name;
             pld_type;
             pld_mutable;
             pld_attributes;
             pld_loc
            }:
              Parsetree.label_declaration) (acc, maker, labels) ->
           let prim = [label_name] in
           let is_optional = Ast_attributes.has_bs_optional pld_attributes in

           let newLabel = if is_optional then {pld_name with txt = Ast_compatible.opt_label pld_name.Asttypes.txt} else pld_name in

            let maker, getter_type =
              if is_optional then
                let maker_optional_type = Ast_core_type.lift_option_type pld_type in
                let getter_optional_type = {
                  Parsetree.ptyp_desc =
                   Ptyp_constr(
                     {txt = Lident "option";
                      loc = pld_loc
                     }, [pld_type]);
                  ptyp_loc = pld_loc;
                  ptyp_attributes = [];
                } in
                Ast_compatible.opt_arrow ~loc:pld_loc label_name
#if OCAML_VERSION =~ "<4.03.0" then
                  maker_optional_type
#else             pld_type
#end
                maker,
                Ast_compatible.arrow ~loc  core_type getter_optional_type
              else
                Ast_compatible.label_arrow ~loc:pld_loc label_name pld_type maker,
                Ast_compatible.arrow ~loc  core_type pld_type
           in
           let makeGetter light deprec pld_name =
             Str.value Nonrecursive [
                 Vb.mk
                   ~loc:pld_loc
                   ~attrs:(if deprec then deprecated (pld_name.Asttypes.txt) :: []
                     else [])
                   (Pat.var {pld_name with txt = if light then label_name else label_name ^ "Get"})
                   (Exp.constraint_ (Ast_compatible.fun_ ~loc:pld_loc
                       (Pat.var {Location.txt = "o"; loc = pld_loc})
                       (Exp.field (Exp.ident {Location.txt = Longident.Lident "o"; loc = pld_loc}) {txt = Longident.Lident pld_name.Location.txt; loc = pld_loc})) getter_type)]
             in
             let acc = if not light then
               makeGetter true true pld_name :: makeGetter false false pld_name  :: acc
             else  makeGetter true false pld_name :: acc in
           let is_current_field_mutable = pld_mutable = Mutable in
           let acc =
             if is_current_field_mutable then
               let setter_type =
                (Ast_compatible.arrow core_type
                   (Ast_compatible.arrow
                      pld_type (* setter *)
                      (Ast_literal.type_unit ()))) in
               let variable = (Exp.ident {Location.txt = Longident.Lident "v"; loc = pld_loc}) in
               let setter = Str.value Nonrecursive [
                 Vb.mk
                   (Pat.var {loc = label_loc; txt = label_name ^ "Set"})
                   (Exp.constraint_ (Ast_compatible.fun_ ~loc:pld_loc
                       (Pat.var {Location.txt = "o"; loc = pld_loc})
                       (Ast_compatible.fun_ ~loc:pld_loc
                         (Pat.var {Location.txt = "v"; loc = pld_loc})
                         (Exp.setfield
                           (Exp.ident {Location.txt = Longident.Lident "o"; loc = pld_loc})
                           {txt = Longident.Lident pld_name.Location.txt; loc = pld_loc}
                           (if is_optional then Exp.construct {txt=Lident "Some"; loc = pld_loc} (Some variable) else variable))))
                       setter_type)
                   ]
                 in
               setter :: acc
             else acc in
           acc,
           maker,
           newLabel::labels
         )
     in
     newTdcl,
     (if is_private then
        setter_accessor
      else
        let my_loc = match labels with
        | [] -> !default_loc
        | { Asttypes.loc = label_loc } :: _ -> label_loc
        in
        let maker_body = Exp.record (Ext_list.fold_right labels [] (fun ({ Asttypes.txt; loc = label_loc }) rest ->
           let field_name = {Asttypes.txt = Longident.Lident (strip_option txt); loc = label_loc} in
           (field_name, Exp.ident field_name) :: rest
         )) None in
        (* This is to support bs.optional, which makes certain args of the function optional so we
           add a unit at the end to prevent auto-currying issues. *)
        let body_with_extra_unit_fun = (if has_optional_field then
           (Ast_compatible.fun_ ~loc:my_loc
             (Pat.var ({txt = "()"; loc = my_loc})) maker_body)
         else maker_body) in

        let myMaker =
         Str.value Nonrecursive [
           Vb.mk
             (Pat.var {loc; txt = type_name})
             (Exp.constraint_ (
               Ext_list.fold_right
                 labels
                 body_with_extra_unit_fun
                 (fun arg_name rest ->
                   (Ast_compatible.label_fun ~label:arg_name.Asttypes.txt ~loc:my_loc
                     (Pat.var ({arg_name with txt = strip_option arg_name.Asttypes.txt})) rest))
                 ) makeType)
         ]
         in
        (myMaker :: setter_accessor))

    | Ptype_abstract
   | Ptype_variant _
   | Ptype_open ->
     (* Looks obvious that it does not make sense to warn *)
     (* U.notApplicable tdcl.ptype_loc derivingName;  *)
     tdcl, []

let code_sig_transform sigi = match sigi with
  | {Parsetree.pstr_loc; pstr_desc =
      Pstr_value (_, (({
        pvb_pat = {ppat_desc = Ppat_var name};
        pvb_expr = {pexp_desc = Pexp_constraint (_, typ)}
      } as _makerVb) :: []))
    } ->
    Sig.value (Val.mk ~loc:pstr_loc name typ)
  | _ -> Sig.type_ []

let handleTdclsInStr ~light tdcls =
  let tdcls, tdcls_sig, code, code_sig =
    Ext_list.fold_right tdcls ([],[], [], []) (fun tdcl (tdcls, tdcls_sig, sts, code_sig)  ->
        match handleTdcl light tdcl with
          ntdcl, value_descriptions ->
          let open Parsetree in
          (
            ntdcl::tdcls,
            {ntdcl with ptype_kind = Ptype_abstract }::tdcls_sig,
            Ext_list.map_append value_descriptions sts (fun x -> x),
            Ext_list.map_append value_descriptions code_sig code_sig_transform
          )
      )  in
  (Ast_compatible.rec_type_str tdcls :: code,
   Ast_compatible.rec_type_sig tdcls_sig :: code_sig)
  (* still need perform transformation for non-abstract type*)

let handleTdclsInSig ~light tdcls =
  let tdcls_sig, code =
    Ext_list.fold_right tdcls ([], []) (fun tdcl (tdcls_sig, sts)  ->
        match handleTdcl light tdcl with
          ntdcl, value_descriptions ->
          let open Parsetree in
          (
            {ntdcl with ptype_kind = Ptype_abstract }::tdcls_sig,
            Ext_list.map_append value_descriptions sts code_sig_transform
          )
      )  in
   Ast_compatible.rec_type_sig tdcls_sig :: code
