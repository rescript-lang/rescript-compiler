(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*    Thomas Gazagnaire (OCamlPro), Fabrice Le Fessant (INRIA Saclay)     *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree
open Parsetree
open Ast_helper

(*
Some notes:

   * For Pexp_function, we cannot go back to the exact original version
   when there is a default argument, because the default argument is
   translated in the typer. The code, if printed, will not be parsable because
   new generated identifiers are not correct.

   * For Pexp_apply, it is unclear whether arguments are reordered, especially
    when there are optional arguments.

  * TODO: check Ttype_variant -> Ptype_variant (stub None)

*)

let string_is_prefix sub str =
  let sublen = String.length sub in
  String.length str >= sublen && String.sub str 0 sublen = sub

let option f = function None -> None | Some e -> Some (f e)

let rec lident_of_path path =
  match path with
      Path.Pident id -> Longident.Lident (Ident.name id)
    | Path.Pdot (p, s, _) -> Longident.Ldot (lident_of_path p, s)
    | Path.Papply (p1, p2) ->
        Longident.Lapply (lident_of_path p1, lident_of_path p2)

let rec untype_structure str =
  List.map untype_structure_item str.str_items

and untype_structure_item item =
  let desc =
    match item.str_desc with
      Tstr_eval (exp, attrs) -> Pstr_eval (untype_expression exp, attrs)
    | Tstr_value (rec_flag, list) ->
        Pstr_value (rec_flag, List.map untype_binding list)
    | Tstr_primitive vd ->
        Pstr_primitive (untype_value_description vd)
    | Tstr_type list ->
        Pstr_type (List.map untype_type_declaration list)
    | Tstr_typext tyext ->
        Pstr_typext (untype_type_extension tyext)
    | Tstr_exception ext ->
        Pstr_exception (untype_extension_constructor ext)
    | Tstr_module mb ->
        Pstr_module (untype_module_binding mb)
    | Tstr_recmodule list ->
        Pstr_recmodule (List.map untype_module_binding list)
    | Tstr_modtype mtd ->
        Pstr_modtype {pmtd_name=mtd.mtd_name;
                      pmtd_type=option untype_module_type mtd.mtd_type;
                      pmtd_loc=mtd.mtd_loc;pmtd_attributes=mtd.mtd_attributes;}
    | Tstr_open od ->
        Pstr_open {popen_lid = od.open_txt; popen_override = od.open_override;
                   popen_attributes = od.open_attributes;
                   popen_loc = od.open_loc;
                  }
    | Tstr_class list ->
        Pstr_class
          (List.map
             (fun (ci, _, _) -> untype_class_declaration ci)
             list)
    | Tstr_class_type list ->
        Pstr_class_type
          (List.map
             (fun (_id, _name, ct) -> untype_class_type_declaration ct)
             list)
    | Tstr_include incl ->
        Pstr_include {pincl_mod = untype_module_expr incl.incl_mod;
                      pincl_attributes = incl.incl_attributes;
                      pincl_loc = incl.incl_loc;
                     }
    | Tstr_attribute x ->
        Pstr_attribute x
  in
  { pstr_desc = desc; pstr_loc = item.str_loc; }

and untype_value_description v =
  {
   pval_name = v.val_name;
   pval_prim = v.val_prim;
   pval_type = untype_core_type v.val_desc;
   pval_loc = v.val_loc;
   pval_attributes = v.val_attributes;
  }

and untype_module_binding mb =
  {
   pmb_name = mb.mb_name;
   pmb_expr = untype_module_expr mb.mb_expr;
   pmb_attributes = mb.mb_attributes;
   pmb_loc = mb.mb_loc;
  }

and untype_type_declaration decl =
  {
    ptype_name = decl.typ_name;
    ptype_params = List.map untype_type_parameter decl.typ_params;
    ptype_cstrs = List.map (fun (ct1, ct2, loc) ->
        (untype_core_type ct1,
          untype_core_type ct2, loc)
    ) decl.typ_cstrs;
    ptype_kind = (match decl.typ_kind with
        Ttype_abstract -> Ptype_abstract
      | Ttype_variant list ->
          Ptype_variant (List.map untype_constructor_declaration list)
      | Ttype_record list ->
          Ptype_record (List.map untype_label_declaration list)
      | Ttype_open -> Ptype_open
    );
    ptype_private = decl.typ_private;
    ptype_manifest = option untype_core_type decl.typ_manifest;
    ptype_attributes = decl.typ_attributes;
    ptype_loc = decl.typ_loc;
  }

and untype_type_parameter (ct, v) = (untype_core_type ct, v)

and untype_constructor_declaration cd =
  {
   pcd_name = cd.cd_name;
   pcd_args = List.map untype_core_type cd.cd_args;
   pcd_res = option untype_core_type cd.cd_res;
   pcd_loc = cd.cd_loc;
   pcd_attributes = cd.cd_attributes;
  }

and untype_label_declaration ld =
  {
    pld_name=ld.ld_name;
    pld_mutable=ld.ld_mutable;
    pld_type=untype_core_type ld.ld_type;
    pld_loc=ld.ld_loc;
    pld_attributes=ld.ld_attributes
  }

and untype_type_extension tyext =
  {
    ptyext_path = tyext.tyext_txt;
    ptyext_params = List.map untype_type_parameter tyext.tyext_params;
    ptyext_constructors =
      List.map untype_extension_constructor tyext.tyext_constructors;
    ptyext_private = tyext.tyext_private;
    ptyext_attributes = tyext.tyext_attributes;
  }

and untype_extension_constructor ext =
  {
    pext_name = ext.ext_name;
    pext_kind = (match ext.ext_kind with
        Text_decl (args, ret) ->
          Pext_decl (List.map untype_core_type args,
                     option untype_core_type ret)
      | Text_rebind (_p, lid) -> Pext_rebind lid
    );
    pext_loc = ext.ext_loc;
    pext_attributes = ext.ext_attributes;
  }

and untype_pattern pat =
  let desc =
  match pat with
      { pat_extra=[Tpat_unpack, _, _attrs]; pat_desc = Tpat_var (_,name); _ } ->
        Ppat_unpack name
    | { pat_extra=[Tpat_type (_path, lid), _, _attrs]; _ } -> Ppat_type lid
    | { pat_extra= (Tpat_constraint ct, _, _attrs) :: rem; _ } ->
        Ppat_constraint (untype_pattern { pat with pat_extra=rem },
                         untype_core_type ct)
    | _ ->
    match pat.pat_desc with
      Tpat_any -> Ppat_any
    | Tpat_var (id, name) ->
        begin
          match (Ident.name id).[0] with
            'A'..'Z' ->
              Ppat_unpack name
          | _ ->
              Ppat_var name
        end
    | Tpat_alias (pat, _id, name) ->
        Ppat_alias (untype_pattern pat, name)
    | Tpat_constant cst -> Ppat_constant cst
    | Tpat_tuple list ->
        Ppat_tuple (List.map untype_pattern list)
    | Tpat_construct (lid, _, args) ->
        Ppat_construct (lid,
          (match args with
              [] -> None
            | [arg] -> Some (untype_pattern arg)
            | args ->
                Some
                  (Pat.tuple ~loc:pat.pat_loc
                     (List.map untype_pattern args)
                  )
          ))
    | Tpat_variant (label, pato, _) ->
        Ppat_variant (label, option untype_pattern pato)
    | Tpat_record (list, closed) ->
        Ppat_record (List.map (fun (lid, _, pat) ->
              lid, untype_pattern pat) list, closed)
    | Tpat_array list -> Ppat_array (List.map untype_pattern list)
    | Tpat_or (p1, p2, _) -> Ppat_or (untype_pattern p1, untype_pattern p2)
    | Tpat_lazy p -> Ppat_lazy (untype_pattern p)
  in
  Pat.mk ~loc:pat.pat_loc ~attrs:pat.pat_attributes desc
    (* todo: fix attributes on extras *)

and untype_extra (extra, loc, attrs) sexp =
  let desc =
    match extra with
      Texp_coerce (cty1, cty2) ->
        Pexp_coerce (sexp,
                     option untype_core_type cty1,
                     untype_core_type cty2)
    | Texp_constraint cty ->
        Pexp_constraint (sexp, untype_core_type cty)
    | Texp_open (ovf, _path, lid, _) -> Pexp_open (ovf, lid, sexp)
    | Texp_poly cto -> Pexp_poly (sexp, option untype_core_type cto)
    | Texp_newtype s -> Pexp_newtype (s, sexp)
  in
  Exp.mk ~loc ~attrs desc

and untype_cases l = List.map untype_case l

and untype_case {c_lhs; c_guard; c_rhs} =
  {
   pc_lhs = untype_pattern c_lhs;
   pc_guard = option untype_expression c_guard;
   pc_rhs = untype_expression c_rhs;
  }

and untype_binding {vb_pat; vb_expr; vb_attributes; vb_loc} =
  {
    pvb_pat = untype_pattern vb_pat;
    pvb_expr = untype_expression vb_expr;
    pvb_attributes = vb_attributes;
    pvb_loc = vb_loc;
  }

and untype_expression exp =
  let desc =
    match exp.exp_desc with
      Texp_ident (_path, lid, _) -> Pexp_ident (lid)
    | Texp_constant cst -> Pexp_constant cst
    | Texp_let (rec_flag, list, exp) ->
        Pexp_let (rec_flag,
          List.map untype_binding list,
          untype_expression exp)
    | Texp_function (label, [{c_lhs=p; c_guard=None; c_rhs=e}], _) ->
        Pexp_fun (label, None, untype_pattern p, untype_expression e)
    | Texp_function ("", cases, _) ->
        Pexp_function (untype_cases cases)
    | Texp_function _ ->
        assert false
    | Texp_apply (exp, list) ->
        Pexp_apply (untype_expression exp,
          List.fold_right (fun (label, expo, _) list ->
              match expo with
                None -> list
              | Some exp -> (label, untype_expression exp) :: list
          ) list [])
    | Texp_match (exp, cases, exn_cases, _) ->
      let merged_cases = untype_cases cases
        @ List.map
          (fun c ->
            let uc = untype_case c in
            let pat = { uc.pc_lhs
                        with ppat_desc = Ppat_exception uc.pc_lhs }
            in
            { uc with pc_lhs = pat })
          exn_cases
      in
      Pexp_match (untype_expression exp, merged_cases)
    | Texp_try (exp, cases) ->
        Pexp_try (untype_expression exp, untype_cases cases)
    | Texp_tuple list ->
        Pexp_tuple (List.map untype_expression list)
    | Texp_construct (lid, _, args) ->
        Pexp_construct (lid,
          (match args with
              [] -> None
          | [ arg ] -> Some (untype_expression arg)
          | args ->
              Some
                (Exp.tuple ~loc:exp.exp_loc (List.map untype_expression args))
          ))
    | Texp_variant (label, expo) ->
        Pexp_variant (label, option untype_expression expo)
    | Texp_record (list, expo) ->
        Pexp_record (List.map (fun (lid, _, exp) ->
              lid, untype_expression exp
          ) list,
          option untype_expression expo)
    | Texp_field (exp, lid, _label) ->
        Pexp_field (untype_expression exp, lid)
    | Texp_setfield (exp1, lid, _label, exp2) ->
        Pexp_setfield (untype_expression exp1, lid,
          untype_expression exp2)
    | Texp_array list ->
        Pexp_array (List.map untype_expression list)
    | Texp_ifthenelse (exp1, exp2, expo) ->
        Pexp_ifthenelse (untype_expression exp1,
          untype_expression exp2,
          option untype_expression expo)
    | Texp_sequence (exp1, exp2) ->
        Pexp_sequence (untype_expression exp1, untype_expression exp2)
    | Texp_while (exp1, exp2) ->
        Pexp_while (untype_expression exp1, untype_expression exp2)
    | Texp_for (_id, name, exp1, exp2, dir, exp3) ->
        Pexp_for (name,
          untype_expression exp1, untype_expression exp2,
          dir, untype_expression exp3)
    | Texp_send (exp, meth, _) ->
        Pexp_send (untype_expression exp, match meth with
            Tmeth_name name -> name
          | Tmeth_val id -> Ident.name id)
    | Texp_new (_path, lid, _) -> Pexp_new (lid)
    | Texp_instvar (_, path, name) ->
      Pexp_ident ({name with txt = lident_of_path path})
    | Texp_setinstvar (_, _path, lid, exp) ->
        Pexp_setinstvar (lid, untype_expression exp)
    | Texp_override (_, list) ->
        Pexp_override (List.map (fun (_path, lid, exp) ->
              lid, untype_expression exp
          ) list)
    | Texp_letmodule (_id, name, mexpr, exp) ->
        Pexp_letmodule (name, untype_module_expr mexpr,
          untype_expression exp)
    | Texp_assert exp -> Pexp_assert (untype_expression exp)
    | Texp_lazy exp -> Pexp_lazy (untype_expression exp)
    | Texp_object (cl, _) ->
        Pexp_object (untype_class_structure cl)
    | Texp_pack (mexpr) ->
        Pexp_pack (untype_module_expr mexpr)
  in
  List.fold_right untype_extra exp.exp_extra
    (Exp.mk ~loc:exp.exp_loc ~attrs:exp.exp_attributes desc)

and untype_package_type pack =
  (pack.pack_txt,
    List.map (fun (s, ct) ->
        (s, untype_core_type ct)) pack.pack_fields)

and untype_signature sg =
  List.map untype_signature_item sg.sig_items

and untype_signature_item item =
  let desc =
    match item.sig_desc with
      Tsig_value v ->
        Psig_value (untype_value_description v)
    | Tsig_type list ->
        Psig_type (List.map untype_type_declaration list)
    | Tsig_typext tyext ->
        Psig_typext (untype_type_extension tyext)
    | Tsig_exception ext ->
        Psig_exception (untype_extension_constructor ext)
    | Tsig_module md ->
        Psig_module {pmd_name = md.md_name;
                     pmd_type = untype_module_type md.md_type;
                     pmd_attributes = md.md_attributes; pmd_loc = md.md_loc;
                    }
    | Tsig_recmodule list ->
        Psig_recmodule (List.map (fun md ->
              {pmd_name = md.md_name; pmd_type = untype_module_type md.md_type;
               pmd_attributes = md.md_attributes; pmd_loc = md.md_loc}) list)
    | Tsig_modtype mtd ->
        Psig_modtype {pmtd_name=mtd.mtd_name;
                      pmtd_type=option untype_module_type mtd.mtd_type;
                      pmtd_attributes=mtd.mtd_attributes; pmtd_loc=mtd.mtd_loc}
    | Tsig_open od ->
        Psig_open {popen_lid = od.open_txt;
                   popen_override = od.open_override;
                   popen_attributes = od.open_attributes;
                   popen_loc = od.open_loc;
                  }
    | Tsig_include incl ->
        Psig_include {pincl_mod = untype_module_type incl.incl_mod;
                      pincl_attributes = incl.incl_attributes;
                      pincl_loc = incl.incl_loc;
                     }
    | Tsig_class list ->
        Psig_class (List.map untype_class_description list)
    | Tsig_class_type list ->
        Psig_class_type (List.map untype_class_type_declaration list)
    | Tsig_attribute x ->
        Psig_attribute x
  in
  { psig_desc = desc;
    psig_loc = item.sig_loc;
  }

and untype_class_declaration cd =
  {
    pci_virt = cd.ci_virt;
    pci_params = List.map untype_type_parameter cd.ci_params;
    pci_name = cd.ci_id_name;
    pci_expr = untype_class_expr cd.ci_expr;
    pci_loc = cd.ci_loc;
    pci_attributes = cd.ci_attributes;
  }

and untype_class_description cd =
  {
    pci_virt = cd.ci_virt;
    pci_params = List.map untype_type_parameter cd.ci_params;
    pci_name = cd.ci_id_name;
    pci_expr = untype_class_type cd.ci_expr;
    pci_loc = cd.ci_loc;
    pci_attributes = cd.ci_attributes;
  }

and untype_class_type_declaration cd =
  {
    pci_virt = cd.ci_virt;
    pci_params = List.map untype_type_parameter cd.ci_params;
    pci_name = cd.ci_id_name;
    pci_expr = untype_class_type cd.ci_expr;
    pci_loc = cd.ci_loc;
    pci_attributes = cd.ci_attributes;
  }

and untype_module_type mty =
  let desc = match mty.mty_desc with
      Tmty_ident (_path, lid) -> Pmty_ident (lid)
    | Tmty_alias (_path, lid) -> Pmty_alias (lid)
    | Tmty_signature sg -> Pmty_signature (untype_signature sg)
    | Tmty_functor (_id, name, mtype1, mtype2) ->
        Pmty_functor (name, Misc.may_map untype_module_type mtype1,
          untype_module_type mtype2)
    | Tmty_with (mtype, list) ->
        Pmty_with (untype_module_type mtype,
          List.map (fun (_path, lid, withc) ->
              untype_with_constraint lid withc
          ) list)
    | Tmty_typeof mexpr ->
        Pmty_typeof (untype_module_expr mexpr)
  in
  Mty.mk ~loc:mty.mty_loc desc

and untype_with_constraint lid cstr =
  match cstr with
    Twith_type decl -> Pwith_type (lid, untype_type_declaration decl)
  | Twith_module (_path, lid2) -> Pwith_module (lid, lid2)
  | Twith_typesubst decl -> Pwith_typesubst (untype_type_declaration decl)
  | Twith_modsubst (_path, lid2) ->
      Pwith_modsubst ({loc = lid.loc; txt=Longident.last lid.txt}, lid2)

and untype_module_expr mexpr =
  match mexpr.mod_desc with
    Tmod_constraint (m, _, Tmodtype_implicit, _ ) ->
      untype_module_expr m
  | _ ->
      let desc = match mexpr.mod_desc with
          Tmod_ident (_p, lid) -> Pmod_ident (lid)
        | Tmod_structure st -> Pmod_structure (untype_structure st)
        | Tmod_functor (_id, name, mtype, mexpr) ->
            Pmod_functor (name, Misc.may_map untype_module_type mtype,
              untype_module_expr mexpr)
        | Tmod_apply (mexp1, mexp2, _) ->
            Pmod_apply (untype_module_expr mexp1, untype_module_expr mexp2)
        | Tmod_constraint (mexpr, _, Tmodtype_explicit mtype, _) ->
            Pmod_constraint (untype_module_expr mexpr,
              untype_module_type mtype)
        | Tmod_constraint (_mexpr, _, Tmodtype_implicit, _) ->
            assert false
        | Tmod_unpack (exp, _pack) ->
        Pmod_unpack (untype_expression exp)
        (* TODO , untype_package_type pack) *)

  in
  Mod.mk ~loc:mexpr.mod_loc desc

and untype_class_expr cexpr =
  let desc = match cexpr.cl_desc with
    | Tcl_constraint ( { cl_desc = Tcl_ident (_path, lid, tyl); _ },
                       None, _, _, _ ) ->
        Pcl_constr (lid,
          List.map untype_core_type tyl)
    | Tcl_structure clstr -> Pcl_structure (untype_class_structure clstr)

    | Tcl_fun (label, pat, _pv, cl, _partial) ->
        Pcl_fun (label, None, untype_pattern pat, untype_class_expr cl)

    | Tcl_apply (cl, args) ->
        Pcl_apply (untype_class_expr cl,
          List.fold_right (fun (label, expo, _) list ->
              match expo with
                None -> list
              | Some exp -> (label, untype_expression exp) :: list
          ) args [])

    | Tcl_let (rec_flat, bindings, _ivars, cl) ->
        Pcl_let (rec_flat,
          List.map untype_binding bindings,
          untype_class_expr cl)

    | Tcl_constraint (cl, Some clty, _vals, _meths, _concrs) ->
        Pcl_constraint (untype_class_expr cl,  untype_class_type clty)

    | Tcl_ident _ -> assert false
    | Tcl_constraint (_, None, _, _, _) -> assert false
  in
  { pcl_desc = desc;
    pcl_loc = cexpr.cl_loc;
    pcl_attributes = cexpr.cl_attributes;
  }

and untype_class_type ct =
  let desc = match ct.cltyp_desc with
      Tcty_signature csg -> Pcty_signature (untype_class_signature csg)
    | Tcty_constr (_path, lid, list) ->
        Pcty_constr (lid, List.map untype_core_type list)
    | Tcty_arrow (label, ct, cl) ->
        Pcty_arrow (label, untype_core_type ct, untype_class_type cl)
  in
  { pcty_desc = desc;
    pcty_loc = ct.cltyp_loc;
    pcty_attributes = ct.cltyp_attributes;
   }

and untype_class_signature cs =
  {
    pcsig_self = untype_core_type cs.csig_self;
    pcsig_fields = List.map untype_class_type_field cs.csig_fields;
  }

and untype_class_type_field ctf =
  let desc = match ctf.ctf_desc with
      Tctf_inherit ct -> Pctf_inherit (untype_class_type ct)
    | Tctf_val (s, mut, virt, ct) ->
        Pctf_val (s, mut, virt, untype_core_type ct)
    | Tctf_method  (s, priv, virt, ct) ->
        Pctf_method  (s, priv, virt, untype_core_type ct)
    | Tctf_constraint  (ct1, ct2) ->
        Pctf_constraint (untype_core_type ct1, untype_core_type ct2)
    | Tctf_attribute x -> Pctf_attribute x
  in
  {
    pctf_desc = desc;
    pctf_loc = ctf.ctf_loc;
    pctf_attributes = ctf.ctf_attributes;
  }

and untype_core_type ct =
  let desc = match ct.ctyp_desc with
      Ttyp_any -> Ptyp_any
    | Ttyp_var s -> Ptyp_var s
    | Ttyp_arrow (label, ct1, ct2) ->
        Ptyp_arrow (label, untype_core_type ct1, untype_core_type ct2)
  | Ttyp_tuple list -> Ptyp_tuple (List.map untype_core_type list)
    | Ttyp_constr (_path, lid, list) ->
        Ptyp_constr (lid,
          List.map untype_core_type list)
    | Ttyp_object (list, o) ->
        Ptyp_object
          (List.map (fun (s, a, t) -> (s, a, untype_core_type t)) list, o)
    | Ttyp_class (_path, lid, list) ->
        Ptyp_class (lid, List.map untype_core_type list)
    | Ttyp_alias (ct, s) ->
        Ptyp_alias (untype_core_type ct, s)
    | Ttyp_variant (list, bool, labels) ->
        Ptyp_variant (List.map untype_row_field list, bool, labels)
    | Ttyp_poly (list, ct) -> Ptyp_poly (list, untype_core_type ct)
    | Ttyp_package pack -> Ptyp_package (untype_package_type pack)
  in
  Typ.mk ~loc:ct.ctyp_loc desc

and untype_class_structure cs =
  let rec remove_self = function
    | { pat_desc = Tpat_alias (p, id, _s) } when string_is_prefix "selfpat-" id.Ident.name ->
        remove_self p
    | p -> p
  in
  { pcstr_self = untype_pattern (remove_self cs.cstr_self);
    pcstr_fields = List.map untype_class_field cs.cstr_fields;
  }

and untype_row_field rf =
  match rf with
    Ttag (label, attrs, bool, list) ->
      Rtag (label, attrs, bool, List.map untype_core_type list)
  | Tinherit ct -> Rinherit (untype_core_type ct)

and is_self_pat = function
  | { pat_desc = Tpat_alias(_pat, id, _) } ->
      string_is_prefix "self-" (Ident.name id)
  | _ -> false

and untype_class_field cf =
  let desc = match cf.cf_desc with
      Tcf_inherit (ovf, cl, super, _vals, _meths) ->
        Pcf_inherit (ovf, untype_class_expr cl, super)
    | Tcf_constraint (cty, cty') ->
        Pcf_constraint (untype_core_type cty, untype_core_type cty')
    | Tcf_val (lab, mut, _, Tcfk_virtual cty, _) ->
        Pcf_val (lab, mut, Cfk_virtual (untype_core_type cty))
    | Tcf_val (lab, mut, _, Tcfk_concrete (o, exp), _) ->
        Pcf_val (lab, mut, Cfk_concrete (o, untype_expression exp))
    | Tcf_method (lab, priv, Tcfk_virtual cty) ->
        Pcf_method (lab, priv, Cfk_virtual (untype_core_type cty))
    | Tcf_method (lab, priv, Tcfk_concrete (o, exp)) ->
        let remove_fun_self = function
          | { exp_desc = Texp_function("", [case], _) } when is_self_pat case.c_lhs && case.c_guard = None -> case.c_rhs
          | e -> e
        in
        let exp = remove_fun_self exp in
        Pcf_method (lab, priv, Cfk_concrete (o, untype_expression exp))
    | Tcf_initializer exp ->
        let remove_fun_self = function
          | { exp_desc = Texp_function("", [case], _) } when is_self_pat case.c_lhs && case.c_guard = None -> case.c_rhs
          | e -> e
        in
        let exp = remove_fun_self exp in
        Pcf_initializer (untype_expression exp)
    | Tcf_attribute x -> Pcf_attribute x
  in
  { pcf_desc = desc; pcf_loc = cf.cf_loc; pcf_attributes = cf.cf_attributes }
