(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Substitutions *)

open Misc
open Path
open Types
open Btype

type t =
  { types: (Ident.t, Path.t) Tbl.t;
    modules: (Ident.t, Path.t) Tbl.t;
    modtypes: (Ident.t, module_type) Tbl.t;
    for_saving: bool }

let identity =
  { types = Tbl.empty; modules = Tbl.empty; modtypes = Tbl.empty;
    for_saving = false }

let add_type id p s = { s with types = Tbl.add id p s.types }

let add_module id p s = { s with modules = Tbl.add id p s.modules }

let add_modtype id ty s = { s with modtypes = Tbl.add id ty s.modtypes }

let for_saving s = { s with for_saving = true }

let loc s x =
  if s.for_saving && not !Clflags.keep_locs then Location.none else x

let remove_loc =
  let open Ast_mapper in
  {default_mapper with location = (fun _this _loc -> Location.none)}

let is_not_doc = function
  | ({Location.txt = "ocaml.doc"}, _) -> false
  | ({Location.txt = "ocaml.text"}, _) -> false
  | ({Location.txt = "doc"}, _) -> false
  | ({Location.txt = "text"}, _) -> false
  | _ -> true

let attrs s x =
  let x =
    if s.for_saving && not !Clflags.keep_docs then
      List.filter is_not_doc x
    else x
  in
    if s.for_saving && not !Clflags.keep_locs
    then remove_loc.Ast_mapper.attributes remove_loc x
    else x

let rec module_path s = function
    Pident id as p ->
      begin try Tbl.find id s.modules with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      Papply(module_path s p1, module_path s p2)

let modtype_path s = function
    Pident id as p ->
      begin try
        match Tbl.find id s.modtypes with
          | Mty_ident p -> p
          | _ -> fatal_error "Subst.modtype_path"
      with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      fatal_error "Subst.modtype_path"

let type_path s = function
    Pident id as p ->
      begin try Tbl.find id s.types with Not_found -> p end
  | Pdot(p, n, pos) ->
      Pdot(module_path s p, n, pos)
  | Papply(p1, p2) ->
      fatal_error "Subst.type_path"

(* Special type ids for saved signatures *)

let new_id = ref (-1)
let reset_for_saving () = new_id := -1

let newpersty desc =
  decr new_id;
  { desc = desc; level = generic_level; id = !new_id }

(* ensure that all occurrences of 'Tvar None' are physically shared *)
let tvar_none = Tvar None
let tunivar_none = Tunivar None
let norm = function
  | Tvar None -> tvar_none
  | Tunivar None -> tunivar_none
  | d -> d

(* Similar to [Ctype.nondep_type_rec]. *)
let rec typexp s ty =
  let ty = repr ty in
  match ty.desc with
    Tvar _ | Tunivar _ as desc ->
      if s.for_saving || ty.id < 0 then
        let ty' =
          if s.for_saving then newpersty (norm desc)
          else newty2 ty.level desc
        in
        save_desc ty desc; ty.desc <- Tsubst ty'; ty'
      else ty
  | Tsubst ty ->
      ty
(* cannot do it, since it would omit subsitution
  | Tvariant row when not (static_row row) ->
      ty
*)
  | _ ->
    let desc = ty.desc in
    save_desc ty desc;
    (* Make a stub *)
    let ty' = if s.for_saving then newpersty (Tvar None) else newgenvar () in
    ty.desc <- Tsubst ty';
    ty'.desc <-
      begin match desc with
      | Tconstr(p, tl, abbrev) ->
          Tconstr(type_path s p, List.map (typexp s) tl, ref Mnil)
      | Tpackage(p, n, tl) ->
          Tpackage(modtype_path s p, n, List.map (typexp s) tl)
      | Tobject (t1, name) ->
          Tobject (typexp s t1,
                 ref (match !name with
                        None -> None
                      | Some (p, tl) ->
                          Some (type_path s p, List.map (typexp s) tl)))
      | Tfield (m, k, t1, t2)
        when s == identity && ty.level < generic_level && m = dummy_method ->
          (* not allowed to lower the level of the dummy method *)
          Tfield (m, k, t1, typexp s t2)
      | Tvariant row ->
          let row = row_repr row in
          let more = repr row.row_more in
          (* We must substitute in a subtle way *)
          (* Tsubst takes a tuple containing the row var and the variant *)
          begin match more.desc with
            Tsubst {desc = Ttuple [_;ty2]} ->
              (* This variant type has been already copied *)
              ty.desc <- Tsubst ty2; (* avoid Tlink in the new type *)
              Tlink ty2
          | _ ->
              let dup =
                s.for_saving || more.level = generic_level || static_row row ||
                match more.desc with Tconstr _ -> true | _ -> false in
              (* Various cases for the row variable *)
              let more' =
                match more.desc with
                  Tsubst ty -> ty
                | Tconstr _ | Tnil -> typexp s more
                | Tunivar _ | Tvar _ ->
                    save_desc more more.desc;
                    if s.for_saving then newpersty (norm more.desc) else
                    if dup && is_Tvar more then newgenty more.desc else more
                | _ -> assert false
              in
              (* Register new type first for recursion *)
              more.desc <- Tsubst(newgenty(Ttuple[more';ty']));
              (* Return a new copy *)
              let row =
                copy_row (typexp s) true row (not dup) more' in
              match row.row_name with
                Some (p, tl) ->
                  Tvariant {row with row_name = Some (type_path s p, tl)}
              | None ->
                  Tvariant row
          end
      | Tfield(label, kind, t1, t2) when field_kind_repr kind = Fabsent ->
          Tlink (typexp s t2)
      | _ -> copy_type_desc (typexp s) desc
      end;
    ty'

(*
   Always make a copy of the type. If this is not done, type levels
   might not be correct.
*)
let type_expr s ty =
  let ty' = typexp s ty in
  cleanup_types ();
  ty'

let type_declaration s decl =
  let decl =
    { type_params = List.map (typexp s) decl.type_params;
      type_arity = decl.type_arity;
      type_kind =
        begin match decl.type_kind with
          Type_abstract -> Type_abstract
        | Type_variant cstrs ->
            Type_variant
              (List.map
                 (fun c ->
                    {
                      cd_id = c.cd_id;
                      cd_args = List.map (typexp s) c.cd_args;
                      cd_res = may_map (typexp s) c.cd_res;
                      cd_loc = loc s c.cd_loc;
                      cd_attributes = attrs s c.cd_attributes;
                    }
                 )
                 cstrs)
        | Type_record(lbls, rep) ->
            Type_record
              (List.map (fun l ->
                   {
                     ld_id = l.ld_id;
                     ld_mutable = l.ld_mutable;
                     ld_type = typexp s l.ld_type;
                     ld_loc = loc s l.ld_loc;
                     ld_attributes = attrs s l.ld_attributes;
                   }
                 )
                  lbls,
               rep)
        | Type_open -> Type_open
        end;
      type_manifest =
        begin
          match decl.type_manifest with
            None -> None
          | Some ty -> Some(typexp s ty)
        end;
      type_private = decl.type_private;
      type_variance = decl.type_variance;
      type_newtype_level = None;
      type_loc = loc s decl.type_loc;
      type_attributes = attrs s decl.type_attributes;
    }
  in
  cleanup_types ();
  decl

let class_signature s sign =
  { csig_self = typexp s sign.csig_self;
    csig_vars =
      Vars.map (function (m, v, t) -> (m, v, typexp s t)) sign.csig_vars;
    csig_concr = sign.csig_concr;
    csig_inher =
      List.map (fun (p, tl) -> (type_path s p, List.map (typexp s) tl))
        sign.csig_inher;
  }

let rec class_type s =
  function
    Cty_constr (p, tyl, cty) ->
      Cty_constr (type_path s p, List.map (typexp s) tyl, class_type s cty)
  | Cty_signature sign ->
      Cty_signature (class_signature s sign)
  | Cty_arrow (l, ty, cty) ->
      Cty_arrow (l, typexp s ty, class_type s cty)

let class_declaration s decl =
  let decl =
    { cty_params = List.map (typexp s) decl.cty_params;
      cty_variance = decl.cty_variance;
      cty_type = class_type s decl.cty_type;
      cty_path = type_path s decl.cty_path;
      cty_new =
        begin match decl.cty_new with
          None    -> None
        | Some ty -> Some (typexp s ty)
        end;
      cty_loc = loc s decl.cty_loc;
      cty_attributes = attrs s decl.cty_attributes;
    }
  in
  (* Do not clean up if saving: next is cltype_declaration *)
  if not s.for_saving then cleanup_types ();
  decl

let cltype_declaration s decl =
  let decl =
    { clty_params = List.map (typexp s) decl.clty_params;
      clty_variance = decl.clty_variance;
      clty_type = class_type s decl.clty_type;
      clty_path = type_path s decl.clty_path;
      clty_loc = loc s decl.clty_loc;
      clty_attributes = attrs s decl.clty_attributes;
    }
  in
  (* Do clean up even if saving: type_declaration may be recursive *)
  cleanup_types ();
  decl

let class_type s cty =
  let cty = class_type s cty in
  cleanup_types ();
  cty

let value_description s descr =
  { val_type = type_expr s descr.val_type;
    val_kind = descr.val_kind;
    val_loc = loc s descr.val_loc;
    val_attributes = attrs s descr.val_attributes;
   }

let extension_constructor s ext =
  let ext =
    { ext_type_path = type_path s ext.ext_type_path;
      ext_type_params = List.map (typexp s) ext.ext_type_params;
      ext_args = List.map (typexp s) ext.ext_args;
      ext_ret_type = may_map (typexp s) ext.ext_ret_type;
      ext_private = ext.ext_private;
      ext_attributes = attrs s ext.ext_attributes;
      ext_loc = if s.for_saving then Location.none else ext.ext_loc; }
  in
    cleanup_types ();
    ext

let rec rename_bound_idents s idents = function
    [] -> (List.rev idents, s)
  | Sig_type(id, d, _) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_type id (Pident id') s) (id' :: idents) sg
  | Sig_module(id, mty, _) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_module id (Pident id') s) (id' :: idents) sg
  | Sig_modtype(id, d) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents (add_modtype id (Mty_ident(Pident id')) s)
                          (id' :: idents) sg
  | (Sig_value(id, _) | Sig_typext(id, _, _) |
     Sig_class(id, _, _) | Sig_class_type(id, _, _)) :: sg ->
      let id' = Ident.rename id in
      rename_bound_idents s (id' :: idents) sg

let rec modtype s = function
    Mty_ident p as mty ->
      begin match p with
        Pident id ->
          begin try Tbl.find id s.modtypes with Not_found -> mty end
      | Pdot(p, n, pos) ->
          Mty_ident(Pdot(module_path s p, n, pos))
      | Papply(p1, p2) ->
          fatal_error "Subst.modtype"
      end
  | Mty_signature sg ->
      Mty_signature(signature s sg)
  | Mty_functor(id, arg, res) ->
      let id' = Ident.rename id in
      Mty_functor(id', may_map (modtype s) arg,
                       modtype (add_module id (Pident id') s) res)
  | Mty_alias p ->
      Mty_alias(module_path s p)

and signature s sg =
  (* Components of signature may be mutually recursive (e.g. type declarations
     or class and type declarations), so first build global renaming
     substitution... *)
  let (new_idents, s') = rename_bound_idents s [] sg in
  (* ... then apply it to each signature component in turn *)
  List.map2 (signature_component s') sg new_idents

and signature_component s comp newid =
  match comp with
    Sig_value(id, d) ->
      Sig_value(newid, value_description s d)
  | Sig_type(id, d, rs) ->
      Sig_type(newid, type_declaration s d, rs)
  | Sig_typext(id, ext, es) ->
      Sig_typext(newid, extension_constructor s ext, es)
  | Sig_module(id, d, rs) ->
      Sig_module(newid, module_declaration s d, rs)
  | Sig_modtype(id, d) ->
      Sig_modtype(newid, modtype_declaration s d)
  | Sig_class(id, d, rs) ->
      Sig_class(newid, class_declaration s d, rs)
  | Sig_class_type(id, d, rs) ->
      Sig_class_type(newid, cltype_declaration s d, rs)

and module_declaration s decl =
  {
    md_type = modtype s decl.md_type;
    md_attributes = attrs s decl.md_attributes;
    md_loc = loc s decl.md_loc;
  }

and modtype_declaration s decl  =
  {
    mtd_type = may_map (modtype s) decl.mtd_type;
    mtd_attributes = attrs s decl.mtd_attributes;
    mtd_loc = loc s decl.mtd_loc;
  }

(* For every binding k |-> d of m1, add k |-> f d to m2
   and return resulting merged map. *)

let merge_tbls f m1 m2 =
  Tbl.fold (fun k d accu -> Tbl.add k (f d) accu) m1 m2

(* Composition of substitutions:
     apply (compose s1 s2) x = apply s2 (apply s1 x) *)

let compose s1 s2 =
  { types = merge_tbls (type_path s2) s1.types s2.types;
    modules = merge_tbls (module_path s2) s1.modules s2.modules;
    modtypes = merge_tbls (modtype s2) s1.modtypes s2.modtypes;
    for_saving = false }
