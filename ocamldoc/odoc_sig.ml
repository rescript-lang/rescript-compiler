(***********************************************************************)
(*                                                                     *)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Analysis of interface files. *)

open Misc
open Asttypes
open Types
open Typedtree
open Path

let print_DEBUG s = print_string s ; print_newline ();;

module Name = Odoc_name
open Odoc_parameter
open Odoc_value
open Odoc_type
open Odoc_extension
open Odoc_exception
open Odoc_class
open Odoc_module
open Odoc_types

module Signature_search =
  struct
    type ele =
      | M of string
      | MT of string
      | V of string
      | T of string
      | C of string
      | CT of string
      | X of string
      | P of string

    type tab = (ele, Types.signature_item) Hashtbl.t

    let add_to_hash table signat =
      match signat with
        Types.Sig_value (ident, _) ->
          Hashtbl.add table (V (Name.from_ident ident)) signat
      | Types.Sig_typext (ident, _, _) ->
          Hashtbl.add table (X (Name.from_ident ident)) signat
      | Types.Sig_type (ident, _, _) ->
          Hashtbl.add table (T (Name.from_ident ident)) signat
      | Types.Sig_class (ident, _, _) ->
          Hashtbl.add table (C (Name.from_ident ident)) signat
      | Types.Sig_class_type (ident, _, _) ->
          Hashtbl.add table (CT (Name.from_ident ident)) signat
      | Types.Sig_module (ident, _, _) ->
          Hashtbl.add table (M (Name.from_ident ident)) signat
      | Types.Sig_modtype (ident,_) ->
          Hashtbl.add table (MT (Name.from_ident ident)) signat

    let table signat =
      let t = Hashtbl.create 13 in
      List.iter (add_to_hash t) signat;
      t

    let search_value table name =
      match Hashtbl.find table (V name) with
      | (Types.Sig_value (_, val_desc)) ->  val_desc.Types.val_type
      | _ -> assert false

    let search_extension table name =
      match Hashtbl.find table (X name) with
      | (Types.Sig_typext (_, ext, _)) -> ext
      | _ -> assert false

    let search_type table name =
      match Hashtbl.find table (T name) with
      | (Types.Sig_type (_, type_decl, _)) -> type_decl
      | _ -> assert false

    let search_class table name =
      match Hashtbl.find table (C name) with
      | (Types.Sig_class (_, class_decl, _)) -> class_decl
      | _ -> assert false

    let search_class_type table name =
      match Hashtbl.find table (CT name) with
      | (Types.Sig_class_type (_, cltype_decl, _)) -> cltype_decl
      | _ -> assert false

    let search_module table name =
      match Hashtbl.find table (M name) with
      | (Types.Sig_module (ident, md, _)) -> md.Types.md_type
      | _ -> assert false

    let search_module_type table name =
      match Hashtbl.find table (MT name) with
      | (Types.Sig_modtype (_, {Types.mtd_type = Some module_type})) ->
          Some module_type
      | (Types.Sig_modtype (_, {Types.mtd_type = None})) ->
          None
      | _ -> assert false

    let search_attribute_type name class_sig =
      let (_, _, type_expr) = Types.Vars.find name class_sig.Types.csig_vars in
      type_expr

    let search_method_type name class_sig =
      let fields = Odoc_misc.get_fields class_sig.Types.csig_self in
      List.assoc name fields
  end

module type Info_retriever =
  sig
    val all_special : string -> string -> int * (Odoc_types.info list)
    val blank_line_outside_simple : string -> string -> bool
    val just_after_special : string -> string -> (int * Odoc_types.info option)
    val first_special : string -> string -> (int * Odoc_types.info option)
    val get_comments :
        (Odoc_types.text -> 'a) -> string -> string -> (Odoc_types.info option * 'a list)
  end

module Analyser =
  functor (My_ir : Info_retriever) ->
  struct
    (** This variable is used to load a file as a string and retrieve characters from it.*)
    let file = ref ""
    (** The name of the analysed file. *)
    let file_name = ref ""

    (** This function takes two indexes (start and end) and return the string
       corresponding to the indexes in the file global variable. The function
       prepare_file must have been called to fill the file global variable.*)
    let get_string_of_file the_start the_end =
      try
        let s = String.sub !file the_start (the_end-the_start) in
        s
      with
        Invalid_argument _ ->
          ""

    (** This function loads the given file in the file global variable,
       and sets file_name.*)
    let prepare_file f input_f =
      try
        let s = Odoc_misc.input_file_as_string input_f in
        file := s;
        file_name := f
      with
        e ->
          file := "";
          raise e

    (** The function used to get the comments in a class. *)
    let get_comments_in_class pos_start pos_end =
      My_ir.get_comments (fun t -> Class_comment t)
        !file_name
        (get_string_of_file pos_start pos_end)

    (** The function used to get the comments in a module. *)
    let get_comments_in_module pos_start pos_end =
      My_ir.get_comments (fun t -> Element_module_comment t)
        !file_name
        (get_string_of_file pos_start pos_end)

    let merge_infos = Odoc_merge.merge_info_opt Odoc_types.all_merge_options

    let name_comment_from_type_decl pos_end pos_limit ty_decl =
      match ty_decl.Parsetree.ptype_kind with
      | Parsetree.Ptype_abstract ->
        let open Parsetree in
        begin match ty_decl.ptype_manifest with
        | None -> (0, [])
        | Some core_ty ->
          begin match core_ty.ptyp_desc with
          | Ptyp_object (fields, _) ->
            let rec f = function
              | [] -> []
              | ("",_,_) :: _ ->
                (* Fields with no name have been eliminated previously. *)
                assert false

              | (name, _atts, ct) :: [] ->
                let pos = ct.Parsetree.ptyp_loc.Location.loc_end.Lexing.pos_cnum in
                let s = get_string_of_file pos pos_end in
                let (_,comment_opt) =  My_ir.just_after_special !file_name s in
                [name, comment_opt]
              | (name, _atts, ct) :: ((name2, _atts2, ct2) as ele2) :: q ->
                let pos = ct.Parsetree.ptyp_loc.Location.loc_end.Lexing.pos_cnum in
                let pos2 = ct2.Parsetree.ptyp_loc.Location.loc_start.Lexing.pos_cnum in
                let s = get_string_of_file pos pos2 in
                let (_,comment_opt) =  My_ir.just_after_special !file_name s in
                (name, comment_opt) :: (f (ele2 :: q))
            in
            let is_named_field field =
              match field with
              | ("",_,_) -> false
              | _ -> true
            in
            (0, f @@ List.filter is_named_field fields)

          | _ -> (0, [])
          end
        end

      | Parsetree.Ptype_variant cons_core_type_list_list ->
          let rec f acc cons_core_type_list_list =
            let open Parsetree in
            match cons_core_type_list_list with
              [] ->
                (0, acc)
            | pcd :: [] ->
                let s = get_string_of_file
                    pcd.pcd_loc.Location.loc_end.Lexing.pos_cnum
                    pos_limit
                in
                let (len, comment_opt) =  My_ir.just_after_special !file_name s in
                (len, acc @ [ (pcd.pcd_name.txt, comment_opt) ])
            | pcd :: (pcd2 :: _ as q) ->
                let pos_end_first = pcd.pcd_loc.Location.loc_end.Lexing.pos_cnum in
                let pos_start_second = pcd2.pcd_loc.Location.loc_start.Lexing.pos_cnum in
                let s = get_string_of_file pos_end_first pos_start_second in
                let (_,comment_opt) = My_ir.just_after_special !file_name  s in
                f (acc @ [pcd.pcd_name.txt, comment_opt]) q
          in
          f [] cons_core_type_list_list

      | Parsetree.Ptype_record name_mutable_type_list (* of (string * mutable_flag * core_type) list*) ->
          let open Parsetree in
          let rec f = function
              [] ->
                []
            | {pld_name=name; pld_type=ct} :: [] ->
                let pos = ct.Parsetree.ptyp_loc.Location.loc_end.Lexing.pos_cnum in
                let s = get_string_of_file pos pos_end in
                let (_,comment_opt) =  My_ir.just_after_special !file_name s in
                [name.txt, comment_opt]
            | {pld_name=name; pld_type=ct} :: ({pld_name=name2; pld_type=ct2} as ele2) :: q ->
                let pos = ct.Parsetree.ptyp_loc.Location.loc_end.Lexing.pos_cnum in
                let pos2 = ct2.Parsetree.ptyp_loc.Location.loc_start.Lexing.pos_cnum in
                let s = get_string_of_file pos pos2 in
                let (_,comment_opt) =  My_ir.just_after_special !file_name s in
                (name.txt, comment_opt) :: (f (ele2 :: q))
          in
          (0, f name_mutable_type_list)
      | Parsetree.Ptype_open ->
          (0, [])


    let manifest_structure env name_comment_list type_expr =
      match type_expr.desc with
      | Tobject (fields, _) ->
        let f (field_name, _, type_expr) =
          let comment_opt =
            try List.assoc field_name name_comment_list
            with Not_found -> None
          in {
            of_name = field_name ;
            of_type = Odoc_env.subst_type env type_expr ;
            of_text = comment_opt ;
          }
        in
        Object_type (List.map f @@ fst @@ Ctype.flatten_fields fields)
      | _ -> Other (Odoc_env.subst_type env type_expr)

    let get_type_kind env name_comment_list type_kind =
      match type_kind with
        Types.Type_abstract ->
          Odoc_type.Type_abstract
      | Types.Type_variant l ->
          let f {Types.cd_id=constructor_name;cd_args=type_expr_list;cd_res=ret_type} =
            let constructor_name = Ident.name constructor_name in
            let comment_opt =
              try List.assoc constructor_name name_comment_list
              with Not_found -> None
            in
            {
              vc_name = constructor_name ;
              vc_args = List.map (Odoc_env.subst_type env) type_expr_list ;
              vc_ret =  may_map (Odoc_env.subst_type env) ret_type;
              vc_text = comment_opt
            }
          in
          Odoc_type.Type_variant (List.map f l)

      | Types.Type_record (l, _) ->
          let f {Types.ld_id=field_name;ld_mutable=mutable_flag;ld_type=type_expr} =
            let field_name = Ident.name field_name in
            let comment_opt =
              try List.assoc field_name name_comment_list
              with Not_found -> None
            in
            {
              rf_name = field_name ;
              rf_mutable = mutable_flag = Mutable ;
              rf_type = Odoc_env.subst_type env type_expr ;
              rf_text = comment_opt
            }
          in
          Odoc_type.Type_record (List.map f l)

      | Types.Type_open ->
          Odoc_type.Type_open


    let erased_names_of_constraints constraints acc =
      List.fold_right (fun constraint_ acc ->
        match constraint_ with
        | Parsetree.Pwith_type _ | Parsetree.Pwith_module _ -> acc
        | Parsetree.Pwith_typesubst {Parsetree.ptype_name=s}
        | Parsetree.Pwith_modsubst (s, _) ->
          Name.Set.add s.txt acc)
        constraints acc

    let filter_out_erased_items_from_signature erased signature =
      if Name.Set.is_empty erased then signature
      else List.fold_right (fun sig_item acc ->
        let take_item psig_desc = { sig_item with Parsetree.psig_desc } :: acc in
        match sig_item.Parsetree.psig_desc with
        | Parsetree.Psig_attribute _
        | Parsetree.Psig_extension _
        | Parsetree.Psig_value _
        | Parsetree.Psig_typext _
        | Parsetree.Psig_exception _
        | Parsetree.Psig_open _
        | Parsetree.Psig_include _
        | Parsetree.Psig_class _
        | Parsetree.Psig_class_type _ as tp -> take_item tp
        | Parsetree.Psig_type types ->
          (match List.filter (fun td -> not (Name.Set.mem td.Parsetree.ptype_name.txt erased)) types with
          | [] -> acc
          | types -> take_item (Parsetree.Psig_type types))
        | Parsetree.Psig_module {Parsetree.pmd_name=name}
        | Parsetree.Psig_modtype {Parsetree.pmtd_name=name} as m ->
          if Name.Set.mem name.txt erased then acc else take_item m
        | Parsetree.Psig_recmodule mods ->
          (match List.filter (fun pmd -> not (Name.Set.mem pmd.Parsetree.pmd_name.txt erased)) mods with
          | [] -> acc
          | mods -> take_item (Parsetree.Psig_recmodule mods)))
        signature []

    (** Analysis of the elements of a class, from the information in the parsetree and in the class
       signature. @return the couple (inherited_class list, elements).*)
    let analyse_class_elements env current_class_name last_pos pos_limit
        class_type_field_list class_signature =
      let get_pos_limit2 q =
        match q with
          [] -> pos_limit
          | ele2 :: _ ->
              let loc = ele2.Parsetree.pctf_loc in
            match ele2.Parsetree.pctf_desc with
              Parsetree.Pctf_val (_, _, _, _)
            | Parsetree.Pctf_method (_, _, _, _)
            | Parsetree.Pctf_constraint (_, _)
            | Parsetree.Pctf_attribute _ -> loc.Location.loc_start.Lexing.pos_cnum
            | Parsetree.Pctf_inherit class_type ->
                class_type.Parsetree.pcty_loc.Location.loc_start.Lexing.pos_cnum
            | Parsetree.Pctf_extension _ -> assert false
      in
      let get_method name comment_opt private_flag loc q =
        let complete_name = Name.concat current_class_name name in
        let typ =
          try Signature_search.search_method_type name class_signature
          with Not_found ->
            raise (Failure (Odoc_messages.method_type_not_found current_class_name name))
        in
        let subst_typ = Odoc_env.subst_type env typ in
        let met =
          {
            met_value =
            {
              val_name = complete_name ;
              val_info = comment_opt ;
              val_type = subst_typ ;
              val_recursive = false ;
              val_parameters = Odoc_value.dummy_parameter_list subst_typ ;
              val_code = None ;
              val_loc = { loc_impl = None ; loc_inter = Some loc };
            } ;
            met_private = private_flag = Asttypes.Private ;
            met_virtual = false ;
          }
        in
        let pos_limit2 = get_pos_limit2 q in
        let pos_end = loc.Location.loc_end.Lexing.pos_cnum in
        let (maybe_more, info_after_opt) =
          My_ir.just_after_special
            !file_name
            (get_string_of_file pos_end pos_limit2)
        in
        met.met_value.val_info <- merge_infos met.met_value.val_info info_after_opt ;
        (* update the parameter description *)
        Odoc_value.update_value_parameters_text met.met_value;
        (met, maybe_more)
      in
      let rec f last_pos class_type_field_list =
        match class_type_field_list with
          [] ->
            let s = get_string_of_file last_pos pos_limit in
            let (_, ele_coms) = My_ir.all_special !file_name s in
            let ele_comments =
              List.fold_left
                (fun acc -> fun sc ->
                  match sc.Odoc_types.i_desc with
                    None ->
                      acc
                  | Some t ->
                      acc @ [Class_comment t])
                []
                ele_coms
            in
            ([], ele_comments)

          | item :: q ->
              let loc = item.Parsetree.pctf_loc in
              match item.Parsetree.pctf_desc with

        | Parsetree.Pctf_val (name, mutable_flag, virtual_flag, _) ->
            (* of (string * mutable_flag * core_type option * Location.t)*)
            let (comment_opt, eles_comments) = get_comments_in_class last_pos loc.Location.loc_start.Lexing.pos_cnum in
            let complete_name = Name.concat current_class_name name in
            let typ =
              try Signature_search.search_attribute_type name class_signature
              with Not_found ->
                raise (Failure (Odoc_messages.attribute_type_not_found current_class_name name))
            in
            let subst_typ = Odoc_env.subst_type env typ in
            let att =
              {
                att_value =
                {
                  val_name = complete_name ;
                  val_info = comment_opt ;
                  val_type = subst_typ;
                  val_recursive = false ;
                  val_parameters = [] ;
                  val_code = None ;
                  val_loc = { loc_impl = None ; loc_inter = Some loc} ;
                } ;
                att_mutable = mutable_flag = Asttypes.Mutable ;
                att_virtual = virtual_flag = Asttypes.Virtual ;
              }
            in
            let pos_limit2 = get_pos_limit2 q in
            let pos_end = loc.Location.loc_end.Lexing.pos_cnum in
            let (maybe_more, info_after_opt) =
              My_ir.just_after_special
                !file_name
                (get_string_of_file pos_end pos_limit2)
            in
            att.att_value.val_info <- merge_infos att.att_value.val_info info_after_opt ;
            let (inher_l, eles) = f (pos_end + maybe_more) q in
            (inher_l, eles_comments @ ((Class_attribute att) :: eles))

        | Parsetree.Pctf_method (name, private_flag, virtual_flag, _) ->
            (* of (string * private_flag * virtual_flag * core_type) *)
            let (comment_opt, eles_comments) = get_comments_in_class last_pos loc.Location.loc_start.Lexing.pos_cnum in
            let (met, maybe_more) = get_method name comment_opt private_flag loc q in
            let met2 =
              match virtual_flag with
              | Concrete -> met
              | Virtual -> { met with met_virtual = true }
            in
            let (inher_l, eles) = f (loc.Location.loc_end.Lexing.pos_cnum + maybe_more) q in
            (inher_l, eles_comments @ ((Class_method met2) :: eles))

        | (Parsetree.Pctf_constraint (_, _)) ->
            (* of (core_type * core_type) *)
            (* A VOIR : cela correspond aux contraintes, non ? on ne les garde pas pour l'instant *)
            let (comment_opt, eles_comments) = get_comments_in_class last_pos loc.Location.loc_start.Lexing.pos_cnum in
            let (inher_l, eles) = f loc.Location.loc_end.Lexing.pos_cnum q in
            (inher_l, eles_comments @ eles)

        | Parsetree.Pctf_inherit class_type ->
            let loc = class_type.Parsetree.pcty_loc in
            let (comment_opt, eles_comments) =
              get_comments_in_class last_pos loc.Location.loc_start.Lexing.pos_cnum
            in
            let pos_limit2 = get_pos_limit2 q in
            let pos_end = loc.Location.loc_end.Lexing.pos_cnum in
            let (maybe_more, info_after_opt) =
              My_ir.just_after_special
                !file_name
                (get_string_of_file pos_end pos_limit2)
            in
            let comment_opt2 = merge_infos comment_opt info_after_opt in
            let text_opt = match comment_opt2 with None -> None | Some i -> i.Odoc_types.i_desc in
            let inh  =
              match class_type.Parsetree.pcty_desc with
                Parsetree.Pcty_constr (longident, _) ->
                  (*of Longident.t * core_type list*)
                  let name = Name.from_longident longident.txt in
                  let ic =
                    {
                      ic_name = Odoc_env.full_class_or_class_type_name env name ;
                      ic_class = None ;
                      ic_text = text_opt ;
                    }
                  in
                  ic

              | Parsetree.Pcty_signature _
              | Parsetree.Pcty_arrow _ ->
                    (* we don't have a name for the class signature, so we call it "object ... end"  *)
                  {
                    ic_name = Odoc_messages.object_end ;
                    ic_class = None ;
                    ic_text = text_opt ;
                  }
              | Parsetree.Pcty_extension _ -> assert false
            in
            let (inher_l, eles) = f (pos_end + maybe_more) q in
            (inh :: inher_l , eles_comments @ eles)
        | Parsetree.Pctf_attribute _ ->
            let (comment_opt, eles_comments) = get_comments_in_class last_pos loc.Location.loc_start.Lexing.pos_cnum in
            let (inher_l, eles) = f loc.Location.loc_end.Lexing.pos_cnum q in
            (inher_l, eles_comments @ eles)

        | Parsetree.Pctf_extension _ -> assert false
      in
      f last_pos class_type_field_list

    (** Analyse of a .mli parse tree, to get the corresponding elements.
       last_pos is the position of the first character which may be used to look for special comments.
    *)
    let rec analyse_parsetree env signat current_module_name last_pos pos_limit sig_item_list =
      let table = Signature_search.table signat in
      (* we look for the comment of each item then analyse the item *)
      let rec f acc_eles acc_env last_pos = function
          [] ->
            let s = get_string_of_file last_pos pos_limit in
            let (_, ele_coms) = My_ir.all_special !file_name s in
            let ele_comments =
              List.fold_left
                (fun acc -> fun sc ->
                  match sc.Odoc_types.i_desc with
                    None ->
                      acc
                  | Some t ->
                      acc @ [Element_module_comment t])
                []
                ele_coms
            in
            acc_eles @ ele_comments

        | ele :: q ->
            let (assoc_com, ele_comments) =  get_comments_in_module
                last_pos
                ele.Parsetree.psig_loc.Location.loc_start.Lexing.pos_cnum
            in
            let (maybe_more, new_env, elements) = analyse_signature_item_desc
                acc_env
                signat
                table
                current_module_name
                ele.Parsetree.psig_loc
                ele.Parsetree.psig_loc.Location.loc_start.Lexing.pos_cnum
                ele.Parsetree.psig_loc.Location.loc_end.Lexing.pos_cnum
                (match q with
                  [] -> pos_limit
                | ele2 :: _ -> ele2.Parsetree.psig_loc.Location.loc_start.Lexing.pos_cnum
                )
                assoc_com
                ele.Parsetree.psig_desc
            in
            f (acc_eles @ (ele_comments @ elements))
              new_env
              (ele.Parsetree.psig_loc.Location.loc_end.Lexing.pos_cnum + maybe_more)
                   (* for the comments of constructors in types,
                      which are after the constructor definition and can
                      go beyond ele.Parsetree.psig_loc.Location.loc_end.Lexing.pos_cnum *)
              q
      in
      f [] env last_pos sig_item_list

    (** Analyse the given signature_item_desc to create the corresponding module element
       (with the given attached comment).*)
    and analyse_signature_item_desc env signat table current_module_name
        sig_item_loc pos_start_ele pos_end_ele pos_limit comment_opt sig_item_desc =
        match sig_item_desc with
          Parsetree.Psig_value value_desc ->
            let name_pre = value_desc.Parsetree.pval_name in
            let type_expr =
              try Signature_search.search_value table name_pre.txt
              with Not_found ->
                raise (Failure (Odoc_messages.value_not_found current_module_name name_pre.txt))
            in
            let name = Name.parens_if_infix name_pre.txt in
            let subst_typ = Odoc_env.subst_type env type_expr in
            let v =
              {
                val_name = Name.concat current_module_name name ;
                val_info = comment_opt ;
                val_type = subst_typ ;
                val_recursive = false ;
                val_parameters = Odoc_value.dummy_parameter_list subst_typ ;
                val_code = None ;
                val_loc = { loc_impl = None ; loc_inter = Some sig_item_loc } ;
              }
            in
            let (maybe_more, info_after_opt) =
              My_ir.just_after_special
                !file_name
                (get_string_of_file pos_end_ele pos_limit)
            in
            v.val_info <- merge_infos v.val_info info_after_opt ;
            (* update the parameter description *)
            Odoc_value.update_value_parameters_text v;

            let new_env = Odoc_env.add_value env v.val_name in
            (maybe_more, new_env, [ Element_value v ])

        | Parsetree.Psig_typext tyext ->
          let new_env, types_ext_list, last_ext =
            List.fold_left
              (fun (env_acc, exts_acc, _) -> fun {Parsetree.pext_name = { txt = name }} ->
                let complete_name = Name.concat current_module_name name in
                let env_acc = Odoc_env.add_extension env_acc complete_name in
                let types_ext =
                  try Signature_search.search_extension table name
                  with Not_found ->
                    raise (Failure (Odoc_messages.extension_not_found current_module_name name))
                in
                  env_acc, ((name, types_ext) :: exts_acc), Some types_ext
              )
              (env, [], None)
              tyext.Parsetree.ptyext_constructors
          in
          let ty_path, ty_params, priv =
            match last_ext with
              None -> assert false
            | Some ext -> ext.ext_type_path, ext.ext_type_params, ext.ext_private
          in
          let new_te =
            {
              te_info = comment_opt;
              te_type_name =
                Odoc_env.full_type_name new_env (Name.from_path ty_path);
              te_type_parameters =
                List.map (Odoc_env.subst_type new_env) ty_params;
              te_private = priv;
              te_constructors = [];
              te_loc = { loc_impl = None ; loc_inter = Some sig_item_loc} ;
              te_code =
                (
                  if !Odoc_global.keep_code then
                    Some (get_string_of_file pos_start_ele pos_end_ele)
                  else
                    None
                ) ;
            }
          in
          let rec analyse_extension_constructors maybe_more exts_acc types_ext_list =
            match types_ext_list with
              [] -> (maybe_more, List.rev exts_acc)
            | (name, types_ext) :: q ->
              let ext_loc_end =  types_ext.Types.ext_loc.Location.loc_end.Lexing.pos_cnum in
              let new_x =
                {
                  xt_name = Name.concat current_module_name name ;
                  xt_args = List.map (Odoc_env.subst_type new_env) types_ext.ext_args ;
                  xt_ret = may_map (Odoc_env.subst_type new_env) types_ext.ext_ret_type ;
                  xt_type_extension = new_te;
                  xt_alias = None ;
                  xt_loc = { loc_impl = None ; loc_inter = Some types_ext.Types.ext_loc} ;
                  xt_text = None;
                }
              in
              let pos_limit2 =
                match q with
                  [] -> pos_limit
                | (_, next) :: _ -> next.Types.ext_loc.Location.loc_start.Lexing.pos_cnum
              in
              let s = get_string_of_file ext_loc_end pos_limit2 in
              let (maybe_more, comment_opt) =  My_ir.just_after_special !file_name s in
                new_x.xt_text <- comment_opt;
                analyse_extension_constructors maybe_more (new_x :: exts_acc) q
          in
          let (maybe_more, exts) = analyse_extension_constructors 0 [] types_ext_list in
            new_te.te_constructors <- exts;
            let (maybe_more2, info_after_opt) =
              My_ir.just_after_special
                !file_name
                (get_string_of_file (pos_end_ele + maybe_more) pos_limit)
            in
              new_te.te_info <- merge_infos new_te.te_info info_after_opt ;
              (maybe_more + maybe_more2, new_env, [ Element_type_extension new_te ])

        | Parsetree.Psig_exception ext ->
            let name = ext.Parsetree.pext_name in
            let types_ext =
              try Signature_search.search_extension table name.txt
              with Not_found ->
                raise (Failure (Odoc_messages.exception_not_found current_module_name name.txt))
            in
            let e =
              {
                ex_name = Name.concat current_module_name name.txt ;
                ex_info = comment_opt ;
                ex_args = List.map (Odoc_env.subst_type env) types_ext.ext_args ;
                ex_ret = may_map (Odoc_env.subst_type env) types_ext.ext_ret_type ;
                ex_alias = None ;
                ex_loc = { loc_impl = None ; loc_inter = Some sig_item_loc } ;
                ex_code =
                   (
                    if !Odoc_global.keep_code then
                      Some (get_string_of_file pos_start_ele pos_end_ele)
                    else
                      None
                   ) ;
              }
            in
            let (maybe_more, info_after_opt) =
              My_ir.just_after_special
                !file_name
                (get_string_of_file pos_end_ele pos_limit)
            in
            e.ex_info <- merge_infos e.ex_info info_after_opt ;
            let new_env = Odoc_env.add_extension env e.ex_name in
            (maybe_more, new_env, [ Element_exception e ])

        | Parsetree.Psig_type name_type_decl_list ->
            let extended_env =
              List.fold_left
                (fun acc_env td ->
                  let complete_name = Name.concat current_module_name td.Parsetree.ptype_name.txt in
                  Odoc_env.add_type acc_env complete_name
                )
                env
                name_type_decl_list
            in
            let env =
              let is_nonrec =
                List.exists
                  (fun td ->
                     List.exists (fun (n, _) -> n.txt = "nonrec")
                       td.Parsetree.ptype_attributes)
                  name_type_decl_list
              in
              if is_nonrec then env else extended_env
            in
            let rec f ?(first=false) acc_maybe_more last_pos name_type_decl_list =
              match name_type_decl_list with
                [] ->
                  (acc_maybe_more, [])
              | type_decl :: q ->
                  let name = type_decl.Parsetree.ptype_name in
                  let (assoc_com, ele_comments) =
                    if first then
                      (comment_opt, [])
                    else
                      get_comments_in_module
                        last_pos
                        type_decl.Parsetree.ptype_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  let pos_limit2 =
                    match q with
                      [] -> pos_limit
                    | td :: _ -> td.Parsetree.ptype_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  let (maybe_more, name_comment_list) =
                    name_comment_from_type_decl
                      type_decl.Parsetree.ptype_loc.Location.loc_end.Lexing.pos_cnum
                      pos_limit2
                      type_decl
                  in
                  print_DEBUG ("Type "^name.txt^" : "^(match assoc_com with None -> "sans commentaire" | Some c -> Odoc_misc.string_of_info c));
                  let f_DEBUG (name, c_opt) = print_DEBUG ("constructor/field "^name^": "^(match c_opt with None -> "sans commentaire" | Some c -> Odoc_misc.string_of_info c)) in
                  List.iter f_DEBUG name_comment_list;
                  (* get the information for the type in the signature *)
                  let sig_type_decl =
                    try Signature_search.search_type table name.txt
                    with Not_found ->
                      raise (Failure (Odoc_messages.type_not_found current_module_name name.txt))
                  in
                  (* get the type kind with the associated comments *)
                  let type_kind = get_type_kind env name_comment_list sig_type_decl.Types.type_kind in
                  let loc_start = type_decl.Parsetree.ptype_loc.Location.loc_start.Lexing.pos_cnum in
                  let new_end = type_decl.Parsetree.ptype_loc.Location.loc_end.Lexing.pos_cnum + maybe_more in
                  (* associate the comments to each constructor and build the [Type.t_type] *)
                  let new_type =
                    {
                      ty_name = Name.concat current_module_name name.txt ;
                      ty_info = assoc_com ;
                      ty_parameters =
                        List.map2 (fun p v ->
                          let (co, cn) = Types.Variance.get_upper v in
                          (Odoc_env.subst_type env p,co, cn))
                        sig_type_decl.Types.type_params
                        sig_type_decl.Types.type_variance;
                      ty_kind = type_kind;
                      ty_private = sig_type_decl.Types.type_private;
                      ty_manifest =
                        begin match sig_type_decl.Types.type_manifest with
                        | None   -> None
                        | Some t ->
                          Some (manifest_structure env name_comment_list t)
                        end ;
                      ty_loc = { loc_impl = None ;  loc_inter = Some sig_item_loc } ;
                      ty_code =
                        (
                         if !Odoc_global.keep_code then
                           Some (get_string_of_file loc_start new_end)
                         else
                           None
                        ) ;
                    }
                  in
                  let (maybe_more2, info_after_opt) =
                    My_ir.just_after_special
                      !file_name
                      (get_string_of_file new_end pos_limit2)
                  in
                  new_type.ty_info <- merge_infos new_type.ty_info info_after_opt ;
                  let (new_maybe_more, eles) = f
                      (maybe_more + maybe_more2)
                      (new_end + maybe_more2)
                      q
                  in
                  (new_maybe_more, (ele_comments @ [Element_type new_type]) @ eles)
            in
            let (maybe_more, types) = f ~first: true 0 pos_start_ele name_type_decl_list in
            (maybe_more, extended_env, types)

        | Parsetree.Psig_open _ -> (* A VOIR *)
            let ele_comments = match comment_opt with
              None -> []
            | Some i ->
                match i.i_desc with
                  None -> []
                | Some t -> [Element_module_comment t]
            in
            (0, env, ele_comments)

        | Parsetree.Psig_module {Parsetree.pmd_name=name; pmd_type=module_type} ->
            let complete_name = Name.concat current_module_name name.txt in
            (* get the the module type in the signature by the module name *)
            let sig_module_type =
              try Signature_search.search_module table name.txt
              with Not_found ->
                raise (Failure (Odoc_messages.module_not_found current_module_name name.txt))
            in
            let module_kind = analyse_module_kind env complete_name module_type sig_module_type in
            let code_intf =
              if !Odoc_global.keep_code then
                let loc = module_type.Parsetree.pmty_loc in
                let st = loc.Location.loc_start.Lexing.pos_cnum in
                let en = loc.Location.loc_end.Lexing.pos_cnum in
                Some (get_string_of_file st en)
              else
                None
            in
            let new_module =
              {
                m_name = complete_name ;
                m_type = sig_module_type;
                m_info = comment_opt ;
                m_is_interface = true ;
                m_file = !file_name ;
                m_kind = module_kind ;
                m_loc = { loc_impl = None ; loc_inter = Some sig_item_loc } ;
                m_top_deps = [] ;
                m_code = None ;
                m_code_intf = code_intf ;
                m_text_only = false ;
              }
            in
            let (maybe_more, info_after_opt) =
              My_ir.just_after_special
                !file_name
                (get_string_of_file pos_end_ele pos_limit)
            in
            new_module.m_info <- merge_infos new_module.m_info info_after_opt ;
            let new_env = Odoc_env.add_module env new_module.m_name in
            let new_env2 =
              match new_module.m_type with (* A VOIR : cela peut-il etre Tmty_ident ? dans ce cas, on aurait pas la signature *)
                Types.Mty_signature s -> Odoc_env.add_signature new_env new_module.m_name ~rel: (Name.simple new_module.m_name) s
              | _ -> new_env
            in
            (maybe_more, new_env2, [ Element_module new_module ])

        | Parsetree.Psig_recmodule decls ->
            (* we start by extending the environment *)
            let new_env =
              List.fold_left
                (fun acc_env {Parsetree.pmd_name={txt=name}} ->
                  let complete_name = Name.concat current_module_name name in
                  let e = Odoc_env.add_module acc_env complete_name in
                  (* get the information for the module in the signature *)
                  let sig_module_type =
                    try Signature_search.search_module table name
                    with Not_found ->
                      raise (Failure (Odoc_messages.module_not_found current_module_name name))
                  in
                  match sig_module_type with
                    (* A VOIR : cela peut-il etre Tmty_ident ? dans ce cas, on aurait pas la signature *)
                    Types.Mty_signature s ->
                      Odoc_env.add_signature e complete_name ~rel: name s
                  | _ ->
                      print_DEBUG "not a Tmty_signature";
                      e
                )
                env
                decls
            in
            let rec f ?(first=false) acc_maybe_more last_pos name_mtype_list =
              match name_mtype_list with
                [] ->
                  (acc_maybe_more, [])
              | {Parsetree.pmd_name=name; pmd_type=modtype} :: q ->
                  let complete_name = Name.concat current_module_name name.txt in
                  let loc = modtype.Parsetree.pmty_loc in
                  let loc_start = loc.Location.loc_start.Lexing.pos_cnum in
                  let loc_end = loc.Location.loc_end.Lexing.pos_cnum in
                  let (assoc_com, ele_comments) =
                    if first then
                      (comment_opt, [])
                    else
                      get_comments_in_module
                        last_pos
                        loc_start
                  in
                  let pos_limit2 =
                    match q with
                      [] -> pos_limit
                    | _ :: _ -> loc.Location.loc_start.Lexing.pos_cnum
                  in
                  (* get the information for the module in the signature *)
                  let sig_module_type =
                    try Signature_search.search_module table name.txt
                    with Not_found ->
                      raise (Failure (Odoc_messages.module_not_found current_module_name name.txt))
                  in
                  (* associate the comments to each constructor and build the [Type.t_type] *)
                  let module_kind = analyse_module_kind new_env complete_name modtype sig_module_type in
                  let code_intf =
                    if !Odoc_global.keep_code then
                      let st = loc.Location.loc_start.Lexing.pos_cnum in
                      let en = loc.Location.loc_end.Lexing.pos_cnum in
                      Some (get_string_of_file st en)
                    else
                      None
                  in
                  let new_module =
                    {
                      m_name = complete_name ;
                      m_type = sig_module_type;
                      m_info = assoc_com ;
                      m_is_interface = true ;
                      m_file = !file_name ;
                      m_kind = module_kind ;
                      m_loc = { loc_impl = None ; loc_inter = Some loc } ;
                      m_top_deps = [] ;
                      m_code = None ;
                      m_code_intf = code_intf ;
                      m_text_only = false ;
                    }
                  in
                  let (maybe_more, info_after_opt) =
                    My_ir.just_after_special
                      !file_name
                      (get_string_of_file loc_end pos_limit2)
                  in
                  new_module.m_info <- merge_infos new_module.m_info info_after_opt ;

                  let (maybe_more2, eles) = f
                      maybe_more
                      (loc_end + maybe_more)
                      q
                  in
                  (maybe_more2, (ele_comments @ [Element_module new_module]) @ eles)
            in
            let (maybe_more, mods) = f ~first: true 0 pos_start_ele decls in
            (maybe_more, new_env, mods)

        | Parsetree.Psig_modtype {Parsetree.pmtd_name=name; pmtd_type=pmodtype_decl} ->
            let complete_name = Name.concat current_module_name name.txt in
            let sig_mtype =
              try Signature_search.search_module_type table name.txt
              with Not_found ->
                raise (Failure (Odoc_messages.module_type_not_found current_module_name name.txt))
            in
            let module_type_kind =
              match pmodtype_decl with
                None -> None
              | Some module_type ->
                match sig_mtype with
                | Some sig_mtype -> Some (analyse_module_type_kind env complete_name module_type sig_mtype)
                | None -> None
            in

            let mt =
              {
                mt_name = complete_name ;
                mt_info = comment_opt ;
                mt_type = sig_mtype ;
                mt_is_interface = true ;
                mt_file = !file_name ;
                mt_kind = module_type_kind ;
                mt_loc = { loc_impl = None ; loc_inter = Some sig_item_loc } ;
              }
            in
            let (maybe_more, info_after_opt) =
              My_ir.just_after_special
                !file_name
                (get_string_of_file pos_end_ele pos_limit)
            in
            mt.mt_info <- merge_infos mt.mt_info info_after_opt ;
            let new_env = Odoc_env.add_module_type env mt.mt_name in
            let new_env2 =
              match sig_mtype with (* A VOIR : cela peut-il etre Tmty_ident ? dans ce cas, on aurait pas la signature *)
                Some (Types.Mty_signature s) -> Odoc_env.add_signature new_env mt.mt_name ~rel: (Name.simple mt.mt_name) s
              | _ -> new_env
            in
            (maybe_more, new_env2, [ Element_module_type mt ])

        | Parsetree.Psig_include incl ->
            let rec f = function
                Parsetree.Pmty_ident longident ->
                  Name.from_longident longident.txt
              | Parsetree.Pmty_alias longident ->
                  Name.from_longident longident.txt
              | Parsetree.Pmty_signature _ ->
                  "??"
              | Parsetree.Pmty_functor _ ->
                  "??"
              | Parsetree.Pmty_with (mt, _) ->
                  f mt.Parsetree.pmty_desc
              | Parsetree.Pmty_typeof mexpr ->
                  begin match mexpr.Parsetree.pmod_desc with
                    Parsetree.Pmod_ident longident -> Name.from_longident longident.txt
                  | _ -> "??"
                  end
              | Parsetree.Pmty_extension _ -> assert false
            in
            let name = f incl.Parsetree.pincl_mod.Parsetree.pmty_desc in
            let full_name = Odoc_env.full_module_or_module_type_name env name in
            let im =
              {
                im_name = full_name ;
                im_module = None ;
                im_info = comment_opt;
              }
            in
            (0, env, [ Element_included_module im ]) (* A VOIR : etendre l'environnement ? avec quoi ? *)

        | Parsetree.Psig_class class_description_list ->
            (* we start by extending the environment *)
            let new_env =
              List.fold_left
                (fun acc_env -> fun class_desc ->
                  let complete_name = Name.concat current_module_name class_desc.Parsetree.pci_name.txt in
                  Odoc_env.add_class acc_env complete_name
                )
                env
                class_description_list
            in
            let rec f ?(first=false) acc_maybe_more last_pos class_description_list =
              match class_description_list with
                [] ->
                  (acc_maybe_more, [])
              | class_desc :: q ->
                  let (assoc_com, ele_comments) =
                    if first then
                      (comment_opt, [])
                    else
                      get_comments_in_module
                        last_pos
                        class_desc.Parsetree.pci_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  let pos_end = class_desc.Parsetree.pci_loc.Location.loc_end.Lexing.pos_cnum in
                  let pos_limit2 =
                    match q with
                      [] -> pos_limit
                    | cd :: _ -> cd.Parsetree.pci_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  let name = class_desc.Parsetree.pci_name in
                  let complete_name = Name.concat current_module_name name.txt in
                  let sig_class_decl =
                    try Signature_search.search_class table name.txt
                    with Not_found ->
                      raise (Failure (Odoc_messages.class_not_found current_module_name name.txt))
                  in
                  let sig_class_type = sig_class_decl.Types.cty_type in
                  let (parameters, class_kind) =
                    analyse_class_kind
                     new_env
                     complete_name
                     class_desc.Parsetree.pci_loc.Location.loc_start.Lexing.pos_cnum
                     class_desc.Parsetree.pci_expr
                     sig_class_type
                 in
                 let new_class =
                   {
                     cl_name = complete_name ;
                     cl_info = assoc_com ;
                     cl_type = Odoc_env.subst_class_type env sig_class_type ;
                     cl_type_parameters = sig_class_decl.Types.cty_params;
                     cl_virtual = class_desc.Parsetree.pci_virt = Asttypes.Virtual ;
                     cl_kind = class_kind ;
                     cl_parameters = parameters ;
                     cl_loc = { loc_impl = None ; loc_inter = Some class_desc.Parsetree.pci_loc } ;
                   }
                 in
                 let (maybe_more, info_after_opt) =
                   My_ir.just_after_special
                     !file_name
                     (get_string_of_file pos_end pos_limit2)
                 in
                 new_class.cl_info <- merge_infos new_class.cl_info info_after_opt ;
                 Odoc_class.class_update_parameters_text new_class ;
                 let (new_maybe_more, eles) =
                   f maybe_more (pos_end + maybe_more) q
                 in
                 (new_maybe_more,
                  ele_comments @ (( Element_class new_class ) :: eles))
            in
            let (maybe_more, eles) =
              f ~first: true 0 pos_start_ele class_description_list
            in
            (maybe_more, new_env, eles)

        | Parsetree.Psig_class_type class_type_declaration_list ->
            (* we start by extending the environment *)
            let new_env =
              List.fold_left
                (fun acc_env -> fun class_type_decl ->
                  let complete_name = Name.concat current_module_name class_type_decl.Parsetree.pci_name.txt in
                  Odoc_env.add_class_type acc_env complete_name
                )
                env
                class_type_declaration_list
            in
            let rec f ?(first=false) acc_maybe_more last_pos class_type_description_list =
              match class_type_description_list with
                [] ->
                  (acc_maybe_more, [])
              | ct_decl :: q ->
                  let (assoc_com, ele_comments) =
                    if first then
                      (comment_opt, [])
                    else
                      get_comments_in_module
                        last_pos
                        ct_decl.Parsetree.pci_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  let pos_end = ct_decl.Parsetree.pci_loc.Location.loc_end.Lexing.pos_cnum in
                  let pos_limit2 =
                    match q with
                      [] -> pos_limit
                    | ct_decl2 :: _ -> ct_decl2.Parsetree.pci_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  let name = ct_decl.Parsetree.pci_name in
                  let complete_name = Name.concat current_module_name name.txt in
                  let sig_cltype_decl =
                    try Signature_search.search_class_type table name.txt
                    with Not_found ->
                      raise (Failure (Odoc_messages.class_type_not_found current_module_name name.txt))
                  in
                  let sig_class_type = sig_cltype_decl.Types.clty_type in
                  let kind = analyse_class_type_kind
                      new_env
                      complete_name
                      ct_decl.Parsetree.pci_loc.Location.loc_start.Lexing.pos_cnum
                      ct_decl.Parsetree.pci_expr
                      sig_class_type
                  in
                  let ct =
                    {
                      clt_name = complete_name ;
                      clt_info = assoc_com ;
                      clt_type = Odoc_env.subst_class_type env sig_class_type ;
                      clt_type_parameters = sig_cltype_decl.clty_params ;
                      clt_virtual = ct_decl.Parsetree.pci_virt = Asttypes.Virtual ;
                      clt_kind = kind ;
                      clt_loc = { loc_impl = None ; loc_inter = Some ct_decl.Parsetree.pci_loc } ;
                    }
                  in
                  let (maybe_more, info_after_opt) =
                    My_ir.just_after_special
                      !file_name
                      (get_string_of_file pos_end pos_limit2)
                  in
                  ct.clt_info <- merge_infos ct.clt_info info_after_opt ;
                  let (new_maybe_more, eles) =
                    f maybe_more (pos_end + maybe_more) q
                  in
                 (new_maybe_more,
                  ele_comments @ (( Element_class_type ct) :: eles))
            in
            let (maybe_more, eles) =
              f ~first: true 0 pos_start_ele class_type_declaration_list
            in
            (maybe_more, new_env, eles)
        | Parsetree.Psig_attribute _
        | Parsetree.Psig_extension _ ->
            (0, env, [])

    (** Return a module_type_kind from a Parsetree.module_type and a Types.module_type *)
    and analyse_module_type_kind
      ?(erased = Name.Set.empty) env current_module_name module_type sig_module_type =
      match module_type.Parsetree.pmty_desc with
        Parsetree.Pmty_ident longident ->
          let name =
            match sig_module_type with
              Types.Mty_ident path -> Name.from_path path
            | _ -> Name.from_longident longident.txt
              (* A VOIR cela arrive quand on fait module type F : functor ... -> Toto, Toto n'est pas un ident mais une structure *)
          in
          Module_type_alias { mta_name = Odoc_env.full_module_type_name env name ;
                              mta_module = None }

      | Parsetree.Pmty_alias longident ->
          let name =
            match sig_module_type with
              Types.Mty_alias path -> Name.from_path path
            | _ -> Name.from_longident longident.txt
          in
          (* Wrong naming... *)
          Module_type_alias { mta_name = Odoc_env.full_module_name env name ;
                              mta_module = None }

      | Parsetree.Pmty_signature ast ->
          (
           let ast = filter_out_erased_items_from_signature erased ast in
           (* we must have a signature in the module type *)
           match sig_module_type with
             Types.Mty_signature signat ->
               let pos_start = module_type.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum in
               let pos_end = module_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
               let elements = analyse_parsetree env signat current_module_name pos_start pos_end ast in
               Module_type_struct elements
           | _ ->
               raise (Failure "Parsetree.Pmty_signature signature but not Types.Mty_signature signat")
          )

      | Parsetree.Pmty_functor (_, pmodule_type2, module_type2) ->
          (
           let loc = match pmodule_type2 with None -> Location.none
                     | Some pmty -> pmty.Parsetree.pmty_loc in
           let loc_start = loc.Location.loc_start.Lexing.pos_cnum in
           let loc_end = loc.Location.loc_end.Lexing.pos_cnum in
           let mp_type_code = get_string_of_file loc_start loc_end in
           print_DEBUG (Printf.sprintf "mp_type_code=%s" mp_type_code);
           match sig_module_type with
             Types.Mty_functor (ident, param_module_type, body_module_type) ->
               let mp_kind =
                 match pmodule_type2, param_module_type with
                   Some pmty, Some mty ->
                     analyse_module_type_kind env current_module_name pmty mty
                 | _ -> Module_type_struct []
               in
               let param =
                 {
                   mp_name = Name.from_ident ident ;
                   mp_type =
                    Misc.may_map (Odoc_env.subst_module_type env)
                      param_module_type;
                   mp_type_code = mp_type_code ;
                   mp_kind = mp_kind ;
                 }
               in
               let k = analyse_module_type_kind ~erased env
                   current_module_name
                   module_type2
                   body_module_type
               in
               Module_type_functor (param, k)

           | _ ->
               (* if we're here something's wrong *)
               raise (Failure "Parsetree.Pmty_functor _ but not Types.Mty_functor _")
          )

      | Parsetree.Pmty_with (module_type2, constraints) ->
          (* of module_type * (Longident.t * with_constraint) list *)
          (
           let loc_start = module_type2.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let loc_end = module_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let s = get_string_of_file loc_start loc_end in
           let erased = erased_names_of_constraints constraints erased in
           let k = analyse_module_type_kind ~erased env current_module_name module_type2 sig_module_type in

           Module_type_with (k, s)
          )

      | Parsetree.Pmty_typeof module_expr ->
          let loc_start = module_expr.Parsetree.pmod_loc.Location.loc_start.Lexing.pos_cnum in
          let loc_end = module_expr.Parsetree.pmod_loc.Location.loc_end.Lexing.pos_cnum in
          let s = get_string_of_file loc_start loc_end in
          Module_type_typeof s

      | Parsetree.Pmty_extension _ -> assert false

    (** analyse of a Parsetree.module_type and a Types.module_type.*)
    and analyse_module_kind
        ?(erased = Name.Set.empty) env current_module_name module_type sig_module_type =
      match module_type.Parsetree.pmty_desc with
      | Parsetree.Pmty_ident longident ->
          let k = analyse_module_type_kind env current_module_name module_type sig_module_type in
          Module_with ( k, "" )
      | Parsetree.Pmty_alias longident ->
          begin
            match sig_module_type with
              Types.Mty_alias path ->
                let alias_name = Odoc_env.full_module_name env (Name.from_path path) in
                let ma = { ma_name = alias_name ; ma_module = None } in
                Module_alias ma
            | _ ->
              raise (Failure "Parsetree.Pmty_alias _ but not Types.Mty_alias _")
           end
      | Parsetree.Pmty_signature signature ->
          (
           let signature = filter_out_erased_items_from_signature erased signature in
           match sig_module_type with
             Types.Mty_signature signat ->
               Module_struct
                 (analyse_parsetree
                    env
                    signat
                    current_module_name
                    module_type.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum
                    module_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum
                    signature
                 )
           | _ ->
               (* if we're here something's wrong *)
               raise (Failure "Parsetree.Pmty_signature signature but not Types.Mty_signature signat")
          )
      | Parsetree.Pmty_functor (_, pmodule_type2,module_type2) (* of string * module_type * module_type *) ->
          (
           match sig_module_type with
             Types.Mty_functor (ident, param_module_type, body_module_type) ->
               let loc = match pmodule_type2 with None -> Location.none
                     | Some pmty -> pmty.Parsetree.pmty_loc in
               let loc_start = loc.Location.loc_start.Lexing.pos_cnum in
               let loc_end = loc.Location.loc_end.Lexing.pos_cnum in
               let mp_type_code = get_string_of_file loc_start loc_end in
               print_DEBUG (Printf.sprintf "mp_type_code=%s" mp_type_code);
               let mp_kind =
                 match pmodule_type2, param_module_type with
                   Some pmty, Some mty ->
                     analyse_module_type_kind env current_module_name pmty mty
                 | _ -> Module_type_struct []
               in
               let param =
                 {
                   mp_name = Name.from_ident ident ;
                   mp_type = Misc.may_map
                    (Odoc_env.subst_module_type env) param_module_type ;
                   mp_type_code = mp_type_code ;
                   mp_kind = mp_kind ;
                 }
               in
               let k = analyse_module_kind ~erased env
                   current_module_name
                   module_type2
                   body_module_type
               in
               Module_functor (param, k)

           | _ ->
               (* if we're here something's wrong *)
               raise (Failure "Parsetree.Pmty_functor _ but not Types.Mty_functor _")
          )
      | Parsetree.Pmty_with (module_type2, constraints) ->
          (*of module_type * (Longident.t * with_constraint) list*)
          (
           let loc_start = module_type2.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let loc_end = module_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
           let s = get_string_of_file loc_start loc_end in
           let erased = erased_names_of_constraints constraints erased in
           let k = analyse_module_type_kind ~erased env current_module_name module_type2 sig_module_type in
           Module_with (k, s)
          )
      | Parsetree.Pmty_typeof module_expr ->
          let loc_start = module_expr.Parsetree.pmod_loc.Location.loc_start.Lexing.pos_cnum in
          let loc_end = module_expr.Parsetree.pmod_loc.Location.loc_end.Lexing.pos_cnum in
          let s = get_string_of_file loc_start loc_end in
          Module_typeof s

      | Parsetree.Pmty_extension _ -> assert false


    (** Analyse of a Parsetree.class_type and a Types.class_type to return a couple
       (class parameters, class_kind).*)
    and analyse_class_kind env current_class_name last_pos parse_class_type sig_class_type =
      match parse_class_type.Parsetree.pcty_desc, sig_class_type with
        (Parsetree.Pcty_constr (_, _) (*of Longident.t * core_type list *),
         Types.Cty_constr (p, typ_list, _) (*of Path.t * type_expr list * class_type*)) ->
          print_DEBUG "Cty_constr _";
           let path_name = Name.from_path p in
           let name = Odoc_env.full_class_or_class_type_name env path_name in
           let k =
             Class_constr
               {
                 cco_name = name ;
                 cco_class = None ;
                 cco_type_parameters = List.map (Odoc_env.subst_type env) typ_list
               }
           in
           ([], k)

      | (Parsetree.Pcty_signature { Parsetree.pcsig_fields = class_type_field_list }, Types.Cty_signature class_signature) ->
          (* we get the elements of the class in class_type_field_list *)
          let (inher_l, ele) = analyse_class_elements env current_class_name
              last_pos
              parse_class_type.Parsetree.pcty_loc.Location.loc_end.Lexing.pos_cnum
              class_type_field_list
              class_signature
          in
          ([], Class_structure (inher_l, ele))

      | (Parsetree.Pcty_arrow (parse_label, _, pclass_type), Types.Cty_arrow (label, type_expr, class_type)) ->
          (* label = string. Dans les signatures, pas de nom de parametres a l'interieur des tuples *)
          (* si label = "", pas de label. ici on a l'information pour savoir si on a un label explicite. *)
          if parse_label = label then
            (
             let new_param = Simple_name
                 {
                   sn_name = Btype.label_name label ;
                   sn_type = Odoc_env.subst_type env type_expr ;
                   sn_text = None ; (* will be updated when the class will be created *)
                 }
             in
             let (l, k) = analyse_class_kind env current_class_name last_pos pclass_type class_type in
             ( (new_param :: l), k )
            )
          else
            (
             raise (Failure "Parsetree.Pcty_arrow (parse_label, _, pclass_type), labels differents")
            )

      | _ ->
          raise (Failure "analyse_class_kind pas de correspondance dans le match")

    (** Analyse of a Parsetree.class_type and a Types.class_type to return a class_type_kind.*)
    and analyse_class_type_kind env current_class_name last_pos parse_class_type sig_class_type =
      match parse_class_type.Parsetree.pcty_desc, sig_class_type with
        (Parsetree.Pcty_constr (_, _) (*of Longident.t * core_type list *),
         Types.Cty_constr (p, typ_list, _) (*of Path.t * type_expr list * class_type*)) ->
          print_DEBUG "Cty_constr _";
           let k =
             Class_type
               {
                 cta_name = Odoc_env.full_class_or_class_type_name env (Name.from_path p) ;
                 cta_class = None ;
                 cta_type_parameters = List.map (Odoc_env.subst_type env) typ_list
               }
           in
           k

        | (Parsetree.Pcty_signature {
              Parsetree.pcsig_fields = class_type_field_list;
              }, Types.Cty_signature class_signature) ->
          (* we get the elements of the class in class_type_field_list *)
          let (inher_l, ele) = analyse_class_elements env current_class_name
              last_pos
              parse_class_type.Parsetree.pcty_loc.Location.loc_end.Lexing.pos_cnum
              class_type_field_list
              class_signature
          in
          Class_signature (inher_l, ele)

      | (Parsetree.Pcty_arrow (parse_label, _, pclass_type), Types.Cty_arrow (label, type_expr, class_type)) ->
          raise (Failure "analyse_class_type_kind : Parsetree.Pcty_arrow (...) with Types.Cty_arrow (...)")
(*
      | (Parsetree.Pcty_constr (longident, _) (*of Longident.t * core_type list *),
         Types.Cty_signature class_signature) ->
           (* A VOIR : c'est pour le cas des contraintes de classes :
              class type cons = object
                method m : int
              end

              class ['a] maxou x =
                (object
                  val a = (x : 'a)
                  method m = a
                end : cons )
                    ^^^^^^
           *)
           let k =
             Class_type
               {
                 cta_name = Odoc_env.full_class_name env (Name.from_longident longident) ;
                 cta_class = None ;
                 cta_type_parameters = List.map (Odoc_env.subst_type env) typ_list (* ?? *)
               }
           in
           ([], k)
*)
      | _ ->
          raise (Failure "analyse_class_type_kind pas de correspondance dans le match")

    let analyse_signature source_file input_file
        (ast : Parsetree.signature) (signat : Types.signature) =
      let complete_source_file =
        try
          let curdir = Sys.getcwd () in
          let (dirname, basename) = (Filename.dirname source_file, Filename.basename source_file) in
          Sys.chdir dirname ;
          let complete = Filename.concat (Sys.getcwd ()) basename in
          Sys.chdir curdir ;
          complete
        with
          Sys_error s ->
            prerr_endline s ;
            incr Odoc_global.errors ;
            source_file
      in
      prepare_file complete_source_file input_file;
      (* We create the t_module for this file. *)
      let mod_name = String.capitalize
          (Filename.basename (try Filename.chop_extension source_file with _ -> source_file))
      in
      let (len,info_opt) = My_ir.first_special !file_name !file in
      let elements =
        analyse_parsetree Odoc_env.empty signat mod_name len (String.length !file) ast
      in
      let code_intf =
        if !Odoc_global.keep_code then
          Some !file
        else
          None
      in
      {
        m_name = mod_name ;
        m_type = Types.Mty_signature signat ;
        m_info = info_opt ;
        m_is_interface = true ;
        m_file = !file_name ;
        m_kind = Module_struct elements ;
        m_loc = { loc_impl = None ; loc_inter = Some (Location.in_file !file_name) } ;
        m_top_deps = [] ;
        m_code = None ;
        m_code_intf = code_intf ;
        m_text_only = false ;
      }

    end
