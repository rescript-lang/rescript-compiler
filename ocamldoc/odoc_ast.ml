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

(** Analysis of implementation files. *)
open Misc
open Asttypes
open Types
open Typedtree

let print_DEBUG3 s = print_string s ; print_newline ();;
let print_DEBUG s = print_string s ; print_newline ();;

type typedtree = (Typedtree.structure * Typedtree.module_coercion)

module Name = Odoc_name
open Odoc_parameter
open Odoc_value
open Odoc_type
open Odoc_extension
open Odoc_exception
open Odoc_class
open Odoc_module
open Odoc_types

(** This variable contains the regular expression representing a blank.*)
let blank = "[ \010\013\009\012']"

(** This variable contains the regular expression representing a blank but not a '\n'.*)
let simple_blank = "[ \013\009\012]"

(** This module is used to search for structure items by name in a Typedtree.structure.
   One function creates two hash tables, which can then be used to search for elements.
   Class elements do not use tables.
*)
module Typedtree_search =
  struct
    type ele =
      | M of string
      | MT of string
      | T of string
      | C of string
      | CT of string
      | X of string
      | E of string
      | P of string
      | IM of string

    type tab = (ele, Typedtree.structure_item_desc) Hashtbl.t
    type tab_values = (Odoc_module.Name.t, Typedtree.pattern * Typedtree.expression) Hashtbl.t

    let iter_val_pattern = function
      | Typedtree.Tpat_any -> None
      | Typedtree.Tpat_var (name, _) -> Some (Name.from_ident name)
      | Typedtree.Tpat_tuple _ -> None (* A VOIR quand on traitera les tuples *)
      | _ -> None

    let add_to_hashes table table_values tt =
      match tt with
      | Typedtree.Tstr_module mb ->
          Hashtbl.add table (M (Name.from_ident mb.mb_id)) tt
      | Typedtree.Tstr_recmodule mods ->
          List.iter
            (fun mb ->
              Hashtbl.add table (M (Name.from_ident mb.mb_id))
                (Typedtree.Tstr_module mb)
            )
            mods
      | Typedtree.Tstr_modtype mtd ->
          Hashtbl.add table (MT (Name.from_ident mtd.mtd_id)) tt
      | Typedtree.Tstr_typext te -> begin
          match te.tyext_constructors with
            [] -> assert false
          | ext :: _ -> Hashtbl.add table (X (Name.from_ident ext.ext_id)) tt
        end
      | Typedtree.Tstr_exception ext ->
          Hashtbl.add table (E (Name.from_ident ext.ext_id)) tt
      | Typedtree.Tstr_type ident_type_decl_list ->
          List.iter
            (fun td ->
              Hashtbl.add table (T (Name.from_ident td.typ_id))
                (Typedtree.Tstr_type [td]))
            ident_type_decl_list
      | Typedtree.Tstr_class info_list ->
          List.iter
            (fun (ci, m, s) ->
              Hashtbl.add table (C (Name.from_ident ci.ci_id_class))
                (Typedtree.Tstr_class [ci, m, s]))
            info_list
      | Typedtree.Tstr_class_type info_list ->
          List.iter
            (fun ((id,id_loc,_) as ci) ->
              Hashtbl.add table
                (CT (Name.from_ident id))
                (Typedtree.Tstr_class_type [ci]))
            info_list
      | Typedtree.Tstr_value (_, pat_exp_list) ->
          List.iter
            (fun {vb_pat=pat; vb_expr=exp} ->
              match iter_val_pattern pat.Typedtree.pat_desc with
                None -> ()
              | Some n -> Hashtbl.add table_values n (pat,exp)
            )
            pat_exp_list
      | Typedtree.Tstr_primitive vd ->
          Hashtbl.add table (P (Name.from_ident vd.val_id)) tt
      | Typedtree.Tstr_open _ -> ()
      | Typedtree.Tstr_include _ -> ()
      | Typedtree.Tstr_eval _ -> ()
      | Typedtree.Tstr_attribute _ -> ()

    let tables typedtree =
      let t = Hashtbl.create 13 in
      let t_values = Hashtbl.create 13 in
      List.iter (fun str -> add_to_hashes t t_values str.str_desc) typedtree;
      (t, t_values)

    let search_module table name =
      match Hashtbl.find table (M name) with
        (Typedtree.Tstr_module mb) -> mb.mb_expr
      | _ -> assert false

    let search_module_type table name =
      match Hashtbl.find table (MT name) with
      | (Typedtree.Tstr_modtype mtd) -> mtd
      | _ -> assert false

    let search_extension table name =
      match Hashtbl.find table (X name) with
      | (Typedtree.Tstr_typext tyext) -> tyext
      | _ -> assert false

    let search_exception table name =
      match Hashtbl.find table (E name) with
      | (Typedtree.Tstr_exception ext) -> ext
      | _ -> assert false

    let search_type_declaration table name =
      match Hashtbl.find table (T name) with
      | (Typedtree.Tstr_type [td]) -> td
      | _ -> assert false

    let search_class_exp table name =
      match Hashtbl.find table (C name) with
      | (Typedtree.Tstr_class [(ci, _, _ )]) ->
          let ce = ci.ci_expr in
          (
           try
             let type_decl = search_type_declaration table name in
             (ce, type_decl.typ_type.Types.type_params)
           with
             Not_found ->
               (ce, [])
          )
      | _ -> assert false

    let search_class_type_declaration table name =
      match Hashtbl.find table (CT name) with
      | (Typedtree.Tstr_class_type [(_,_,cltype_decl)]) -> cltype_decl
      | _ -> assert false

    let search_value table name = Hashtbl.find table name

    let search_primitive table name =
      match Hashtbl.find table (P name) with
        Tstr_primitive vd -> vd.val_val.Types.val_type
      | _ -> assert false

    let get_nth_inherit_class_expr cls n =
      let rec iter cpt = function
        | [] ->
            raise Not_found
        | { cf_desc = Typedtree.Tcf_inherit (_, clexp, _, _, _) } :: q ->
            if n = cpt then clexp else iter (cpt+1) q
        | _ :: q ->
            iter cpt q
      in
      iter 0 cls.Typedtree.cstr_fields

    let search_attribute_type cls name =
      let rec iter = function
        | [] ->
            raise Not_found
        | { cf_desc = Typedtree.Tcf_val (_, _, ident, Tcfk_concrete (_, exp), _) } :: q
          when Name.from_ident ident = name ->
            exp.Typedtree.exp_type
        | { cf_desc = Typedtree.Tcf_val (_, _, ident, Tcfk_virtual typ, _) } :: q
          when Name.from_ident ident = name ->
            typ.Typedtree.ctyp_type
        | _ :: q ->
            iter q
      in
      iter cls.Typedtree.cstr_fields

    let class_sig_of_cltype_decl =
      let rec iter = function
        Types.Cty_constr (_, _, cty) -> iter cty
      | Types.Cty_signature s -> s
      | Types.Cty_arrow (_,_, cty) -> iter cty
      in
      fun ct_decl -> iter ct_decl.Types.clty_type

   let search_method_expression cls name =
      let rec iter = function
        | [] ->
            raise Not_found
        | { cf_desc = Typedtree.Tcf_method (label, _, Tcfk_concrete (_, exp)) } :: q when label.txt = name ->
            exp
        | _ :: q ->
            iter q
      in
      iter cls.Typedtree.cstr_fields
  end

module Analyser =
  functor (My_ir : Odoc_sig.Info_retriever) ->

  struct
    module Sig = Odoc_sig.Analyser (My_ir)

    (** This variable is used to load a file as a string and retrieve characters from it.*)
    let file = Sig.file

    (** The name of the analysed file. *)
    let file_name = Sig.file_name

    (** This function takes two indexes (start and end) and return the string
       corresponding to the indexes in the file global variable. The function
       prepare_file must have been called to fill the file global variable.*)
    let get_string_of_file = Sig.get_string_of_file

    (** This function loads the given file in the file global variable.
       and sets file_name.*)
    let prepare_file = Sig.prepare_file

    (** The function used to get the comments in a class. *)
    let get_comments_in_class = Sig.get_comments_in_class

    (** The function used to get the comments in a module. *)
    let get_comments_in_module = Sig.get_comments_in_module

    (** This function takes a parameter pattern and builds the
       corresponding [parameter] structure. The f_desc function
       is used to retrieve a parameter description, if any, from
       a parameter name.
    *)
    let tt_param_info_from_pattern env f_desc pat =
      let rec iter_pattern pat =
        match pat.pat_desc with
          Typedtree.Tpat_var (ident, _) ->
            let name = Name.from_ident ident in
            Simple_name { sn_name = name ;
                          sn_text = f_desc name ;
                          sn_type = Odoc_env.subst_type env pat.pat_type
                        }

        | Typedtree.Tpat_alias (pat, _, _) ->
            iter_pattern pat

        | Typedtree.Tpat_tuple patlist ->
            Tuple
              (List.map iter_pattern patlist,
               Odoc_env.subst_type env pat.pat_type)

        | Typedtree.Tpat_construct (_, cons_desc, _) when
            (* we give a name to the parameter only if it unit *)
            (match cons_desc.cstr_res.desc with
              Tconstr (p, _, _) ->
                Path.same p Predef.path_unit
            | _ ->
                false)
          ->
            (* a () argument, it never has description *)
            Simple_name { sn_name = "()" ;
                          sn_text = None ;
                          sn_type = Odoc_env.subst_type env pat.pat_type
                        }

        | _ ->
            (* implicit pattern matching -> anonymous parameter *)
            Simple_name { sn_name = "()" ;
                          sn_text = None ;
                          sn_type = Odoc_env.subst_type env pat.pat_type
                        }
      in
      iter_pattern pat

    (** Analysis of the parameter of a function. Return a list of t_parameter created from
       the (pattern, expression) structures encountered. *)
    let rec tt_analyse_function_parameters env current_comment_opt pat_exp_list =
      match pat_exp_list with
        [] ->
          (* This case means we have a 'function' without pattern, that's impossible *)
          raise (Failure "tt_analyse_function_parameters: 'function' without pattern")

      | {c_lhs=pattern_param} :: second_ele :: q ->
          (* implicit pattern matching -> anonymous parameter and no more parameter *)
          (* A VOIR : le label ? *)
          let parameter = Odoc_parameter.Tuple ([], Odoc_env.subst_type env pattern_param.pat_type) in
          [ parameter ]

      | {c_lhs=pattern_param; c_rhs=func_body} :: [] ->
          let parameter =
            tt_param_info_from_pattern
              env
              (Odoc_parameter.desc_from_info_opt current_comment_opt)
              pattern_param

          in
         (* For optional parameters with a default value, a special treatment is required *)
         (* we look if the name of the parameter we just add is "*opt*", which means
            that there is a let param_name = ... in ... just right now *)
          let (p, next_exp) =
            match parameter with
              Simple_name { sn_name = "*opt*" } ->
                (
                 (
                  match func_body.exp_desc with
                    Typedtree.Texp_let (_, {vb_pat={pat_desc = Typedtree.Tpat_var (id, _) };
                                            vb_expr=exp} :: _, func_body2) ->
                      let name = Name.from_ident id in
                      let new_param = Simple_name
                          { sn_name = name ;
                            sn_text = Odoc_parameter.desc_from_info_opt current_comment_opt name ;
                            sn_type = Odoc_env.subst_type env exp.exp_type
                          }
                      in
                      (new_param, func_body2)
                  | _ ->
                      print_DEBUG3 "Pas le bon filtre pour le parametre optionnel avec valeur par defaut.";
                      (parameter, func_body)
                 )
                )
            | _ ->
                (parameter, func_body)
          in
         (* continue if the body is still a function *)
          match next_exp.exp_desc with
            Texp_function (_, pat_exp_list, _) ->
              p :: (tt_analyse_function_parameters env current_comment_opt pat_exp_list)
          | _ ->
              (* something else ; no more parameter *)
              [ p ]

     (** Analysis of a Tstr_value from the typedtree. Create and return a list of [t_value].
        @raise Failure if an error occurs.*)
     let tt_analyse_value env current_module_name comment_opt loc pat_exp rec_flag =
       let (pat, exp) = pat_exp in
       match (pat.pat_desc, exp.exp_desc) with
         (Typedtree.Tpat_var (ident, _), Typedtree.Texp_function (_, pat_exp_list2, partial)) ->
           (* a new function is defined *)
           let name_pre = Name.from_ident ident in
           let name = Name.parens_if_infix name_pre in
           let complete_name = Name.concat current_module_name name in
           let code =
              if !Odoc_global.keep_code then
                Some (get_string_of_file loc.Location.loc_start.Lexing.pos_cnum
                      loc.Location.loc_end.Lexing.pos_cnum)
              else
                None
            in
           (* create the value *)
           let new_value = {
             val_name = complete_name ;
             val_info = comment_opt ;
             val_type = Odoc_env.subst_type env pat.Typedtree.pat_type ;
             val_recursive = rec_flag = Asttypes.Recursive ;
             val_parameters = tt_analyse_function_parameters env comment_opt pat_exp_list2 ;
             val_code = code ;
             val_loc = { loc_impl = Some loc ; loc_inter = None } ;
           }
           in
           [ new_value ]

       | (Typedtree.Tpat_var (ident, _), _) ->
           (* a new value is defined *)
           let name_pre = Name.from_ident ident in
           let name = Name.parens_if_infix name_pre in
           let complete_name = Name.concat current_module_name name in
           let code =
             if !Odoc_global.keep_code then
                Some (get_string_of_file loc.Location.loc_start.Lexing.pos_cnum
                      loc.Location.loc_end.Lexing.pos_cnum)
             else
               None
            in
           let new_value = {
             val_name = complete_name ;
             val_info = comment_opt ;
             val_type = Odoc_env.subst_type env pat.Typedtree.pat_type ;
             val_recursive = rec_flag = Asttypes.Recursive ;
             val_parameters = [] ;
             val_code = code ;
             val_loc = { loc_impl = Some loc ; loc_inter = None } ;
           }
           in
           [ new_value ]

       | (Typedtree.Tpat_tuple lpat, _) ->
           (* new identifiers are defined *)
           (* A VOIR : by now we don't accept to have global variables defined in tuples *)
           []

       | _ ->
           (* something else, we don't care ? A VOIR *)
           []

    (** This function takes a Typedtree.class_expr and returns a string which can stand for the class name.
       The name can be "object ... end" if the class expression is not an ident or a class constraint or a class apply. *)
    let rec tt_name_of_class_expr clexp =
(*
      (
       match clexp.Typedtree.cl_desc with
         Tclass_ident _ -> prerr_endline "Tclass_ident"
       | Tclass_structure _ -> prerr_endline "Tclass_structure"
       | Tclass_fun _ -> prerr_endline "Tclass_fun"
       | Tclass_apply _ -> prerr_endline "Tclass_apply"
       | Tclass_let _ -> prerr_endline "Tclass_let"
       | Tclass_constraint _ -> prerr_endline "Tclass_constraint"
      );
*)
      match clexp.Typedtree.cl_desc with
        Typedtree.Tcl_ident (p, _, _) -> Name.from_path p
      | Typedtree.Tcl_constraint (class_expr, _, _, _, _)
      | Typedtree.Tcl_apply (class_expr, _) -> tt_name_of_class_expr class_expr
(*
      | Typedtree.Tclass_fun (_, _, class_expr, _) -> tt_name_of_class_expr class_expr
      | Typedtree.Tclass_let (_,_,_, class_expr) -> tt_name_of_class_expr class_expr
*)
      |  _ -> Odoc_messages.object_end

    (** Analysis of a method expression to get the method parameters.
       @param first indicates if we're analysing the method for
       the first time ; in that case we must not keep the first parameter,
       which is "self-*", the object itself.
    *)
    let rec tt_analyse_method_expression env current_method_name comment_opt ?(first=true) exp =
      match exp.Typedtree.exp_desc with
        Typedtree.Texp_function (_, pat_exp_list, _) ->
          (
           match pat_exp_list with
             [] ->
               (* it is not a function since there are no parameters *)
               (* we can't get here normally *)
               raise (Failure (Odoc_messages.bad_tree^" "^(Odoc_messages.method_without_param current_method_name)))
           | l ->
               match l with
                 [] ->
                   (* cas impossible, on l'a filtre avant *)
                   assert false
               | {c_lhs=pattern_param} :: second_ele :: q ->
                   (* implicit pattern matching -> anonymous parameter *)
                   (* Note : We can't match this pattern if it is the first call to the function. *)
                   let new_param = Simple_name
                       { sn_name = "??" ; sn_text =  None;
                         sn_type = Odoc_env.subst_type env pattern_param.Typedtree.pat_type }
                   in
                   [ new_param ]

               | {c_lhs=pattern_param; c_rhs=body} :: [] ->
                   (* if this is the first call to the function, this is the first parameter and we skip it *)
                   if not first then
                     (
                      let parameter =
                        tt_param_info_from_pattern
                          env
                          (Odoc_parameter.desc_from_info_opt comment_opt)
                          pattern_param
                      in
                      (* For optional parameters with a default value, a special treatment is required. *)
                      (* We look if the name of the parameter we just add is "*opt*", which means
                         that there is a let param_name = ... in ... just right now. *)
                      let (current_param, next_exp) =
                        match parameter with
                          Simple_name { sn_name = "*opt*"} ->
                            (
                             (
                              match body.exp_desc with
                                Typedtree.Texp_let (_, {vb_pat={pat_desc = Typedtree.Tpat_var (id, _) };
                                                        vb_expr=exp} :: _, body2) ->
                                  let name = Name.from_ident id in
                                  let new_param = Simple_name
                                      { sn_name = name ;
                                        sn_text = Odoc_parameter.desc_from_info_opt comment_opt name ;
                                        sn_type = Odoc_env.subst_type env exp.Typedtree.exp_type ;
                                      }
                                  in
                                  (new_param, body2)
                              | _ ->
                                  print_DEBUG3 "Pas le bon filtre pour le parametre optionnel avec valeur par defaut.";
                                  (parameter, body)
                             )
                            )
                        | _ ->
                            (* no *opt* parameter, we add the parameter then continue *)
                            (parameter, body)
                      in
                      current_param :: (tt_analyse_method_expression env current_method_name comment_opt ~first: false next_exp)
                     )
                   else
                     tt_analyse_method_expression env current_method_name comment_opt ~first: false body
          )
      | _ ->
          (* no more parameter *)
          []

    (** Analysis of a [Parsetree.class_struture] and a [Typedtree.class_structure] to get a couple
       (inherited classes, class elements). *)
    let analyse_class_structure env current_class_name tt_class_sig last_pos pos_limit p_cls tt_cls table =
      let rec iter acc_inher acc_fields last_pos = function
        | [] ->
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
            (acc_inher, acc_fields @ ele_comments)
          | item :: q ->
              let loc = item.Parsetree.pcf_loc in
              match item.Parsetree.pcf_desc with
        | (Parsetree.Pcf_inherit (_, p_clexp, _))  ->
            let tt_clexp =
              let n = List.length acc_inher in
              try Typedtree_search.get_nth_inherit_class_expr tt_cls n
              with Not_found ->
                raise (Failure (
                       Odoc_messages.inherit_classexp_not_found_in_typedtree n))
            in
            let (info_opt, ele_comments) =
              get_comments_in_class last_pos
                p_clexp.Parsetree.pcl_loc.Location.loc_start.Lexing.pos_cnum
            in
            let text_opt =
              match info_opt with None -> None
              | Some i -> i.Odoc_types.i_desc in
            let name = tt_name_of_class_expr tt_clexp in
            let inher =
              {
                ic_name = Odoc_env.full_class_or_class_type_name env name ;
                ic_class = None ;
                ic_text = text_opt ;
              }
            in
            iter (acc_inher @ [ inher ]) (acc_fields @ ele_comments)
              p_clexp.Parsetree.pcl_loc.Location.loc_end.Lexing.pos_cnum
              q

      | Parsetree.Pcf_val ({ txt = label }, mutable_flag, k) ->
            let virt = match k with Parsetree.Cfk_virtual _ -> true | Parsetree.Cfk_concrete _ -> false in
            let complete_name = Name.concat current_class_name label in
            let (info_opt, ele_comments) = get_comments_in_class last_pos loc.Location.loc_start.Lexing.pos_cnum in
            let type_exp =
              try Typedtree_search.search_attribute_type tt_cls label
              with Not_found ->
                raise (Failure (Odoc_messages.attribute_not_found_in_typedtree complete_name))
          in
          let code =
            if !Odoc_global.keep_code then
              Some (get_string_of_file loc.Location.loc_start.Lexing.pos_cnum
                    loc.Location.loc_end.Lexing.pos_cnum)
            else
              None
          in
          let att =
            {
              att_value = { val_name = complete_name ;
                val_info = info_opt ;
                val_type = Odoc_env.subst_type env type_exp ;
                val_recursive = false ;
                val_parameters = [] ;
                val_code = code ;
                val_loc = { loc_impl = Some loc ; loc_inter = None } ;
              } ;
              att_mutable = mutable_flag = Asttypes.Mutable ;
              att_virtual = virt ;
            }
          in
          iter acc_inher (acc_fields @ ele_comments @ [ Class_attribute att ]) loc.Location.loc_end.Lexing.pos_cnum q

        | (Parsetree.Pcf_method  ({ txt = label }, private_flag, Parsetree.Cfk_virtual _)) ->
            let complete_name = Name.concat current_class_name label in
            let (info_opt, ele_comments) = get_comments_in_class last_pos loc.Location.loc_start.Lexing.pos_cnum in
            let met_type =
              try Odoc_sig.Signature_search.search_method_type label tt_class_sig
              with Not_found -> raise (Failure (Odoc_messages.method_type_not_found current_class_name label))
            in
            let real_type =
              match met_type.Types.desc with
              Tarrow (_, _, t, _) ->
                t
            |  _ ->
                (* ?!? : not an arrow type ! return the original type *)
                met_type
          in
          let code =
            if !Odoc_global.keep_code then
              Some (get_string_of_file loc.Location.loc_start.Lexing.pos_cnum
               loc.Location.loc_end.Lexing.pos_cnum)
            else
              None
          in
          let met =
            {
              met_value = {
                val_name = complete_name ;
                val_info = info_opt ;
                val_type = Odoc_env.subst_type env real_type ;
                val_recursive = false ;
                val_parameters = [] ;
                val_code = code ;
                val_loc = { loc_impl = Some loc ; loc_inter = None } ;
              } ;
              met_private = private_flag = Asttypes.Private ;
              met_virtual = true ;
            }
          in
          (* update the parameter description *)
          Odoc_value.update_value_parameters_text met.met_value;

          iter acc_inher (acc_fields @ ele_comments @ [ Class_method met ]) loc.Location.loc_end.Lexing.pos_cnum q

        | (Parsetree.Pcf_method ({ txt = label }, private_flag, Parsetree.Cfk_concrete _)) ->
            let complete_name = Name.concat current_class_name label in
            let (info_opt, ele_comments) = get_comments_in_class last_pos loc.Location.loc_start.Lexing.pos_cnum in
            let exp =
              try Typedtree_search.search_method_expression tt_cls label
            with Not_found -> raise (Failure (Odoc_messages.method_not_found_in_typedtree complete_name))
          in
          let real_type =
            match exp.exp_type.desc with
              Tarrow (_, _, t,_) ->
                t
            |  _ ->
                (* ?!? : not an arrow type ! return the original type *)
                exp.Typedtree.exp_type
          in
          let code =
            if !Odoc_global.keep_code then
                Some (get_string_of_file loc.Location.loc_start.Lexing.pos_cnum
               loc.Location.loc_end.Lexing.pos_cnum)
            else
              None
          in
          let met =
            {
              met_value = { val_name = complete_name ;
                val_info = info_opt ;
                val_type = Odoc_env.subst_type env real_type ;
                val_recursive = false ;
                val_parameters = tt_analyse_method_expression env complete_name info_opt exp ;
                val_code = code ;
                val_loc = { loc_impl = Some loc ; loc_inter = None } ;
              } ;
              met_private = private_flag = Asttypes.Private ;
              met_virtual = false ;
              }
          in
          (* update the parameter description *)
          Odoc_value.update_value_parameters_text met.met_value;

          iter acc_inher (acc_fields @ ele_comments @ [ Class_method met ]) loc.Location.loc_end.Lexing.pos_cnum q

        | Parsetree.Pcf_constraint (_, _) ->
            (* don't give a $*%@ ! *)
            iter acc_inher acc_fields loc.Location.loc_end.Lexing.pos_cnum q

        | (Parsetree.Pcf_initializer exp) ->
            iter acc_inher acc_fields exp.Parsetree.pexp_loc.Location.loc_end.Lexing.pos_cnum q

        | Parsetree.Pcf_attribute _ ->
            iter acc_inher acc_fields loc.Location.loc_end.Lexing.pos_cnum q

        | Parsetree.Pcf_extension _ -> assert false
      in
      iter [] [] last_pos (p_cls.Parsetree.pcstr_fields)

    (** Analysis of a [Parsetree.class_expr] and a [Typedtree.class_expr] to get a a couple (class parameters, class kind). *)
    let rec analyse_class_kind env current_class_name comment_opt last_pos p_class_expr tt_class_exp table =
      match (p_class_expr.Parsetree.pcl_desc, tt_class_exp.Typedtree.cl_desc) with
        (Parsetree.Pcl_constr (lid, _), tt_class_exp_desc ) ->
          let name =
            match tt_class_exp_desc with
              Typedtree.Tcl_ident (p,_,_) -> Name.from_path p
            | _ ->
                (* we try to get the name from the environment. *)
                (* A VOIR : dommage qu'on n'ait pas un Tclass_ident :-( meme quand on a class tutu = toto *)
                Name.from_longident lid.txt
          in
          (* On n'a pas ici les parametres de type sous forme de Types.type_expr,
             par contre on peut les trouver dans le class_type *)
          let params =
            match tt_class_exp.Typedtree.cl_type with
              Types.Cty_constr (p2, type_exp_list, cltyp) ->
                (* cltyp is the class type for [type_exp_list] p *)
                type_exp_list
            | _ ->
                []
          in
          ([],
           Class_constr
             {
               cco_name = Odoc_env.full_class_name env name ;
               cco_class = None ;
               cco_type_parameters = List.map (Odoc_env.subst_type env) params ;
             } )

      | (Parsetree.Pcl_structure p_class_structure, Typedtree.Tcl_structure tt_class_structure) ->
          (* we need the class signature to get the type of methods in analyse_class_structure *)
          let tt_class_sig =
            match tt_class_exp.Typedtree.cl_type with
              Types.Cty_signature class_sig -> class_sig
            | _ -> raise (Failure "analyse_class_kind: no class signature for a class structure.")
          in
          let (inherited_classes, class_elements) = analyse_class_structure
              env
              current_class_name
              tt_class_sig
              last_pos
              p_class_expr.Parsetree.pcl_loc.Location.loc_end.Lexing.pos_cnum
              p_class_structure
              tt_class_structure
              table
          in
          ([],
           Class_structure (inherited_classes, class_elements) )

      | (Parsetree.Pcl_fun (label, expression_opt, pattern, p_class_expr2),
         Typedtree.Tcl_fun (_, pat, ident_exp_list, tt_class_expr2, partial)) ->
           (* we check that this is not an optional parameter with
              a default value. In this case, we look for the good parameter pattern *)
           let (parameter, next_tt_class_exp) =
             match pat.Typedtree.pat_desc with
               Typedtree.Tpat_var (ident, _) when Name.from_ident ident = "*opt*" ->
                 (
                  (* there must be a Tcl_let just after *)
                  match tt_class_expr2.Typedtree.cl_desc with
                    Typedtree.Tcl_let (_, {vb_pat={pat_desc = Typedtree.Tpat_var (id,_) };
                                           vb_expr=exp} :: _, _, tt_class_expr3) ->
                      let name = Name.from_ident id in
                      let new_param = Simple_name
                          { sn_name = name ;
                            sn_text = Odoc_parameter.desc_from_info_opt comment_opt name ;
                            sn_type = Odoc_env.subst_type env exp.exp_type
                          }
                      in
                      (new_param, tt_class_expr3)
                 | _ ->
                     (* strange case *)
                     (* we create the parameter and add it to the class *)
                     raise (Failure "analyse_class_kind: strange case")
                 )
             | _ ->
                 (* no optional parameter with default value, we create the parameter *)
                 let new_param =
                   tt_param_info_from_pattern
                     env
                     (Odoc_parameter.desc_from_info_opt comment_opt)
                     pat
                 in
                 (new_param, tt_class_expr2)
           in
           let (params, k) = analyse_class_kind
              env current_class_name comment_opt last_pos p_class_expr2
                next_tt_class_exp table
            in
           (parameter :: params, k)

      | (Parsetree.Pcl_apply (p_class_expr2, _), Tcl_apply (tt_class_expr2, exp_opt_optional_list)) ->
          let applied_name =
            (* we want an ident, or else the class applied will appear in the form object ... end,
               because if the class applied has no name, the code is kinda ugly, isn't it ? *)
            match tt_class_expr2.Typedtree.cl_desc with
              Typedtree.Tcl_ident (p,_,_) -> Name.from_path p (* A VOIR : obtenir le nom complet *)
            | _ ->
                (* A VOIR : dommage qu'on n'ait pas un Tclass_ident :-( meme quand on a class tutu = toto *)
                match p_class_expr2.Parsetree.pcl_desc with
                  Parsetree.Pcl_constr (lid, _) ->
                    (* we try to get the name from the environment. *)
                    Name.from_longident lid.txt
                |  _ ->
                    Odoc_messages.object_end
          in
          let param_exps = List.fold_left
              (fun acc -> fun (_, exp_opt, _) ->
                match exp_opt with
                  None -> acc
                | Some e -> acc @ [e])
              []
              exp_opt_optional_list
          in
          let param_types = List.map (fun e -> e.Typedtree.exp_type) param_exps in
          let params_code =
            List.map
              (fun e -> get_string_of_file
                  e.exp_loc.Location.loc_start.Lexing.pos_cnum
                  e.exp_loc.Location.loc_end.Lexing.pos_cnum)
              param_exps
          in
          ([],
           Class_apply
             { capp_name = Odoc_env.full_class_name env applied_name ;
               capp_class = None ;
               capp_params = param_types ;
               capp_params_code = params_code ;
             } )

      | (Parsetree.Pcl_let (_, _, p_class_expr2), Typedtree.Tcl_let (_, _, _, tt_class_expr2)) ->
          (* we don't care about these lets *)
          analyse_class_kind
              env current_class_name comment_opt last_pos p_class_expr2
              tt_class_expr2 table

      | (Parsetree.Pcl_constraint (p_class_expr2, p_class_type2),
         Typedtree.Tcl_constraint (tt_class_expr2, _, _, _, _)) ->
          let (l, class_kind) = analyse_class_kind
              env current_class_name comment_opt last_pos p_class_expr2
                tt_class_expr2 table
            in
            (* A VOIR : analyse du class type ? on n'a pas toutes les infos. cf. Odoc_sig.analyse_class_type_kind *)
          let class_type_kind =
            (*Sig.analyse_class_type_kind
              env
              ""
              p_class_type2.Parsetree.pcty_loc.Location.loc_start.Lexing.pos_cnum
              p_class_type2
              tt_class_expr2.Typedtree.cl_type
            *)
            Class_type { cta_name = Odoc_messages.object_end ;
                         cta_class = None ; cta_type_parameters = [] }
          in
          (l, Class_constraint (class_kind, class_type_kind))

      | _ ->
          raise (Failure "analyse_class_kind: Parsetree and typedtree don't match.")

    (** Analysis of a [Parsetree.class_declaration] and a [Typedtree.class_expr] to return a [t_class].*)
    let analyse_class env current_module_name comment_opt p_class_decl tt_type_params tt_class_exp table =
      let name = p_class_decl.Parsetree.pci_name in
      let complete_name = Name.concat current_module_name name.txt in
      let loc = p_class_decl.Parsetree.pci_expr.Parsetree.pcl_loc in
      let pos_start = loc.Location.loc_start.Lexing.pos_cnum in
      let type_parameters = tt_type_params in
      let virt = p_class_decl.Parsetree.pci_virt = Asttypes.Virtual in
      let cltype = Odoc_env.subst_class_type env tt_class_exp.Typedtree.cl_type in
      let (parameters, kind) = analyse_class_kind
          env
          complete_name
          comment_opt
          pos_start
          p_class_decl.Parsetree.pci_expr
          tt_class_exp
          table
      in
      let cl =
        {
          cl_name = complete_name ;
          cl_info = comment_opt ;
          cl_type = cltype ;
          cl_virtual = virt ;
          cl_type_parameters = type_parameters ;
          cl_kind = kind ;
          cl_parameters = parameters ;
          cl_loc = { loc_impl = Some loc ; loc_inter = None } ;
        }
      in
      cl

    (** Get a name from a module expression, or "struct ... end" if the module expression
       is not an ident of a constraint on an ident. *)
    let rec tt_name_from_module_expr mod_expr =
      match mod_expr.Typedtree.mod_desc with
        Typedtree.Tmod_ident (p,_) -> Name.from_path p
      | Typedtree.Tmod_constraint (m_exp, _, _, _) -> tt_name_from_module_expr m_exp
      | Typedtree.Tmod_structure _
      | Typedtree.Tmod_functor _
      | Typedtree.Tmod_apply _
      | Typedtree.Tmod_unpack _ ->
          Odoc_messages.struct_end

    (** Get the list of included modules in a module structure of a typed tree. *)
    let tt_get_included_module_list tt_structure =
      let f acc item =
        match item.str_desc with
          Typedtree.Tstr_include incl ->
            acc @ [
                  { (* A VOIR : chercher dans les modules et les module types, avec quel env ? *)
                    im_name = tt_name_from_module_expr incl.incl_mod ;
                    im_module = None ;
                    im_info = None ;
                  }
                ]
        | _ ->
            acc
      in
      List.fold_left f [] tt_structure.str_items

    (** This function takes a [module element list] of a module and replaces the "dummy" included modules with
       the ones found in typed tree structure of the module. *)
    let replace_dummy_included_modules module_elements included_modules =
      let rec f = function
        | ([], _) ->
            []
        | ((Element_included_module im) :: q, (im_repl :: im_q)) ->
            (Element_included_module { im_repl with im_info = im.im_info })
            :: (f (q, im_q))
        | ((Element_included_module im) :: q, []) ->
            (Element_included_module im) :: q
        | (ele :: q, l) ->
            ele :: (f (q, l))
      in
      f (module_elements, included_modules)

    (** This function removes the elements of the module which does not
       belong to the given module type, if the module type is expanded
       and the module has a "structure" kind. *)
    let rec filter_module_with_module_type_constraint m mt =
      match m.m_kind, mt with
        Module_struct l, Types.Mty_signature lsig ->
          m.m_kind <- Module_struct (filter_module_elements_with_module_type_constraint l lsig);
          m.m_type <- mt;
      | _ -> ()

    (** This function removes the elements of the module type which does not
       belong to the given module type, if the module type is expanded
       and the module type has a "structure" kind. *)
    and filter_module_type_with_module_type_constraint mtyp mt =
      match mtyp.mt_kind, mt with
        Some Module_type_struct l, Types.Mty_signature lsig ->
          mtyp.mt_kind <- Some (Module_type_struct (filter_module_elements_with_module_type_constraint l lsig));
          mtyp.mt_type <- Some mt;
      | _ -> ()

    and filter_module_elements_with_module_type_constraint l lsig =
      let pred ele =
        let f = match ele with
          Element_module m ->
            (function
                Types.Sig_module (ident,md,_) ->
                  let n1 = Name.simple m.m_name
                  and n2 = Ident.name ident in
                  (
                   match n1 = n2 with
                     true -> filter_module_with_module_type_constraint m md.md_type; true
                   | false -> false
                  )
              | _ -> false)
        | Element_module_type mt ->
            (function
                Types.Sig_modtype (ident,{Types.mtd_type=Some t}) ->
                  let n1 = Name.simple mt.mt_name
                  and n2 = Ident.name ident in
                  (
                   match n1 = n2 with
                     true -> filter_module_type_with_module_type_constraint mt t; true
                   | false -> false
                  )
              | _ -> false)
        | Element_value v ->
            (function
                Types.Sig_value (ident,_) ->
                  let n1 = Name.simple v.val_name
                  and n2 = Ident.name ident in
                  n1 = n2
              | _ -> false)
        | Element_type t ->
             (function
                Types.Sig_type (ident,_,_) ->
                  (* A VOIR: il est possible que le detail du type soit cache *)
                  let n1 = Name.simple t.ty_name
                  and n2 = Ident.name ident in
                  n1 = n2
               | _ -> false)
        | Element_type_extension te ->
            let l =
              filter_extension_constructors_with_module_type_constraint
                te.te_constructors lsig
            in
              te.te_constructors <- l;
              if l <> [] then (fun _ -> true)
              else (fun _ -> false)
        | Element_exception e ->
            (function
                Types.Sig_typext (ident,_,_) ->
                  let n1 = Name.simple e.ex_name
                  and n2 = Ident.name ident in
                  n1 = n2
              | _ -> false)
        | Element_class c ->
            (function
                Types.Sig_class (ident,_,_) ->
                  let n1 = Name.simple c.cl_name
                  and n2 = Ident.name ident in
                  n1 = n2
              | _ -> false)
        | Element_class_type ct ->
            (function
                Types.Sig_class_type (ident,_,_) ->
                  let n1 = Name.simple ct.clt_name
                  and n2 = Ident.name ident in
                  n1 = n2
              | _ -> false)
        | Element_module_comment _ -> fun _ -> true
        | Element_included_module _ -> fun _ -> true
        in
        List.exists f lsig
      in
      List.filter pred l

    and filter_extension_constructors_with_module_type_constraint l lsig =
      let pred xt =
        List.exists
          (function
              Types.Sig_typext (ident, _, _) ->
                let n1 = Name.simple xt.xt_name
                and n2 = Ident.name ident in
                  n1 = n2
            | _ -> false)
          lsig
      in
        List.filter pred l

    (** Analysis of a parse tree structure with a typed tree, to return module elements.*)
    let rec analyse_structure env current_module_name last_pos pos_limit parsetree typedtree =
      print_DEBUG "Odoc_ast:analyse_struture";
      let (table, table_values) = Typedtree_search.tables typedtree.str_items in
      let rec iter env last_pos = function
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
            ele_comments
        | item :: q ->
            let (comment_opt, ele_comments) =
              get_comments_in_module last_pos item.Parsetree.pstr_loc.Location.loc_start.Lexing.pos_cnum
            in
            let pos_limit2 =
              match q with
                [] -> pos_limit
              | item2 :: _ -> item2.Parsetree.pstr_loc.Location.loc_start.Lexing.pos_cnum
            in
            let (maybe_more, new_env, elements) = analyse_structure_item
                env
                current_module_name
                item.Parsetree.pstr_loc
                pos_limit2
                comment_opt
                item.Parsetree.pstr_desc
                typedtree
                table
                table_values
            in
            ele_comments @ elements @ (iter new_env (item.Parsetree.pstr_loc.Location.loc_end.Lexing.pos_cnum + maybe_more) q)
      in
      iter env last_pos parsetree

   (** Analysis of a parse tree structure item to obtain a new environment and a list of elements.*)
   and analyse_structure_item env current_module_name loc pos_limit comment_opt parsetree_item_desc typedtree
        table table_values =
      print_DEBUG "Odoc_ast:analyse_struture_item";
      match parsetree_item_desc with
        Parsetree.Pstr_eval _ ->
          (* don't care *)
          (0, env, [])
      | Parsetree.Pstr_attribute _
      | Parsetree.Pstr_extension _ ->
          (0, env, [])
      | Parsetree.Pstr_value (rec_flag, pat_exp_list) ->
          (* of rec_flag * (pattern * expression) list *)
          (* For each value, look for the value name, then look in the
             typedtree for the corresponding information,
             at last analyse this information to build the value *)
          let rec iter_pat = function
            | Parsetree.Ppat_any -> None
            | Parsetree.Ppat_var name -> Some name
            | Parsetree.Ppat_tuple _ -> None (* A VOIR quand on traitera les tuples *)
            | Parsetree.Ppat_constraint (pat, _) -> iter_pat pat.Parsetree.ppat_desc
            | _ -> None
          in
          let rec iter ?(first=false) last_pos acc_env acc p_e_list =
            match p_e_list with
              [] ->
                (acc_env, acc)
            | {Parsetree.pvb_pat=pat; pvb_expr=exp} :: q ->
                let value_name_opt = iter_pat pat.Parsetree.ppat_desc in
                let new_last_pos = exp.Parsetree.pexp_loc.Location.loc_end.Lexing.pos_cnum in
                match value_name_opt with
                  None ->
                    iter new_last_pos acc_env acc q
                | Some name ->
                    try
                      let pat_exp = Typedtree_search.search_value table_values name.txt in
                      let (info_opt, ele_comments) =
                        (* we already have the optional comment for the first value. *)
                        if first then
                          (comment_opt, [])
                        else
                          get_comments_in_module
                            last_pos
                            pat.Parsetree.ppat_loc.Location.loc_start.Lexing.pos_cnum
                      in
                      let l_values = tt_analyse_value
                          env
                          current_module_name
                          info_opt
                          loc
                          pat_exp
                          rec_flag
                      in
                      let new_env = List.fold_left
                          (fun e -> fun v ->
                            Odoc_env.add_value e v.val_name
                          )
                          acc_env
                          l_values
                      in
                      let l_ele = List.map (fun v -> Element_value v) l_values in
                      iter
                        new_last_pos
                        new_env
                        (acc @ ele_comments @ l_ele)
                        q
                    with
                      Not_found ->
                        iter new_last_pos acc_env acc q
          in
          let (new_env, l_ele) = iter ~first: true loc.Location.loc_start.Lexing.pos_cnum env [] pat_exp_list in
          (0, new_env, l_ele)

      | Parsetree.Pstr_primitive val_desc ->
            let name_pre = val_desc.Parsetree.pval_name.txt in
            (* of string * value_description *)
            print_DEBUG ("Parsetree.Pstr_primitive ("^name_pre^", ["^(String.concat ", " val_desc.Parsetree.pval_prim)^"]");
            let typ = Typedtree_search.search_primitive table name_pre in
            let name = Name.parens_if_infix name_pre in
            let complete_name = Name.concat current_module_name name in
            let code =
              if !Odoc_global.keep_code then
                Some (get_string_of_file loc.Location.loc_start.Lexing.pos_cnum
                      loc.Location.loc_end.Lexing.pos_cnum)
              else
                None
            in
            let new_value = {
                val_name = complete_name ;
                val_info = comment_opt ;
                val_type = Odoc_env.subst_type env typ ;
                val_recursive = false ;
                val_parameters = [] ;
                val_code = code ;
                val_loc = { loc_impl = Some loc ; loc_inter = None } ;
              }
            in
            let new_env = Odoc_env.add_value env new_value.val_name in
            (0, new_env, [Element_value new_value])

      | Parsetree.Pstr_type name_typedecl_list ->
          (* of (string * type_declaration) list *)
          let extended_env =
            List.fold_left
              (fun acc_env {Parsetree.ptype_name = { txt = name }} ->
                let complete_name = Name.concat current_module_name name in
                Odoc_env.add_type acc_env complete_name
              )
              env
              name_typedecl_list
          in
          let env =
            let is_nonrec =
              List.exists
                (fun td ->
                   List.exists (fun (n, _) -> n.txt = "nonrec")
                     td.Parsetree.ptype_attributes)
                name_typedecl_list
            in
            if is_nonrec then env else extended_env
          in
          let rec f ?(first=false) maybe_more_acc last_pos name_type_decl_list =
            match name_type_decl_list with
              [] -> (maybe_more_acc, [])
            | type_decl :: q ->
                let name = type_decl.Parsetree.ptype_name.txt in
                let complete_name = Name.concat current_module_name name in
                let loc = type_decl.Parsetree.ptype_loc in
                let loc_start = loc.Location.loc_start.Lexing.pos_cnum in
                let loc_end =  loc.Location.loc_end.Lexing.pos_cnum in
                let pos_limit2 =
                  match q with
                      [] -> pos_limit
                    | td :: _ -> td.Parsetree.ptype_loc.Location.loc_start.Lexing.pos_cnum
                  in
                  let (maybe_more, name_comment_list) =
                    Sig.name_comment_from_type_decl loc_end pos_limit2 type_decl
                  in
                  let tt_type_decl =
                    try Typedtree_search.search_type_declaration table name
                    with Not_found -> raise (Failure (Odoc_messages.type_not_found_in_typedtree complete_name))
                  in
                  let tt_type_decl = tt_type_decl.Typedtree.typ_type in
                  let (com_opt, ele_comments) = (* the comment for the first type was already retrieved *)
                    if first then
                      (comment_opt , [])
                    else
                      get_comments_in_module last_pos loc_start
                  in
                  let kind = Sig.get_type_kind
                    env name_comment_list
                    tt_type_decl.Types.type_kind
                  in
                  let new_end = loc_end + maybe_more in
                  let t =
                    {
                      ty_name = complete_name ;
                      ty_info = com_opt ;
                      ty_parameters =
                      List.map2
                       (fun p v ->
                         let (co, cn) = Types.Variance.get_upper v in
                         (Odoc_env.subst_type env p, co, cn))
                       tt_type_decl.Types.type_params
                       tt_type_decl.Types.type_variance ;
                      ty_kind = kind ;
                      ty_private = tt_type_decl.Types.type_private;
                      ty_manifest =
                        (match tt_type_decl.Types.type_manifest with
                           None -> None
                         | Some t ->
                           Some (Sig.manifest_structure env name_comment_list t));
                      ty_loc = { loc_impl = Some loc ; loc_inter = None } ;
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
                  t.ty_info <- Sig.merge_infos t.ty_info info_after_opt ;
                  let (maybe_more3, eles) = f (maybe_more + maybe_more2) (new_end + maybe_more2) q in
                  (maybe_more3, ele_comments @ ((Element_type t) :: eles))
            in
            let (maybe_more, eles) = f ~first: true 0 loc.Location.loc_start.Lexing.pos_cnum name_typedecl_list in
            (maybe_more, extended_env, eles)

      | Parsetree.Pstr_typext tyext ->
          (* we get the extension declaration in the typed tree *)
          let tt_tyext =
            match tyext.Parsetree.ptyext_constructors with
              [] -> assert false
            | ext :: _ ->
                try
                  Typedtree_search.search_extension table ext.Parsetree.pext_name.txt
                with Not_found ->
                  raise (Failure
                           (Odoc_messages.extension_not_found_in_typedtree
                          (Name.concat current_module_name ext.Parsetree.pext_name.txt)))
          in
          let new_env =
            List.fold_left
              (fun acc_env -> fun {Parsetree.pext_name = { txt = name }} ->
                let complete_name = Name.concat current_module_name name in
                  Odoc_env.add_extension acc_env complete_name
              )
              env
              tyext.Parsetree.ptyext_constructors
          in
          let loc_start = loc.Location.loc_start.Lexing.pos_cnum in
          let loc_end =  loc.Location.loc_end.Lexing.pos_cnum in
          let new_te =
            {
              te_info = comment_opt;
              te_type_name =
                Odoc_env.full_type_name new_env (Name.from_path tt_tyext.tyext_path);
              te_type_parameters =
                List.map (fun (ctyp, _) -> Odoc_env.subst_type new_env ctyp.ctyp_type)  tt_tyext.tyext_params;
              te_private = tt_tyext.tyext_private;
              te_constructors = [];
              te_loc = { loc_impl = Some loc ; loc_inter = None } ;
              te_code =
                (
                  if !Odoc_global.keep_code then
                    Some (get_string_of_file loc_start loc_end)
                  else
                    None
                ) ;
            }
          in
          let rec analyse_extension_constructors maybe_more exts_acc tt_ext_list =
              match tt_ext_list with
                  [] -> (maybe_more, List.rev exts_acc)
                | tt_ext :: q ->
                    let complete_name = Name.concat current_module_name tt_ext.ext_name.txt in
                    let ext_loc_end =  tt_ext.ext_loc.Location.loc_end.Lexing.pos_cnum in
                    let new_xt =
                      match tt_ext.ext_kind with
                          Text_decl(args, ret_type) ->
                            {
                              xt_name = complete_name;
                              xt_args =
                                List.map (fun ctyp -> Odoc_env.subst_type new_env ctyp.ctyp_type) args;
                              xt_ret =
                                may_map (fun ctyp -> Odoc_env.subst_type new_env ctyp.ctyp_type) ret_type;
                              xt_type_extension = new_te;
                              xt_alias = None;
                              xt_loc = { loc_impl = Some tt_ext.ext_loc ; loc_inter = None } ;
                              xt_text = None;
                            }
                        | Text_rebind(path, _) ->
                            {
                              xt_name = complete_name;
                              xt_args = [];
                              xt_ret = None;
                              xt_type_extension = new_te;
                              xt_alias =
                                Some {
                                  xa_name = Odoc_env.full_extension_constructor_name env (Name.from_path path);
                                  xa_xt = None;
                                };
                              xt_loc = { loc_impl = Some tt_ext.ext_loc ; loc_inter = None } ;
                              xt_text = None;
                            }
                    in
                      let pos_limit2 =
                        match q with
                          [] -> pos_limit
                        | next :: _ ->
                            next.ext_loc.Location.loc_start.Lexing.pos_cnum
                      in
                      let s = get_string_of_file ext_loc_end pos_limit2 in
                      let (maybe_more, comment_opt) =  My_ir.just_after_special !file_name s in
                        new_xt.xt_text <- comment_opt;
                        analyse_extension_constructors maybe_more (new_xt :: exts_acc) q
          in
            let (maybe_more, exts) = analyse_extension_constructors 0 [] tt_tyext.tyext_constructors in
              new_te.te_constructors <- exts;
              (maybe_more, new_env, [ Element_type_extension new_te ])

      | Parsetree.Pstr_exception ext ->
          let name = ext.Parsetree.pext_name in
          (* a new exception is defined *)
          let complete_name = Name.concat current_module_name name.txt in
          (* we get the exception declaration in the typed tree *)
          let tt_ext =
            try Typedtree_search.search_exception table name.txt
            with Not_found ->
              raise (Failure (Odoc_messages.exception_not_found_in_typedtree complete_name))
          in
          let new_env = Odoc_env.add_extension env complete_name in
          let new_ext =
            match tt_ext.ext_kind with
              Text_decl(tt_args, tt_ret_type) ->
                let loc_start = loc.Location.loc_start.Lexing.pos_cnum in
                let loc_end =  loc.Location.loc_end.Lexing.pos_cnum in
                {
                  ex_name = complete_name ;
                  ex_info = comment_opt ;
                  ex_args =
                    List.map
                      (fun ctyp -> Odoc_env.subst_type new_env ctyp.ctyp_type)
                      tt_args;
                  ex_ret =
                    Misc.may_map
                      (fun ctyp -> Odoc_env.subst_type new_env ctyp.ctyp_type)
                      tt_ret_type;
                  ex_alias = None ;
                  ex_loc = { loc_impl = Some loc ; loc_inter = None } ;
                  ex_code =
                    (
                      if !Odoc_global.keep_code then
                        Some (get_string_of_file loc_start loc_end)
                      else
                        None
                    ) ;
                }
            | Text_rebind(tt_path, _) ->
                {
                  ex_name = complete_name ;
                  ex_info = comment_opt ;
                  ex_args = [] ;
                  ex_ret = None ;
                  ex_alias =
                    Some { ea_name =
                             Odoc_env.full_extension_constructor_name
                               env (Name.from_path tt_path) ;
                           ea_ex = None ; } ;
                  ex_loc = { loc_impl = Some loc ; loc_inter = None } ;
                  ex_code = None ;
                }
          in
            (0, new_env, [ Element_exception new_ext ])

      | Parsetree.Pstr_module {Parsetree.pmb_name=name; pmb_expr=module_expr} ->
          (
           (* of string * module_expr *)
           try
             let tt_module_expr = Typedtree_search.search_module table name.txt in
             let new_module_pre = analyse_module
                 env
                 current_module_name
                 name.txt
                 comment_opt
                 module_expr
                 tt_module_expr
             in
             let code =
               if !Odoc_global.keep_code then
                 let loc = module_expr.Parsetree.pmod_loc in
                 let st = loc.Location.loc_start.Lexing.pos_cnum in
                 let en = loc.Location.loc_end.Lexing.pos_cnum in
                 Some (get_string_of_file st en)
               else
                 None
             in
             let new_module =
               { new_module_pre with m_code = code }
             in
             let new_env = Odoc_env.add_module env new_module.m_name in
             let new_env2 =
               match new_module.m_type with
                 (* A VOIR : cela peut-il etre Tmty_ident ? dans ce cas, on aurait pas la signature *)
                 Types.Mty_signature s ->
                   Odoc_env.add_signature new_env new_module.m_name
                     ~rel: (Name.simple new_module.m_name) s
               | _ ->
                   new_env
             in
             (0, new_env2, [ Element_module new_module ])
           with
             Not_found ->
               let complete_name = Name.concat current_module_name name.txt in
               raise (Failure (Odoc_messages.module_not_found_in_typedtree complete_name))
          )

      | Parsetree.Pstr_recmodule mods ->
          (* A VOIR ICI pb: pas de lien avec les module type
             dans les contraintes sur les modules *)
          let new_env =
            List.fold_left
              (fun acc_env {Parsetree.pmb_name=name;pmb_expr=mod_exp} ->
                let complete_name = Name.concat current_module_name name.txt in
                let e = Odoc_env.add_module acc_env complete_name in
                let tt_mod_exp =
                  try Typedtree_search.search_module table name.txt
                  with Not_found -> raise (Failure (Odoc_messages.module_not_found_in_typedtree complete_name))
                in
                let new_module = analyse_module
                    e
                    current_module_name
                    name.txt
                    None
                    mod_exp
                    tt_mod_exp
                in
                match new_module.m_type with
                  Types.Mty_signature s ->
                    Odoc_env.add_signature e new_module.m_name
                      ~rel: (Name.simple new_module.m_name) s
                  | _ ->
                      e
              )
              env
              mods
          in
          let rec f ?(first=false) last_pos name_mod_exp_list =
            match name_mod_exp_list with
              [] -> []
            | {Parsetree.pmb_name=name;pmb_expr=mod_exp} :: q ->
                let complete_name = Name.concat current_module_name name.txt in
                let loc_start = mod_exp.Parsetree.pmod_loc.Location.loc_start.Lexing.pos_cnum in
                let loc_end =  mod_exp.Parsetree.pmod_loc.Location.loc_end.Lexing.pos_cnum in
                let tt_mod_exp =
                  try Typedtree_search.search_module table name.txt
                  with Not_found -> raise (Failure (Odoc_messages.module_not_found_in_typedtree complete_name))
                in
                let (com_opt, ele_comments) = (* the comment for the first type was already retrieved *)
                  if first then
                    (comment_opt, [])
                  else
                    get_comments_in_module last_pos loc_start
                in
                let new_module = analyse_module
                    new_env
                    current_module_name
                    name.txt
                    com_opt
                    mod_exp
                    tt_mod_exp
                in
                let eles = f loc_end q in
                ele_comments @ ((Element_module new_module) :: eles)
          in
          let eles = f ~first: true loc.Location.loc_start.Lexing.pos_cnum mods in
          (0, new_env, eles)

      | Parsetree.Pstr_modtype {Parsetree.pmtd_name=name; pmtd_type=modtype} ->
          let complete_name = Name.concat current_module_name name.txt in
          let tt_module_type =
            try Typedtree_search.search_module_type table name.txt
            with Not_found ->
              raise (Failure (Odoc_messages.module_type_not_found_in_typedtree complete_name))
          in
          let kind, sig_mtype =
            match modtype, tt_module_type.mtd_type with
            | Some modtype, Some mty_type ->
                Some (Sig.analyse_module_type_kind env complete_name
                        modtype mty_type.mty_type),
                Some mty_type.mty_type
            | _ -> None, None
          in
          let mt =
            {
              mt_name = complete_name ;
              mt_info = comment_opt ;
              mt_type = sig_mtype ;
              mt_is_interface = false ;
              mt_file = !file_name ;
              mt_kind = kind ;
              mt_loc = { loc_impl = Some loc ; loc_inter = None } ;
            }
          in
          let new_env = Odoc_env.add_module_type env mt.mt_name in
          let new_env2 =
            match sig_mtype with
              (* A VOIR : cela peut-il etre Tmty_ident ? dans ce cas, on n'aurait pas la signature *)
              Some (Types.Mty_signature s) ->
                Odoc_env.add_signature new_env mt.mt_name ~rel: (Name.simple mt.mt_name) s
            | _ ->
                new_env
          in
          (0, new_env2, [ Element_module_type mt ])

      | Parsetree.Pstr_open _ ->
          (* A VOIR : enrichir l'environnement quand open ? *)
          let ele_comments = match comment_opt with
            None -> []
          | Some i ->
              match i.i_desc with
                None -> []
              | Some t -> [Element_module_comment t]
          in
          (0, env, ele_comments)

      | Parsetree.Pstr_class class_decl_list ->
          (* we start by extending the environment *)
          let new_env =
            List.fold_left
              (fun acc_env -> fun class_decl ->
                let complete_name = Name.concat current_module_name class_decl.Parsetree.pci_name.txt in
                Odoc_env.add_class acc_env complete_name
              )
              env
              class_decl_list
          in
          let rec f ?(first=false) last_pos class_decl_list =
            match class_decl_list with
              [] ->
                []
            | class_decl :: q ->
                let (tt_class_exp, tt_type_params) =
                  try Typedtree_search.search_class_exp table class_decl.Parsetree.pci_name.txt
                  with Not_found ->
                    let complete_name = Name.concat current_module_name class_decl.Parsetree.pci_name.txt in
                    raise (Failure (Odoc_messages.class_not_found_in_typedtree complete_name))
                in
                let (com_opt, ele_comments) =
                  if first then
                    (comment_opt, [])
                  else
                    get_comments_in_module last_pos class_decl.Parsetree.pci_loc.Location.loc_start.Lexing.pos_cnum
                in
                let last_pos2 = class_decl.Parsetree.pci_loc.Location.loc_end.Lexing.pos_cnum in
                let new_class = analyse_class
                    new_env
                    current_module_name
                    com_opt
                    class_decl
                    tt_type_params
                    tt_class_exp
                    table
                in
                ele_comments @ ((Element_class new_class) :: (f last_pos2 q))
          in
          (0, new_env, f ~first: true loc.Location.loc_start.Lexing.pos_cnum class_decl_list)

      | Parsetree.Pstr_class_type class_type_decl_list ->
          (* we start by extending the environment *)
          let new_env =
            List.fold_left
              (fun acc_env -> fun class_type_decl ->
                let complete_name = Name.concat current_module_name class_type_decl.Parsetree.pci_name.txt in
                Odoc_env.add_class_type acc_env complete_name
              )
              env
              class_type_decl_list
          in
          let rec f ?(first=false) last_pos class_type_decl_list =
            match class_type_decl_list with
              [] ->
                []
            | class_type_decl :: q ->
                let name = class_type_decl.Parsetree.pci_name in
                let complete_name = Name.concat current_module_name name.txt in
                let virt = class_type_decl.Parsetree.pci_virt = Asttypes.Virtual in
                let tt_cltype_declaration =
                  try Typedtree_search.search_class_type_declaration table name.txt
                  with Not_found ->
                    raise (Failure (Odoc_messages.class_type_not_found_in_typedtree complete_name))
                  in
                  let tt_cltype_declaration = tt_cltype_declaration.ci_type_decl in
                let type_params = tt_cltype_declaration.Types.clty_params in
                let kind = Sig.analyse_class_type_kind
                    new_env
                    complete_name
                    class_type_decl.Parsetree.pci_loc.Location.loc_start.Lexing.pos_cnum
                    class_type_decl.Parsetree.pci_expr
                    tt_cltype_declaration.Types.clty_type
                in
                let (com_opt, ele_comments) =
                  if first then
                    (comment_opt, [])
                  else
                    get_comments_in_module last_pos class_type_decl.Parsetree.pci_loc.Location.loc_start.Lexing.pos_cnum
                in
                let last_pos2 = class_type_decl.Parsetree.pci_loc.Location.loc_end.Lexing.pos_cnum in
                let new_ele =
                  Element_class_type
                    {
                      clt_name = complete_name ;
                      clt_info = com_opt ;
                      clt_type = Odoc_env.subst_class_type env tt_cltype_declaration.Types.clty_type ;
                      clt_type_parameters = List.map (Odoc_env.subst_type new_env) type_params ;
                      clt_virtual = virt ;
                      clt_kind = kind ;
                      clt_loc = { loc_impl = Some loc ;
                                  loc_inter = None } ;
                    }
                in
                ele_comments @ (new_ele :: (f last_pos2 q))
          in
          (0, new_env, f ~first: true loc.Location.loc_start.Lexing.pos_cnum class_type_decl_list)

      | Parsetree.Pstr_include incl ->
          (* we add a dummy included module which will be replaced by a correct
             one at the end of the module analysis,
             to use the Path.t of the included modules in the typdtree. *)
          let im =
            {
              im_name = "dummy" ;
              im_module = None ;
              im_info = comment_opt ;
            }
          in
          (0, env, [ Element_included_module im ]) (* A VOIR : etendre l'environnement ? avec quoi ? *)

     (** Analysis of a [Parsetree.module_expr] and a name to return a [t_module].*)
     and analyse_module env current_module_name module_name comment_opt p_module_expr tt_module_expr =
      let complete_name = Name.concat current_module_name module_name in
      let loc = p_module_expr.Parsetree.pmod_loc in
      let pos_start = loc.Location.loc_start.Lexing.pos_cnum in
      let pos_end = loc.Location.loc_end.Lexing.pos_cnum in
      let modtype =
        (* A VOIR : Odoc_env.subst_module_type env  ? *)
        tt_module_expr.Typedtree.mod_type
      in
      let m_code_intf =
        match p_module_expr.Parsetree.pmod_desc with
          Parsetree.Pmod_constraint (_, pmodule_type) ->
            let loc_start = pmodule_type.Parsetree.pmty_loc.Location.loc_start.Lexing.pos_cnum in
            let loc_end = pmodule_type.Parsetree.pmty_loc.Location.loc_end.Lexing.pos_cnum in
            Some (get_string_of_file loc_start loc_end)
        | _ ->
            None
      in
      let m_base =
        {
          m_name = complete_name ;
          m_type = modtype ;
          m_info = comment_opt ;
          m_is_interface = false ;
          m_file = !file_name ;
          m_kind = Module_struct [] ;
          m_loc = { loc_impl = Some loc ; loc_inter = None } ;
          m_top_deps = [] ;
          m_code = None ; (* code is set by the caller, after the module is created *)
          m_code_intf = m_code_intf ;
          m_text_only = false ;
      }
      in
      match (p_module_expr.Parsetree.pmod_desc, tt_module_expr.Typedtree.mod_desc) with
        (Parsetree.Pmod_ident longident, Typedtree.Tmod_ident (path, _))
        | (Parsetree.Pmod_ident longident,
           Typedtree.Tmod_constraint
             ({Typedtree.mod_desc = Typedtree.Tmod_ident (path, _)}, _, _, _))
          ->
          let alias_name = Odoc_env.full_module_name env (Name.from_path path) in
          { m_base with m_kind = Module_alias { ma_name = alias_name ;
                                                ma_module = None ; } }

      | (Parsetree.Pmod_structure p_structure, Typedtree.Tmod_structure tt_structure) ->
          let elements = analyse_structure env complete_name pos_start pos_end p_structure tt_structure in
          (* we must complete the included modules *)
          let included_modules_from_tt = tt_get_included_module_list tt_structure in
          let elements2 = replace_dummy_included_modules elements included_modules_from_tt in
          { m_base with m_kind = Module_struct elements2 }

      | (Parsetree.Pmod_functor (_, pmodule_type, p_module_expr2),
         Typedtree.Tmod_functor (ident, _, mtyp, tt_module_expr2)) ->
           let loc = match pmodule_type with None -> Location.none
                     | Some pmty -> pmty.Parsetree.pmty_loc in
           let loc_start = loc.Location.loc_start.Lexing.pos_cnum in
           let loc_end = loc.Location.loc_end.Lexing.pos_cnum in
           let mp_type_code = get_string_of_file loc_start loc_end in
           print_DEBUG (Printf.sprintf "mp_type_code=%s" mp_type_code);
           let mp_name = Name.from_ident ident in
           let mp_kind =
             match pmodule_type, mtyp with
               Some pmty, Some mty ->
                 Sig.analyse_module_type_kind env current_module_name pmty
                   mty.mty_type
             | _ -> Module_type_struct []
           in
           let param =
             {
               mp_name = mp_name ;
               mp_type = Misc.may_map
                (fun m -> Odoc_env.subst_module_type env m.mty_type) mtyp ;
               mp_type_code = mp_type_code ;
               mp_kind = mp_kind ;
             }
           in
           let dummy_complete_name = (*Name.concat "__"*) param.mp_name in
           (* TODO: A VOIR CE __ *)
           let new_env = Odoc_env.add_module env dummy_complete_name in
           let m_base2 = analyse_module
               new_env
               current_module_name
               module_name
               None
               p_module_expr2
               tt_module_expr2
           in
           let kind = m_base2.m_kind in
           { m_base with m_kind = Module_functor (param, kind) }

      | (Parsetree.Pmod_apply (p_module_expr1, p_module_expr2),
         Typedtree.Tmod_apply (tt_module_expr1, tt_module_expr2, _))
      | (Parsetree.Pmod_apply (p_module_expr1, p_module_expr2),
         Typedtree.Tmod_constraint
           ({ Typedtree.mod_desc = Typedtree.Tmod_apply (tt_module_expr1, tt_module_expr2, _)}, _,
            _, _)
        ) ->
          let m1 = analyse_module
              env
              current_module_name
              module_name
              None
              p_module_expr1
              tt_module_expr1
          in
          let m2 = analyse_module
              env
              current_module_name
              module_name
              None
              p_module_expr2
              tt_module_expr2
          in
          { m_base with m_kind = Module_apply (m1.m_kind, m2.m_kind) }

      | (Parsetree.Pmod_constraint (p_module_expr2, p_modtype),
         Typedtree.Tmod_constraint (tt_module_expr2, tt_modtype, _, _)) ->
          print_DEBUG ("Odoc_ast: case Parsetree.Pmod_constraint + Typedtree.Tmod_constraint "^module_name);
          let m_base2 = analyse_module
              env
              current_module_name
              module_name
              None
              p_module_expr2
              tt_module_expr2
          in
          let mtkind = Sig.analyse_module_type_kind env
              (Name.concat current_module_name "??")
              p_modtype tt_modtype
          in
          let tt_modtype = Odoc_env.subst_module_type env tt_modtype in
          if !Odoc_global.filter_with_module_constraints then
            filter_module_with_module_type_constraint m_base2 tt_modtype;
          {
            m_base with
            m_type = tt_modtype ;
            m_kind = Module_constraint (m_base2.m_kind, mtkind) ;
          }

      | (Parsetree.Pmod_structure p_structure,
         Typedtree.Tmod_constraint
           ({ Typedtree.mod_desc = Typedtree.Tmod_structure tt_structure},
            tt_modtype, _, _)
        ) ->
          (* needed for recursive modules *)

          print_DEBUG ("Odoc_ast: case Parsetree.Pmod_structure + Typedtree.Tmod_constraint "^module_name);
          let elements = analyse_structure env complete_name pos_start pos_end p_structure tt_structure in
          (* we must complete the included modules *)
          let included_modules_from_tt = tt_get_included_module_list tt_structure in
          let elements2 = replace_dummy_included_modules elements included_modules_from_tt in
          { m_base with
            m_type = Odoc_env.subst_module_type env tt_modtype ;
            m_kind = Module_struct elements2 ;
          }

      | (Parsetree.Pmod_unpack p_exp,
         Typedtree.Tmod_unpack (t_exp, tt_modtype)) ->
          print_DEBUG ("Odoc_ast: case Parsetree.Pmod_unpack + Typedtree.Tmod_unpack "^module_name);
          let code =
            let loc = p_module_expr.Parsetree.pmod_loc in
            let loc_end = loc.Location.loc_end.Lexing.pos_cnum in
            let exp_loc = p_exp.Parsetree.pexp_loc in
            let exp_loc_end = exp_loc.Location.loc_end.Lexing.pos_cnum in
            let s = get_string_of_file exp_loc_end loc_end in
            Printf.sprintf "(val ...%s" s
          in
          (* let name = Odoc_env.full_module_type_name env (Name.from_path (fst pkg_type)) in *)
          let name =
            match tt_modtype with
            | Mty_ident p ->
                Odoc_env.full_module_type_name env (Name.from_path p)
            | _ -> ""
          in
          let alias = { mta_name = name ; mta_module = None } in
          { m_base with
            m_type = Odoc_env.subst_module_type env tt_modtype ;
            m_kind = Module_unpack (code, alias) ;
          }

      | (parsetree, typedtree) ->
          (*DEBUG*)let s_parse =
          (*DEBUG*)  match parsetree with
          (*DEBUG*)    Parsetree.Pmod_ident _ -> "Pmod_ident"
          (*DEBUG*)  | Parsetree.Pmod_structure _ -> "Pmod_structure"
          (*DEBUG*)  | Parsetree.Pmod_functor _ -> "Pmod_functor"
          (*DEBUG*)  | Parsetree.Pmod_apply _ -> "Pmod_apply"
          (*DEBUG*)  | Parsetree.Pmod_constraint _ -> "Pmod_constraint"
          (*DEBUG*)  | Parsetree.Pmod_unpack _ -> "Pmod_unpack"
          (*DEBUG*)  | Parsetree.Pmod_extension _ -> "Pmod_extension"
          (*DEBUG*)in
          (*DEBUG*)let s_typed =
          (*DEBUG*)  match typedtree with
          (*DEBUG*)    Typedtree.Tmod_ident _ -> "Tmod_ident"
          (*DEBUG*)  | Typedtree.Tmod_structure _ -> "Tmod_structure"
          (*DEBUG*)  | Typedtree.Tmod_functor _ -> "Tmod_functor"
          (*DEBUG*)  | Typedtree.Tmod_apply _ -> "Tmod_apply"
          (*DEBUG*)  | Typedtree.Tmod_constraint _ -> "Tmod_constraint"
          (*DEBUG*)  | Typedtree.Tmod_unpack _ -> "Tmod_unpack"
          (*DEBUG*)in
          (*DEBUG*)let code = get_string_of_file pos_start pos_end in
          print_DEBUG (Printf.sprintf "code=%s\ns_parse=%s\ns_typed=%s\n" code s_parse s_typed);

          raise (Failure "analyse_module: parsetree and typedtree don't match.")

     let analyse_typed_tree source_file input_file
         (parsetree : Parsetree.structure) (typedtree : typedtree) =
       let (tree_structure, _) = typedtree in
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
       let mod_name = String.capitalize (Filename.basename (Filename.chop_extension source_file)) in
       let (len,info_opt) = My_ir.first_special !file_name !file in

       (* we must complete the included modules *)
       let elements = analyse_structure Odoc_env.empty mod_name len (String.length !file) parsetree tree_structure in
       let included_modules_from_tt = tt_get_included_module_list tree_structure in
       let elements2 = replace_dummy_included_modules elements included_modules_from_tt in
       let kind = Module_struct elements2 in
       {
         m_name = mod_name ;
         m_type = Types.Mty_signature [] ;
         m_info = info_opt ;
         m_is_interface = false ;
         m_file = !file_name ;
         m_kind = kind ;
         m_loc = { loc_impl = Some (Location.in_file !file_name) ; loc_inter = None } ;
         m_top_deps = [] ;
         m_code = (if !Odoc_global.keep_code then Some !file else None) ;
         m_code_intf = None ;
         m_text_only = false ;
       }
  end
