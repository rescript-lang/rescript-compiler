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

(** Merge of information from [.ml] and [.mli] for a module.*)

open Odoc_types

module Name = Odoc_name
open Odoc_parameter
open Odoc_value
open Odoc_type
open Odoc_extension
open Odoc_exception
open Odoc_class
open Odoc_module

let merge_before_tags l =
  let rec iter acc = function
    [] -> List.rev acc
  | (v, text) :: q ->
      let (l1, l2) = List.partition
        (fun (v2,_) -> v = v2) q
      in
      let acc =
        let text =
          List.fold_left
            (fun acc t -> acc @ [Raw " "] @ t)
            text (List.map snd l1)
        in
        (v, text) :: acc
      in
      iter acc l2
  in
  iter [] l
;;

let version_separators = Str.regexp "[\\.\\+]";;

(** Merge two Odoctypes.info struture, completing the information of
   the first one with the information in the second one.
   The merge treatment depends on a given merge_option list.
   @return the new info structure.*)
let merge_info merge_options (m1 : info) (m2 : info) =
  let new_desc_opt =
    match m1.i_desc, m2.i_desc with
      None, None -> None
    | None, Some d
    | Some d, None -> Some d
    | Some d1, Some d2 ->
        if List.mem Merge_description merge_options then
          Some (d1 @ (Newline :: d2))
        else
          Some d1
  in
  let new_authors =
    match m1.i_authors, m2.i_authors with
      [], [] -> []
    | l, []
    | [], l -> l
    | l1, l2 ->
        if List.mem Merge_author merge_options then
          l1 @ l2
        else
          l1
  in
  let new_version =
    match m1.i_version , m2.i_version with
      None, None -> None
    | Some v, None
    | None, Some v -> Some v
    | Some v1, Some v2 ->
        if List.mem Merge_version merge_options then
          Some (v1^" "^v2)
        else
          Some v1
  in
  let new_sees =
    match m1.i_sees, m2.i_sees with
      [], [] -> []
    | l, []
    | [], l -> l
    | l1, l2 ->
        if List.mem Merge_see merge_options then
          l1 @ l2
        else
          l1
  in
  let new_since =
    match m1.i_since, m2.i_since with
      None, None -> None
    | Some v, None
    | None, Some v -> Some v
    | Some v1, Some v2 ->
        if List.mem Merge_since merge_options then
          Some (v1^" "^v2)
        else
          Some v1
  in
  let new_before =
    match m1.i_before, m2.i_before with
      [], [] -> []
    | l, []
    | [], l -> l
    | l1, l2 ->
        if List.mem Merge_before merge_options then
          merge_before_tags (m1.i_before @ m2.i_before)
        else
          l1 in
  let new_before = List.map (fun (v, t) -> (Str.split version_separators v, v, t)) new_before in
  let new_before = List.sort Pervasives.compare new_before in
  let new_before = List.map (fun (_, v, t) -> (v, t)) new_before in
  let new_dep =
    match m1.i_deprecated, m2.i_deprecated with
      None, None -> None
    | None, Some t
    | Some t, None -> Some t
    | Some t1, Some t2 ->
        if List.mem Merge_deprecated merge_options then
          Some (t1 @ (Newline :: t2))
        else
          Some t1
  in
  let new_params =
    match m1.i_params, m2.i_params with
      [], [] -> []
    | l, []
    | [], l -> l
    | l1, l2 ->
        if List.mem Merge_param merge_options then
          (
           let l_in_m1_and_m2, l_in_m2_only = List.partition
               (fun (param2, _) -> List.mem_assoc param2 l1)
               l2
           in
           let rec iter = function
               [] -> []
             | (param2, desc2) :: q ->
                 let desc1 = List.assoc param2 l1 in
                 (param2, desc1 @ (Newline :: desc2)) :: (iter q)
           in
           let l1_completed = iter l_in_m1_and_m2 in
           l1_completed @ l_in_m2_only
          )
        else
          l1
  in
  let new_raised_exceptions =
    match m1.i_raised_exceptions, m2.i_raised_exceptions with
      [], [] -> []
    | l, []
    | [], l -> l
    | l1, l2 ->
        if List.mem Merge_raised_exception merge_options then
          (
           let l_in_m1_and_m2, l_in_m2_only = List.partition
               (fun (exc2, _) -> List.mem_assoc exc2 l1)
               l2
           in
           let rec iter = function
               [] -> []
             | (exc2, desc2) :: q ->
                 let desc1 = List.assoc exc2 l1 in
                 (exc2, desc1 @ (Newline :: desc2)) :: (iter q)
           in
           let l1_completed = iter l_in_m1_and_m2 in
           l1_completed @ l_in_m2_only
          )
        else
          l1
  in
  let new_rv =
    match m1.i_return_value, m2.i_return_value with
      None, None -> None
    | None, Some t
    | Some t, None -> Some t
    | Some t1, Some t2 ->
        if List.mem Merge_return_value merge_options then
          Some (t1 @ (Newline :: t2))
        else
          Some t1
  in
  let new_custom =
    match m1.i_custom, m2.i_custom with
      [], [] -> []
    | [], l
    | l, [] -> l
    | l1, l2 ->
        if List.mem Merge_custom merge_options then
          l1 @ l2
        else
          l1
  in
  {
    Odoc_types.i_desc = new_desc_opt ;
    Odoc_types.i_authors = new_authors ;
    Odoc_types.i_version = new_version ;
    Odoc_types.i_sees = new_sees ;
    Odoc_types.i_since = new_since ;
    Odoc_types.i_before = new_before ;
    Odoc_types.i_deprecated = new_dep ;
    Odoc_types.i_params = new_params ;
    Odoc_types.i_raised_exceptions = new_raised_exceptions ;
    Odoc_types.i_return_value = new_rv ;
    Odoc_types.i_custom = new_custom ;
  }

(** Merge of two optional info structures. *)
let merge_info_opt merge_options mli_opt ml_opt =
  match mli_opt, ml_opt with
    None, Some i -> Some i
  | Some i, None -> Some i
  | None, None -> None
  | Some i1, Some i2 -> Some (merge_info merge_options i1 i2)

(** merge of two t_type, one for a .mli, another for the .ml.
   The .mli type is completed with the information in the .ml type. *)
let merge_types merge_options mli ml =
  mli.ty_info <- merge_info_opt merge_options mli.ty_info ml.ty_info;
  mli.ty_loc <- { mli.ty_loc with loc_impl = ml.ty_loc.loc_impl } ;
  mli.ty_code <- (match mli.ty_code with None -> ml.ty_code | _ -> mli.ty_code) ;

  match mli.ty_kind, ml.ty_kind with
    Type_abstract, _ ->
      ()

  | Type_variant l1, Type_variant l2 ->
      let f cons =
        try
          let cons2 = List.find
              (fun c2 -> c2.vc_name = cons.vc_name)
              l2
          in
          let new_desc =
            match cons.vc_text, cons2.vc_text with
              None, None -> None
            | Some d, None
            | None, Some d -> Some d
            | Some d1, Some d2 ->
                if List.mem Merge_description merge_options then
                  Some (merge_info merge_options d1 d2)
                else
                  Some d1
          in
          cons.vc_text <- new_desc
        with
          Not_found ->
            if !Odoc_global.inverse_merge_ml_mli then
              ()
            else
              raise (Failure (Odoc_messages.different_types mli.ty_name))
      in
      List.iter f l1

  | Type_record l1, Type_record l2 ->
      let f record =
        try
          let record2= List.find
              (fun r -> r.rf_name = record.rf_name)
              l2
          in
          let new_desc =
            match record.rf_text, record2.rf_text with
              None, None -> None
            | Some d, None
            | None, Some d -> Some d
            | Some d1, Some d2 ->
                if List.mem Merge_description merge_options then
                  Some (merge_info merge_options d1 d2)
                else
                  Some d1
          in
          record.rf_text <- new_desc
        with
          Not_found ->
            if !Odoc_global.inverse_merge_ml_mli then
              ()
            else
              raise (Failure (Odoc_messages.different_types mli.ty_name))
      in
      List.iter f l1

  | Type_open, Type_open ->
      ()

  | _ ->
      if !Odoc_global.inverse_merge_ml_mli then
        ()
      else
        raise (Failure (Odoc_messages.different_types mli.ty_name))

(** merge of two t_type_extension, one for a .mli, another for the .ml.
   The .mli type is completed with the information in the .ml type.
   Information for the extension constructors is merged separately
   by [merge_extension_constructor]. *)
let merge_type_extension merge_options mli ml =
  mli.te_info <- merge_info_opt merge_options mli.te_info ml.te_info;
  mli.te_loc <- { mli.te_loc with loc_impl = ml.te_loc.loc_impl } ;
  mli.te_code <- (match mli.te_code with None -> ml.te_code | _ -> mli.te_code)

(** merge of two t_extension_constructor, one for a .mli, another for the .ml.
   The .mli type is completed with the information in the .ml type. *)
let merge_extension_constructor merge_options mli ml =
  let new_desc =
    match mli.xt_text, ml.xt_text with
      None, None -> None
    | Some d, None
    | None, Some d -> Some d
    | Some d1, Some d2 ->
      if List.mem Merge_description merge_options then
        Some (merge_info merge_options d1 d2)
      else
        Some d1
  in
    mli.xt_text <- new_desc


(** Merge of two param_info, one from a .mli, one from a .ml.
   The text fields are not handled but will be recreated from the
   i_params field of the info structure.
   Here, if a parameter in the .mli has no name, we take the one
   from the .ml. When two parameters have two different forms,
   we take the one from the .mli. *)
let rec merge_param_info pi_mli pi_ml =
  match (pi_mli, pi_ml) with
    (Simple_name sn_mli, Simple_name sn_ml) ->
      if sn_mli.sn_name = "" then
        Simple_name { sn_mli with sn_name = sn_ml.sn_name }
      else
        pi_mli
  | (Simple_name _, Tuple _) ->
      pi_mli
  | (Tuple (_, t_mli), Simple_name sn_ml) ->
      (* if we're here, then the tuple in the .mli has no parameter names ;
         then we take the name of the parameter of the .ml and the type of the .mli. *)
      Simple_name { sn_ml with sn_type = t_mli }

  | (Tuple (l_mli, t_mli), Tuple (l_ml, _)) ->
      (* if the two tuples have different lengths
         (which should not occurs), we return the pi_mli,
         without further investigation.*)
      if (List.length l_mli) <> (List.length l_ml) then
        pi_mli
      else
        let new_l = List.map2 merge_param_info l_mli l_ml in
        Tuple (new_l, t_mli)

(** Merge of the parameters of two functions/methods/classes, one for a .mli, another for a .ml.
   The prameters in the .mli are completed by the name in the .ml.*)
let rec merge_parameters param_mli param_ml =
  match (param_mli, param_ml) with
    ([], []) -> []
  | (l, []) | ([], l) -> l
  | ((pi_mli :: li), (pi_ml :: l)) ->
      (merge_param_info pi_mli pi_ml) :: merge_parameters li l

(** Merge of two t_class, one for a .mli, another for the .ml.
   The .mli class is completed with the information in the .ml class. *)
let merge_classes merge_options mli ml =
  mli.cl_info <- merge_info_opt merge_options mli.cl_info ml.cl_info;
  mli.cl_loc <- { mli.cl_loc with loc_impl = ml.cl_loc.loc_impl } ;
  mli.cl_parameters <- merge_parameters mli.cl_parameters ml.cl_parameters;

  (* we must reassociate comments in @param to the the corresponding
     parameters because the associated comment of a parameter may have been changed y the merge.*)
  Odoc_class.class_update_parameters_text mli;

  (* merge values *)
  List.iter
    (fun a ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Class_attribute a2 ->
                  if a2.att_value.val_name = a.att_value.val_name then
                    (
                     a.att_value.val_info <- merge_info_opt merge_options
                         a.att_value.val_info a2.att_value.val_info;
                     a.att_value.val_loc <- { a.att_value.val_loc with loc_impl = a2.att_value.val_loc.loc_impl } ;
                     if !Odoc_global.keep_code then
                       a.att_value.val_code <- a2.att_value.val_code;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last attribute with this name defined in the implementation *)
            (List.rev (Odoc_class.class_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_class.class_attributes mli);
  (* merge methods *)
  List.iter
    (fun m ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Class_method m2 ->
                  if m2.met_value.val_name = m.met_value.val_name then
                    (
                     m.met_value.val_info <- merge_info_opt
                         merge_options m.met_value.val_info m2.met_value.val_info;
                     m.met_value.val_loc <- { m.met_value.val_loc with loc_impl = m2.met_value.val_loc.loc_impl } ;
                     (* merge the parameter names *)
                     m.met_value.val_parameters <- (merge_parameters
                                                      m.met_value.val_parameters
                                                      m2.met_value.val_parameters) ;
                     (* we must reassociate comments in @param to the corresponding
                        parameters because the associated comment of a parameter may have been changed by the merge.*)
                     Odoc_value.update_value_parameters_text m.met_value;

                     if !Odoc_global.keep_code then
                       m.met_value.val_code <- m2.met_value.val_code;

                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last method with this name defined in the implementation *)
            (List.rev (Odoc_class.class_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_class.class_methods mli)

(** merge of two t_class_type, one for a .mli, another for the .ml.
   The .mli class is completed with the information in the .ml class. *)
let merge_class_types merge_options mli ml =
  mli.clt_info <- merge_info_opt merge_options  mli.clt_info ml.clt_info;
  mli.clt_loc <- { mli.clt_loc with loc_impl = ml.clt_loc.loc_impl } ;
  (* merge values *)
  List.iter
    (fun a ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Class_attribute a2 ->
                  if a2.att_value.val_name = a.att_value.val_name then
                    (
                     a.att_value.val_info <- merge_info_opt merge_options
                         a.att_value.val_info a2.att_value.val_info;
                     a.att_value.val_loc <- { a.att_value.val_loc with loc_impl = a2.att_value.val_loc.loc_impl } ;
                     if !Odoc_global.keep_code then
                       a.att_value.val_code <- a2.att_value.val_code;

                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last attribute with this name defined in the implementation *)
            (List.rev (Odoc_class.class_type_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_class.class_type_attributes mli);
  (* merge methods *)
  List.iter
    (fun m ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Class_method m2 ->
                  if m2.met_value.val_name = m.met_value.val_name then
                    (
                     m.met_value.val_info <- merge_info_opt
                         merge_options m.met_value.val_info m2.met_value.val_info;
                     m.met_value.val_loc <- { m.met_value.val_loc with loc_impl = m2.met_value.val_loc.loc_impl } ;
                     m.met_value.val_parameters <- (merge_parameters
                                                      m.met_value.val_parameters
                                                      m2.met_value.val_parameters) ;
                     (* we must reassociate comments in @param to the the corresponding
                        parameters because the associated comment of a parameter may have been changed y the merge.*)
                     Odoc_value.update_value_parameters_text m.met_value;

                     if !Odoc_global.keep_code then
                       m.met_value.val_code <- m2.met_value.val_code;

                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last method with this name defined in the implementation *)
            (List.rev (Odoc_class.class_type_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_class.class_type_methods mli)


(** merge of two t_module_type, one for a .mli, another for the .ml.
   The .mli module is completed with the information in the .ml module. *)
let rec merge_module_types merge_options mli ml =
  mli.mt_info <- merge_info_opt merge_options mli.mt_info ml.mt_info;
  mli.mt_loc <- { mli.mt_loc with loc_impl = ml.mt_loc.loc_impl } ;
  (* merge type extensions *)
  List.iter
    (fun te ->
       let rec f exts elems =
         match exts, elems with
             [], _
           | _, [] -> ()
           | _, (Element_type_extension te2 :: rest) ->
               let merge_ext xt =
                 try
                   let xt2 =
                     List.find (fun xt2 -> xt.xt_name = xt2.xt_name)
                       te2.te_constructors
                   in
                     merge_extension_constructor merge_options xt xt2;
                     true
                 with Not_found -> false
               in
               let merged, unmerged = List.partition merge_ext exts in
                 if merged <> [] then merge_type_extension merge_options te te2;
                 f unmerged rest
           | _, (_ :: rest) -> f exts rest
       in
         (* we look for the extensions in reverse order *)
         f te.te_constructors (List.rev (Odoc_module.module_type_elements ml))
    )
    (Odoc_module.module_type_type_extensions mli);
  (* merge exceptions *)
  List.iter
    (fun ex ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_exception ex2 ->
                  if ex2.ex_name = ex.ex_name then
                    (
                     ex.ex_info <- merge_info_opt merge_options ex.ex_info ex2.ex_info;
                     ex.ex_loc <- { ex.ex_loc with loc_impl = ex2.ex_loc.loc_impl } ;
                     ex.ex_code <- (match ex.ex_code with None -> ex2.ex_code | _ -> ex.ex_code) ;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last exception with this name defined in the implementation *)
            (List.rev (Odoc_module.module_type_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_type_exceptions mli);
  (* merge types *)
  List.iter
    (fun ty ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_type ty2 ->
                  if ty2.ty_name = ty.ty_name then
                    (
                     merge_types merge_options ty ty2;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last type with this name defined in the implementation *)
            (List.rev (Odoc_module.module_type_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_type_types mli);
  (* merge submodules *)
  List.iter
    (fun m ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_module m2 ->
                  if m2.m_name = m.m_name then
                    (
                     ignore (merge_modules merge_options m m2);
(*
                     m.m_info <- merge_info_opt merge_options m.m_info m2.m_info;
                     m.m_loc <- { m.m_loc with loc_impl = m2.m_loc.loc_impl } ;
*)
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last module with this name defined in the implementation *)
            (List.rev (Odoc_module.module_type_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_type_modules mli);

  (* merge module types *)
  List.iter
    (fun m ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_module_type m2 ->
                  if m2.mt_name = m.mt_name then
                    (
                     merge_module_types merge_options m m2;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last module with this name defined in the implementation *)
            (List.rev (Odoc_module.module_type_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_type_module_types mli);

  (* A VOIR : merge included modules ? *)

  (* merge values *)
  List.iter
    (fun v ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_value v2 ->
                  if v2.val_name = v.val_name then
                    (
                     v.val_info <- merge_info_opt merge_options v.val_info v2.val_info ;
                     v.val_loc <- { v.val_loc with loc_impl = v2.val_loc.loc_impl } ;
                     (* in the .mli we don't know any parameters so we add the ones in the .ml *)
                     v.val_parameters <- (merge_parameters
                                            v.val_parameters
                                            v2.val_parameters) ;
                     (* we must reassociate comments in @param to the the corresponding
                        parameters because the associated comment of a parameter may have been changed y the merge.*)
                     Odoc_value.update_value_parameters_text v;

                     if !Odoc_global.keep_code then
                       v.val_code <- v2.val_code;

                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last value with this name defined in the implementation *)
            (List.rev (Odoc_module.module_type_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_type_values mli);

  (* merge classes *)
  List.iter
    (fun c ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_class c2 ->
                  if c2.cl_name = c.cl_name then
                    (
                     merge_classes merge_options c c2;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last value with this name defined in the implementation *)
            (List.rev (Odoc_module.module_type_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_type_classes mli);

  (* merge class types *)
  List.iter
    (fun c ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_class_type c2 ->
                  if c2.clt_name = c.clt_name then
                    (
                     merge_class_types merge_options c c2;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last value with this name defined in the implementation *)
            (List.rev (Odoc_module.module_type_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_type_class_types mli)

(** merge of two t_module, one for a .mli, another for the .ml.
   The .mli module is completed with the information in the .ml module. *)
and merge_modules merge_options mli ml =
  mli.m_info <- merge_info_opt merge_options mli.m_info ml.m_info;
  mli.m_loc <- { mli.m_loc with loc_impl = ml.m_loc.loc_impl } ;
  let rec remove_doubles acc = function
      [] -> acc
    | h :: q ->
        if List.mem h acc then remove_doubles acc q
        else remove_doubles (h :: acc) q
  in
  mli.m_top_deps <- remove_doubles mli.m_top_deps ml.m_top_deps ;

  let code =
    if !Odoc_global.keep_code then
      match mli.m_code, ml.m_code with
        Some s, _ -> Some s
      | _, Some s -> Some s
      | _ -> None
    else
      None
  in
  let code_intf =
    if !Odoc_global.keep_code then
      match mli.m_code_intf, ml.m_code_intf with
        Some s, _ -> Some s
      | _, Some s -> Some s
      | _ -> None
    else
      None
  in
  mli.m_code <- code;
  mli.m_code_intf <- code_intf;

  (* merge type extensions *)
  List.iter
    (fun te ->
       let rec f exts elems =
         match exts, elems with
             [], _
           | _, [] -> ()
           | _, (Element_type_extension te2 :: rest) ->
               let merge_ext xt =
                 try
                   let xt2 =
                     List.find (fun xt2 -> xt.xt_name = xt2.xt_name)
                       te2.te_constructors
                   in
                     merge_extension_constructor merge_options xt xt2;
                     true
                 with Not_found -> false
               in
               let merged, unmerged = List.partition merge_ext exts in
                 if merged <> [] then merge_type_extension merge_options te te2;
                 f unmerged rest
           | _, (_ :: rest) -> f exts rest
       in
         (* we look for the extensions in reverse order *)
         f te.te_constructors (List.rev (Odoc_module.module_elements ml))
    )
    (Odoc_module.module_type_extensions mli);
  (* merge exceptions *)
  List.iter
    (fun ex ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_exception ex2 ->
                  if ex2.ex_name = ex.ex_name then
                    (
                     ex.ex_info <- merge_info_opt merge_options ex.ex_info ex2.ex_info;
                     ex.ex_loc <- { ex.ex_loc with loc_impl = ex.ex_loc.loc_impl } ;
                     ex.ex_code <- (match ex.ex_code with None -> ex2.ex_code | _ -> ex.ex_code) ;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last exception with this name defined in the implementation *)
            (List.rev (Odoc_module.module_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_exceptions mli);
  (* merge types *)
  List.iter
    (fun ty ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_type ty2 ->
                  if ty2.ty_name = ty.ty_name then
                    (
                     merge_types merge_options ty ty2;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last type with this name defined in the implementation *)
            (List.rev (Odoc_module.module_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_types mli);
  (* merge submodules *)
  List.iter
    (fun m ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_module m2 ->
                  if m2.m_name = m.m_name then
                    (
                     ignore (merge_modules merge_options m m2);
(*
                     m.m_info <- merge_info_opt merge_options m.m_info m2.m_info;
                     m.m_loc <- { m.m_loc with loc_impl = m2.m_loc.loc_impl } ;
*)
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last module with this name defined in the implementation *)
            (List.rev (Odoc_module.module_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_modules mli);

  (* merge module types *)
  List.iter
    (fun m ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_module_type m2 ->
                  if m2.mt_name = m.mt_name then
                    (
                     merge_module_types merge_options m m2;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last module with this name defined in the implementation *)
            (List.rev (Odoc_module.module_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_module_types mli);

  (* A VOIR : merge included modules ? *)

  (* merge values *)
  List.iter
    (fun v ->
      try
        let _ = List.find
            (fun v2 ->
              if v2.val_name = v.val_name then
                (
                 v.val_info <- merge_info_opt merge_options v.val_info v2.val_info ;
                 v.val_loc <- { v.val_loc with loc_impl = v2.val_loc.loc_impl } ;
                 (* in the .mli we don't know any parameters so we add the ones in the .ml *)
                 v.val_parameters <- (merge_parameters
                                        v.val_parameters
                                        v2.val_parameters) ;
                 (* we must reassociate comments in @param to the the corresponding
                    parameters because the associated comment of a parameter may have been changed y the merge.*)
                 Odoc_value.update_value_parameters_text v;

                 if !Odoc_global.keep_code then
                   v.val_code <- v2.val_code;
                 true
                )
              else
                false
            )
            (* we look for the last value with this name defined in the implementation *)
            (List.rev (Odoc_module.module_values ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_values mli);

  (* merge classes *)
  List.iter
    (fun c ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_class c2 ->
                  if c2.cl_name = c.cl_name then
                    (
                     merge_classes merge_options c c2;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last value with this name defined in the implementation *)
            (List.rev (Odoc_module.module_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_classes mli);

  (* merge class types *)
  List.iter
    (fun c ->
      try
        let _ = List.find
            (fun ele ->
              match ele with
                Element_class_type c2 ->
                  if c2.clt_name = c.clt_name then
                    (
                     merge_class_types merge_options c c2;
                     true
                    )
                  else
                    false
              | _ ->
                  false
            )
            (* we look for the last value with this name defined in the implementation *)
            (List.rev (Odoc_module.module_elements ml))
        in
        ()
      with
        Not_found ->
          ()
    )
    (Odoc_module.module_class_types mli);

  mli

let merge merge_options modules_list =
  let rec iter = function
      [] -> []
    | m :: q ->
        (* look for another module with the same name *)
        let (l_same, l_others) = List.partition
            (fun m2 -> m.m_name = m2.m_name)
            q
        in
        match l_same with
          [] ->
            (* no other module to merge with *)
            m :: (iter l_others)
        | m2 :: [] ->
            (
             (* we can merge m with m2 if there is an implementation
                and an interface.*)
             let f b = if !Odoc_global.inverse_merge_ml_mli then not b else b in
             match f m.m_is_interface, f m2.m_is_interface with
               true, false -> (merge_modules merge_options m m2) :: (iter l_others)
             | false, true -> (merge_modules merge_options m2 m) :: (iter l_others)
             | false, false ->
                 if !Odoc_global.inverse_merge_ml_mli then
                   (* two Module.ts for the .mli ! *)
                   raise (Failure (Odoc_messages.two_interfaces m.m_name))
                 else
                   (* two Module.t for the .ml ! *)
                   raise (Failure (Odoc_messages.two_implementations m.m_name))
             | true, true ->
                 if !Odoc_global.inverse_merge_ml_mli then
                   (* two Module.t for the .ml ! *)
                   raise (Failure (Odoc_messages.two_implementations m.m_name))
                 else
                   (* two Module.ts for the .mli ! *)
                   raise (Failure (Odoc_messages.two_interfaces m.m_name))
            )
        | _ ->
            (* too many Module.t ! *)
            raise (Failure (Odoc_messages.too_many_module_objects m.m_name))

  in
  iter modules_list
