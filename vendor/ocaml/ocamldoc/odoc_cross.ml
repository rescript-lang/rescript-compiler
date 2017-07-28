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

(** Cross referencing. *)

module Name = Odoc_name
open Odoc_module
open Odoc_class
open Odoc_extension
open Odoc_exception
open Odoc_types
open Odoc_value
open Odoc_type
open Odoc_parameter

(*** Replacements of aliases : if e1 = e2 and e2 = e3, then replace e2 by e3 to have e1 = e3,
   in order to associate the element with complete information. *)

(** The module used to keep what refs were modified. *)
module S = Set.Make
    (
     struct type t = string * ref_kind option
       let compare = Pervasives.compare
     end
    )

let verified_refs = ref S.empty

let add_verified v = verified_refs := S.add v !verified_refs
let was_verified v = S.mem v !verified_refs

(** The module with the predicates used to get the aliased modules, classes and exceptions. *)
module P_alias =
  struct
    type t = int

    let p_module m _ =
      (true,
       match m.m_kind with
         Module_alias _ -> true
       | _ -> false
      )
    let p_module_type mt _ =
      (true,
       match mt.mt_kind with
         Some (Module_type_alias _) -> true
       | _ -> false
      )
    let p_class c _ = (false, false)
    let p_class_type ct _ = (false, false)
    let p_value v _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type t _ = (false, false)
    let p_extension x _ = x.xt_alias <> None
    let p_exception e _ = e.ex_alias <> None
    let p_attribute a _ = false
    let p_method m _ = false
    let p_section s _ = false
  end

(** The module used to get the aliased elements. *)
module Search_alias = Odoc_search.Search (P_alias)

type alias_state =
    Alias_resolved
  | Alias_to_resolve

(** Couples of module name aliases. *)
let (module_aliases : (Name.t, Name.t * alias_state) Hashtbl.t) = Hashtbl.create 13 ;;

(** Couples of module or module type name aliases. *)
let module_and_modtype_aliases = Hashtbl.create 13;;

(** Couples of extension name aliases. *)
let extension_aliases = Hashtbl.create 13;;

(** Couples of exception name aliases. *)
let exception_aliases = Hashtbl.create 13;;

let rec build_alias_list = function
    [] -> ()
  | (Odoc_search.Res_module m) :: q ->
      (
       match m.m_kind with
         Module_alias ma ->
           Hashtbl.add module_aliases m.m_name (ma.ma_name, Alias_to_resolve);
           Hashtbl.add module_and_modtype_aliases m.m_name (ma.ma_name, Alias_to_resolve)
       | _ -> ()
      );
      build_alias_list q
  | (Odoc_search.Res_module_type mt) :: q ->
      (
       match mt.mt_kind with
         Some (Module_type_alias mta) ->
           Hashtbl.add module_and_modtype_aliases
             mt.mt_name (mta.mta_name, Alias_to_resolve)
       | _ -> ()
      );
      build_alias_list q
  | (Odoc_search.Res_extension x) :: q ->
      (
       match x.xt_alias with
         None -> ()
       | Some xa ->
           Hashtbl.add extension_aliases
             x.xt_name (xa.xa_name,Alias_to_resolve)
      );
      build_alias_list q
  | (Odoc_search.Res_exception e) :: q ->
      (
       match e.ex_alias with
         None -> ()
       | Some ea ->
           Hashtbl.add exception_aliases
             e.ex_name (ea.ea_name,Alias_to_resolve)
      );
      build_alias_list q
  | _ :: q ->
      build_alias_list q

(** Retrieve the aliases for modules, module types and exceptions
   and put them in global hash tables. *)
let get_alias_names module_list =
  Hashtbl.clear module_aliases;
  Hashtbl.clear module_and_modtype_aliases;
  Hashtbl.clear extension_aliases;
  Hashtbl.clear exception_aliases;
  build_alias_list (Search_alias.search module_list 0)

exception Found of string
let name_alias =
  let rec f t name =
    try
      match Hashtbl.find t name with
        (s, Alias_resolved) -> s
      | (s, Alias_to_resolve) -> f t s
    with
      Not_found ->
        try
          Hashtbl.iter
            (fun n2 (n3, _) ->
              if Name.prefix n2 name then
                let ln2 = String.length n2 in
                let s = n3^(String.sub name ln2 ((String.length name) - ln2)) in
                raise (Found s)
            )
            t ;
          Hashtbl.replace t name (name, Alias_resolved);
          name
        with
          Found s ->
            let s2 = f t s in
            Hashtbl.replace t s2 (s2, Alias_resolved);
            s2
  in
  fun name alias_tbl ->
    f alias_tbl name


module Map_ord =
  struct
    type t = string
    let compare (x:t) y = Pervasives.compare x y
  end

module Ele_map = Map.Make (Map_ord)

let known_elements = ref Ele_map.empty
let add_known_element name k =
  try
    let l = Ele_map.find name !known_elements in
    let s = Ele_map.remove name !known_elements in
    known_elements := Ele_map.add name (k::l) s
  with
    Not_found ->
      known_elements := Ele_map.add name [k] !known_elements

let rec get_known_elements name =
  try Ele_map.find name !known_elements
  with Not_found -> []

let kind_name_exists kind =
  let pred =
    match kind with
      RK_module -> (fun e -> match e with Odoc_search.Res_module _ -> true | _ -> false)
    | RK_module_type -> (fun e -> match e with Odoc_search.Res_module_type _ -> true | _ -> false)
    | RK_class -> (fun e -> match e with Odoc_search.Res_class _ -> true | _ -> false)
    | RK_class_type -> (fun e -> match e with Odoc_search.Res_class_type _ -> true | _ -> false)
    | RK_value -> (fun e -> match e with Odoc_search.Res_value _ -> true | _ -> false)
    | RK_type -> (fun e -> match e with Odoc_search.Res_type _ -> true | _ -> false)
    | RK_extension -> (fun e -> match e with Odoc_search.Res_extension _ -> true | _ -> false)
    | RK_exception -> (fun e -> match e with Odoc_search.Res_exception _ -> true | _ -> false)
    | RK_attribute -> (fun e -> match e with Odoc_search.Res_attribute _ -> true | _ -> false)
    | RK_method -> (fun e -> match e with Odoc_search.Res_method _ -> true | _ -> false)
    | RK_section _ -> assert false
    | RK_recfield -> (fun e -> match e with Odoc_search.Res_recfield _ -> true | _ -> false)
    | RK_const -> (fun e -> match e with Odoc_search.Res_const _ -> true | _ -> false)
  in
  fun name ->
    try List.exists pred (get_known_elements name)
    with Not_found -> false

let module_exists = kind_name_exists RK_module
let module_type_exists = kind_name_exists RK_module_type
let class_exists = kind_name_exists RK_class
let class_type_exists = kind_name_exists RK_class_type
let value_exists = kind_name_exists RK_value
let type_exists = kind_name_exists RK_type
let extension_exists = kind_name_exists RK_extension
let exception_exists = kind_name_exists RK_exception
let attribute_exists = kind_name_exists RK_attribute
let method_exists = kind_name_exists RK_method
let recfield_exists = kind_name_exists RK_recfield
let const_exists = kind_name_exists RK_const

let lookup_module name =
  match List.find
      (fun k -> match k with Odoc_search.Res_module _ -> true | _ -> false)
      (get_known_elements name)
  with
  | Odoc_search.Res_module m -> m
  | _ -> assert false

let lookup_module_type name =
  match List.find
      (fun k -> match k with Odoc_search.Res_module_type _ -> true | _ -> false)
      (get_known_elements name)
  with
  | Odoc_search.Res_module_type m -> m
  | _ -> assert false

let lookup_class name =
  match List.find
      (fun k -> match k with Odoc_search.Res_class _ -> true | _ -> false)
      (get_known_elements name)
  with
  | Odoc_search.Res_class c -> c
  | _ -> assert false

let lookup_class_type name =
  match List.find
      (fun k -> match k with Odoc_search.Res_class_type _ -> true | _ -> false)
      (get_known_elements name)
  with
  | Odoc_search.Res_class_type c -> c
  | _ -> assert false

let lookup_extension name =
  match List.find
      (fun k -> match k with Odoc_search.Res_extension _ -> true | _ -> false)
      (get_known_elements name)
  with
  | Odoc_search.Res_extension x -> x
  | _ -> assert false

let lookup_exception name =
  match List.find
      (fun k -> match k with Odoc_search.Res_exception _ -> true | _ -> false)
      (get_known_elements name)
  with
  | Odoc_search.Res_exception e -> e
  | _ -> assert false

class scan =
  object
    inherit Odoc_scan.scanner
    method! scan_value v =
      add_known_element v.val_name (Odoc_search.Res_value v)
    method! scan_type_recfield t f =
      add_known_element
        (Printf.sprintf "%s.%s" t.ty_name f.rf_name)
        (Odoc_search.Res_recfield (t, f))
    method! scan_type_const t f =
      add_known_element
        (Printf.sprintf "%s.%s" t.ty_name f.vc_name)
        (Odoc_search.Res_const (t, f))
    method! scan_type_pre t =
      add_known_element t.ty_name (Odoc_search.Res_type t);
      true
    method! scan_extension_constructor x =
      add_known_element x.xt_name (Odoc_search.Res_extension x)
    method! scan_exception e =
      add_known_element e.ex_name (Odoc_search.Res_exception e)
    method! scan_attribute a =
      add_known_element a.att_value.val_name
        (Odoc_search.Res_attribute a)
    method! scan_method m =
      add_known_element m.met_value.val_name
        (Odoc_search.Res_method m)
    method! scan_class_pre c =
      add_known_element c.cl_name (Odoc_search.Res_class c);
      true
    method! scan_class_type_pre c =
      add_known_element c.clt_name (Odoc_search.Res_class_type c);
      true
    method! scan_module_pre m =
      add_known_element m.m_name (Odoc_search.Res_module m);
      true
    method! scan_module_type_pre m =
      add_known_element m.mt_name (Odoc_search.Res_module_type m);
      true

  end

let init_known_elements_map module_list =
  let c = new scan in
  c#scan_module_list module_list


(** The type to describe the names not found. *)
type not_found_name =
    NF_m of Name.t
  | NF_mt of Name.t
  | NF_mmt of Name.t
  | NF_c of Name.t
  | NF_ct of Name.t
  | NF_cct of Name.t
  | NF_xt of Name.t
  | NF_ex of Name.t

(** Functions to find and associate aliases elements. *)

let rec associate_in_module module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) m =
  let rec iter_kind (acc_b, acc_inc, acc_names) k =
    match k with
      Module_struct elements ->
        List.fold_left
          (associate_in_module_element module_list m.m_name)
          (acc_b, acc_inc, acc_names)
          elements

    | Module_alias ma ->
        (
         match ma.ma_module with
           Some _ ->
             (acc_b, acc_inc, acc_names)
         | None ->
             let mmt_opt =
               try Some (Mod (lookup_module ma.ma_name))
               with Not_found ->
                 try Some (Modtype (lookup_module_type ma.ma_name))
                 with Not_found -> None
             in
             match mmt_opt with
               None -> (acc_b, (Name.head m.m_name) :: acc_inc,
                        (* we don't want to output warning messages for
                           "sig ... end" or "struct ... end" modules not found *)
                        (if ma.ma_name = Odoc_messages.struct_end ||
                          ma.ma_name = Odoc_messages.sig_end then
                          acc_names
                        else
                          (NF_mmt ma.ma_name) :: acc_names)
                       )
             | Some mmt ->
                 ma.ma_module <- Some mmt ;
                 (true, acc_inc, acc_names)
        )

    | Module_functor (_, k) ->
        iter_kind (acc_b, acc_inc, acc_names) k

    | Module_with (tk, _) ->
        associate_in_module_type module_list (acc_b, acc_inc, acc_names)
          { mt_name = "" ; mt_info = None ; mt_type = None ;
            mt_is_interface = false ; mt_file = ""; mt_kind = Some tk ;
            mt_loc = Odoc_types.dummy_loc }

    | Module_apply (k1, k2) ->
        let (acc_b2, acc_inc2, acc_names2) = iter_kind (acc_b, acc_inc, acc_names) k1 in
        iter_kind (acc_b2, acc_inc2, acc_names2) k2

    | Module_constraint (k, tk) ->
        let (acc_b2, acc_inc2, acc_names2) = iter_kind (acc_b, acc_inc, acc_names) k in
        associate_in_module_type module_list (acc_b2, acc_inc2, acc_names2)
          { mt_name = "" ; mt_info = None ; mt_type = None ;
            mt_is_interface = false ; mt_file = "" ; mt_kind = Some tk ;
            mt_loc = Odoc_types.dummy_loc }

     | Module_typeof _ ->
        (acc_b, acc_inc, acc_names)

     | Module_unpack (code, mta) ->
        begin
          match mta.mta_module with
            Some _ ->
              (acc_b, acc_inc, acc_names)
          | None ->
              let mt_opt =
                try Some (lookup_module_type mta.mta_name)
                with Not_found -> None
              in
              match mt_opt with
                None -> (acc_b, (Name.head m.m_name) :: acc_inc,
                   (* we don't want to output warning messages for
                      "sig ... end" or "struct ... end" modules not found *)
                   (if mta.mta_name = Odoc_messages.struct_end ||
                      mta.mta_name = Odoc_messages.sig_end then
                      acc_names
                    else
                      (NF_mt mta.mta_name) :: acc_names)
                  )
              | Some mt ->
                  mta.mta_module <- Some mt ;
                  (true, acc_inc, acc_names)
        end
  in
  iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) m.m_kind

and associate_in_module_type module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) mt =
  let rec iter_kind (acc_b, acc_inc, acc_names) k =
    match k with
      Module_type_struct elements ->
        List.fold_left
          (associate_in_module_element module_list mt.mt_name)
          (acc_b, acc_inc, acc_names)
          elements

    | Module_type_functor (_, k) ->
        iter_kind (acc_b, acc_inc, acc_names) k

    | Module_type_with (k, _) ->
        iter_kind (acc_b, acc_inc, acc_names) k

    | Module_type_alias mta ->
        begin
          match mta.mta_module with
            Some _ ->
              (acc_b, acc_inc, acc_names)
          | None ->
              let mt_opt =
                try Some (lookup_module_type mta.mta_name)
                with Not_found -> None
              in
              match mt_opt with
                None -> (acc_b, (Name.head mt.mt_name) :: acc_inc,
                   (* we don't want to output warning messages for
                      "sig ... end" or "struct ... end" modules not found *)
                   (if mta.mta_name = Odoc_messages.struct_end ||
                      mta.mta_name = Odoc_messages.sig_end then
                      acc_names
                    else
                      (NF_mt mta.mta_name) :: acc_names)
                  )
              | Some mt ->
                  mta.mta_module <- Some mt ;
                  (true, acc_inc, acc_names)
        end
    | Module_type_typeof _ ->
        (acc_b, acc_inc, acc_names)
  in
  match mt.mt_kind with
    None -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
  | Some k -> iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) k

and associate_in_module_element module_list m_name (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) element =
   match element with
     Element_module m -> associate_in_module module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) m
   | Element_module_type mt -> associate_in_module_type module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) mt
   | Element_included_module im ->
       (
        match im.im_module with
          Some _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
        | None ->
            let mmt_opt =
              try Some (Mod (lookup_module im.im_name))
              with Not_found ->
                try Some (Modtype (lookup_module_type im.im_name))
                with Not_found -> None
            in
            match mmt_opt with
              None -> (acc_b_modif, (Name.head m_name) :: acc_incomplete_top_module_names,
                       (* we don't want to output warning messages for
                           "sig ... end" or "struct ... end" modules not found *)
                        (if im.im_name = Odoc_messages.struct_end ||
                          im.im_name = Odoc_messages.sig_end then
                          acc_names_not_found
                        else
                          (NF_mmt im.im_name) :: acc_names_not_found)
                      )
            | Some mmt ->
                im.im_module <- Some mmt ;
                (true, acc_incomplete_top_module_names, acc_names_not_found)
       )
   | Element_class cl -> associate_in_class module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) cl
   | Element_class_type ct -> associate_in_class_type module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) ct
   | Element_value _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
   | Element_type_extension te -> associate_in_type_extension module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) te
   | Element_exception ex ->
       (
        match ex.ex_alias with
          None -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
        | Some ea ->
            match ea.ea_ex with
              Some _ ->
                (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
            | None ->
                let ex_opt =
                  try Some (lookup_exception ea.ea_name)
                  with Not_found -> None
                in
                match ex_opt with
                  None -> (acc_b_modif, (Name.head m_name) :: acc_incomplete_top_module_names, (NF_ex ea.ea_name) :: acc_names_not_found)
                | Some e ->
                    ea.ea_ex <- Some e ;
                    (true, acc_incomplete_top_module_names, acc_names_not_found)
       )
   | Element_type _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
   | Element_module_comment _ -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)

and associate_in_class module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) c =
  let rec iter_kind (acc_b, acc_inc, acc_names) k =
    match k with
      Class_structure (inher_l, _) ->
        let f (acc_b2, acc_inc2, acc_names2) ic =
          match ic.ic_class with
          Some _ -> (acc_b2, acc_inc2, acc_names2)
        | None ->
            let cct_opt =
              try Some (Cl (lookup_class ic.ic_name))
              with Not_found ->
                try Some (Cltype (lookup_class_type ic.ic_name, []))
                with Not_found -> None
            in
            match cct_opt with
              None -> (acc_b2, (Name.head c.cl_name) :: acc_inc2,
                       (* we don't want to output warning messages for "object ... end" classes not found *)
                       (if ic.ic_name = Odoc_messages.object_end then acc_names2 else (NF_cct ic.ic_name) :: acc_names2))
            | Some cct ->
                ic.ic_class <- Some cct ;
                (true, acc_inc2, acc_names2)
        in
        List.fold_left f (acc_b, acc_inc, acc_names) inher_l

    | Class_apply capp ->
        (
         match capp.capp_class with
           Some _ ->  (acc_b, acc_inc, acc_names)
         | None ->
             let cl_opt =
               try Some (lookup_class capp.capp_name)
               with Not_found -> None
             in
             match cl_opt with
               None -> (acc_b, (Name.head c.cl_name) :: acc_inc,
                        (* we don't want to output warning messages for "object ... end" classes not found *)
                        (if capp.capp_name = Odoc_messages.object_end then acc_names else (NF_c capp.capp_name) :: acc_names))
             | Some c ->
                 capp.capp_class <- Some c ;
                 (true, acc_inc, acc_names)
        )

    | Class_constr cco ->
        (
         match cco.cco_class with
           Some _ ->  (acc_b, acc_inc, acc_names)
         | None ->
             let cl_opt =
               try Some (lookup_class cco.cco_name)
               with Not_found -> None
             in
             match cl_opt with
               None ->
                 (
                  let clt_opt =
                    try Some (lookup_class_type cco.cco_name)
                    with Not_found -> None
                  in
                  match clt_opt with
                    None ->
                      (acc_b, (Name.head c.cl_name) :: acc_inc,
                        (* we don't want to output warning messages for "object ... end" classes not found *)
                       (if cco.cco_name = Odoc_messages.object_end then acc_names else (NF_cct cco.cco_name) :: acc_names))
                  | Some ct ->
                      cco.cco_class <- Some (Cltype (ct, [])) ;
                      (true, acc_inc, acc_names)
                 )
             | Some c ->
                 cco.cco_class <- Some (Cl c) ;
                 (true, acc_inc, acc_names)
        )
    | Class_constraint (ckind, ctkind) ->
        let (acc_b2, acc_inc2, acc_names2) = iter_kind (acc_b, acc_inc, acc_names) ckind in
        associate_in_class_type module_list (acc_b2, acc_inc2, acc_names2)
            { clt_name = "" ; clt_info = None ;
              clt_type = c.cl_type ; (* should be ok *)
              clt_type_parameters = [] ;
              clt_virtual = false ;
              clt_kind = ctkind ;
              clt_loc = Odoc_types.dummy_loc }
  in
  iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) c.cl_kind

and associate_in_class_type module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) ct =
  let rec iter_kind (acc_b, acc_inc, acc_names) k =
    match k with
      Class_signature (inher_l, _) ->
        let f (acc_b2, acc_inc2, acc_names2) ic =
          match ic.ic_class with
            Some _ -> (acc_b2, acc_inc2, acc_names2)
          | None ->
              let cct_opt =
                try Some (Cltype (lookup_class_type ic.ic_name, []))
                with Not_found ->
                  try Some (Cl (lookup_class ic.ic_name))
                  with Not_found -> None
              in
              match cct_opt with
                None -> (acc_b2, (Name.head ct.clt_name) :: acc_inc2,
                         (* we don't want to output warning messages for "object ... end" class types not found *)
                         (if ic.ic_name = Odoc_messages.object_end then acc_names2 else (NF_cct ic.ic_name) :: acc_names2))
              | Some cct ->
                  ic.ic_class <- Some cct ;
                  (true, acc_inc2, acc_names2)
        in
        List.fold_left f (acc_b, acc_inc, acc_names) inher_l

    | Class_type cta ->
        (
         match cta.cta_class with
           Some _ ->  (acc_b, acc_inc, acc_names)
         | None ->
             let cct_opt =
               try Some (Cltype (lookup_class_type cta.cta_name, []))
               with Not_found ->
                 try Some (Cl (lookup_class cta.cta_name))
                 with Not_found -> None
             in
             match cct_opt with
               None -> (acc_b, (Name.head ct.clt_name) :: acc_inc,
                        (* we don't want to output warning messages for "object ... end" class types not found *)
                        (if cta.cta_name = Odoc_messages.object_end then acc_names else (NF_cct cta.cta_name) :: acc_names))
             | Some c ->
                 cta.cta_class <- Some c ;
                 (true, acc_inc, acc_names)
        )
  in
  iter_kind (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) ct.clt_kind

and associate_in_type_extension module_list (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) te =
  List.fold_left
    (fun (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found) xt ->
       match xt.xt_alias with
           None -> (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
         | Some xa ->
             match xa.xa_xt with
                 Some _ ->
                   (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
               | None ->
                   let xt_opt =
                     try Some (lookup_extension xa.xa_name)
                     with Not_found -> None
                   in
                     match xt_opt with
                         None -> (acc_b_modif, (Name.head xt.xt_name) :: acc_incomplete_top_module_names, (NF_xt xa.xa_name) :: acc_names_not_found)
                       | Some x ->
                           xa.xa_xt <- Some x ;
                           (true, acc_incomplete_top_module_names, acc_names_not_found))
    (acc_b_modif, acc_incomplete_top_module_names, acc_names_not_found)
    te.te_constructors


(*************************************************************)
(** Association of types to elements referenced in comments .*)

let ao = Odoc_misc.apply_opt

let not_found_of_kind kind name =
  (match kind with
    RK_module -> Odoc_messages.cross_module_not_found
  | RK_module_type -> Odoc_messages.cross_module_type_not_found
  | RK_class -> Odoc_messages.cross_class_not_found
  | RK_class_type -> Odoc_messages.cross_class_type_not_found
  | RK_value -> Odoc_messages.cross_value_not_found
  | RK_type -> Odoc_messages.cross_type_not_found
  | RK_extension -> Odoc_messages.cross_extension_not_found
  | RK_exception -> Odoc_messages.cross_exception_not_found
  | RK_attribute -> Odoc_messages.cross_attribute_not_found
  | RK_method -> Odoc_messages.cross_method_not_found
  | RK_section _ -> Odoc_messages.cross_section_not_found
  | RK_recfield -> Odoc_messages.cross_recfield_not_found
  | RK_const -> Odoc_messages.cross_const_not_found
  ) name

let rec assoc_comments_text_elements parent_name module_list t_ele =
  match t_ele with
  | Raw _
  | Code _
  | CodePre _
  | Latex _
  | Verbatim _ -> t_ele
  | Bold t -> Bold (assoc_comments_text parent_name module_list t)
  | Italic t -> Italic (assoc_comments_text parent_name module_list t)
  | Center t -> Center (assoc_comments_text parent_name module_list t)
  | Left t -> Left (assoc_comments_text parent_name module_list t)
  | Right t -> Right (assoc_comments_text parent_name module_list t)
  | Emphasize t -> Emphasize (assoc_comments_text parent_name module_list t)
  | List l -> List (List.map (assoc_comments_text parent_name module_list) l)
  | Enum l -> Enum (List.map (assoc_comments_text parent_name module_list) l)
  | Newline -> Newline
  | Block t -> Block (assoc_comments_text parent_name module_list t)
  | Superscript t -> Superscript (assoc_comments_text parent_name module_list t)
  | Subscript t -> Subscript (assoc_comments_text parent_name module_list t)
  | Title (n, l_opt, t) -> Title (n, l_opt, (assoc_comments_text parent_name module_list t))
  | Link (s, t) -> Link (s, (assoc_comments_text parent_name module_list t))
  | Ref (initial_name, None, text_option) ->
      (
       let rec iter_parent ?parent_name name =
         let name = Odoc_name.normalize_name name in
         let res =
           match get_known_elements name with
             [] ->
               (
                try
                  let re = Str.regexp ("^"^(Str.quote name)^"$") in
                  let t = Odoc_search.find_section module_list re in
                  let v2 = (name, Some (RK_section t)) in
                  add_verified v2 ;
                  (name, Some (RK_section t))
              with
                  Not_found ->
                    (name, None)
               )
           | ele :: _ ->
           (* we look for the first element with this name *)
               let (name, kind) =
                 match ele with
                   Odoc_search.Res_module m -> (m.m_name, RK_module)
                 | Odoc_search.Res_module_type mt -> (mt.mt_name, RK_module_type)
                 | Odoc_search.Res_class c -> (c.cl_name, RK_class)
                 | Odoc_search.Res_class_type ct -> (ct.clt_name, RK_class_type)
                 | Odoc_search.Res_value v -> (v.val_name, RK_value)
                 | Odoc_search.Res_type t -> (t.ty_name, RK_type)
                 | Odoc_search.Res_extension x -> (x.xt_name, RK_extension)
                 | Odoc_search.Res_exception e -> (e.ex_name, RK_exception)
                 | Odoc_search.Res_attribute a -> (a.att_value.val_name, RK_attribute)
                 | Odoc_search.Res_method m -> (m.met_value.val_name, RK_method)
                 | Odoc_search.Res_section (_ ,t)-> assert false
                 | Odoc_search.Res_recfield (t, f) ->
                     (Printf.sprintf "%s.%s" t.ty_name f.rf_name, RK_recfield)
                 | Odoc_search.Res_const (t, f) ->
                     (Printf.sprintf "%s.%s" t.ty_name f.vc_name, RK_const)
               in
               add_verified (name, Some kind) ;
               (name, Some kind)
         in
         match res with
         | (name, Some k) -> Ref (name, Some k, text_option)
         | (_, None) ->
             match parent_name with
               None ->
                 Odoc_global.pwarning (Odoc_messages.cross_element_not_found initial_name);
                 Ref (initial_name, None, text_option)
             | Some p ->
                 let parent_name =
                   match Name.father p with
                     "" -> None
                   | s -> Some s
                 in
                 iter_parent ?parent_name (Name.concat p initial_name)
       in
       iter_parent ~parent_name initial_name
      )
  | Ref (initial_name, Some kind, text_option) ->
      (
       let rec iter_parent ?parent_name name =
         let v = (name, Some kind) in
         if was_verified v then
           Ref (name, Some kind, text_option)
         else
           let res =
             match kind with
             | RK_section _ ->
                 (
                  (** we just verify that we find an element of this kind with this name *)
                  try
                    let re = Str.regexp ("^"^(Str.quote name)^"$") in
                    let t = Odoc_search.find_section module_list re in
                    let v2 = (name, Some (RK_section t)) in
                    add_verified v2 ;
                    (name, Some (RK_section t))
                  with
                    Not_found ->
                      (name, None)
                 )
             | _ ->
                 let f =
                   match kind with
                     RK_module -> module_exists
                   | RK_module_type -> module_type_exists
                   | RK_class -> class_exists
                   | RK_class_type -> class_type_exists
                   | RK_value -> value_exists
                   | RK_type -> type_exists
                   | RK_extension -> extension_exists
                   | RK_exception -> exception_exists
                   | RK_attribute -> attribute_exists
                   | RK_method -> method_exists
                   | RK_section _ -> assert false
                   | RK_recfield -> recfield_exists
                   | RK_const -> const_exists
                 in
                 if f name then
                   (
                    add_verified v ;
                    (name, Some kind)
                   )
                 else
                   (name, None)
           in
           match res with
           | (name, Some k) -> Ref (name, Some k, text_option)
           | (_, None) ->
               match parent_name with
                 None ->
                   Odoc_global.pwarning (not_found_of_kind kind initial_name);
                   Ref (initial_name, None, text_option)
               | Some p ->
                   let parent_name =
                     match Name.father p with
                       "" -> None
                     | s -> Some s
                   in
                   iter_parent ?parent_name (Name.concat p initial_name)
       in
       iter_parent ~parent_name initial_name
      )
  | Module_list l ->
      Module_list l
  | Index_list ->
      Index_list
  | Custom (s,t) -> Custom (s, (assoc_comments_text parent_name module_list t))
  | Target (target, code) -> Target (target, code)

and assoc_comments_text parent_name module_list text =
  List.map (assoc_comments_text_elements parent_name module_list) text

and assoc_comments_info parent_name module_list i =
  let ft = assoc_comments_text parent_name module_list in
  {
    i with
    i_desc = ao ft i.i_desc ;
    i_sees = List.map (fun (sr, t) -> (sr, ft t)) i.i_sees;
    i_deprecated = ao ft i.i_deprecated ;
    i_params = List.map (fun (name, t) -> (name, ft t)) i.i_params;
    i_raised_exceptions = List.map (fun (name, t) -> (name, ft t)) i.i_raised_exceptions;
    i_return_value = ao ft i.i_return_value ;
    i_custom = List.map (fun (tag, t) -> (tag, ft t)) i.i_custom ;
  }


let rec assoc_comments_module_element parent_name module_list m_ele =
  match m_ele with
    Element_module m ->
      Element_module (assoc_comments_module module_list m)
  | Element_module_type mt ->
      Element_module_type (assoc_comments_module_type module_list mt)
  | Element_included_module _ ->
      m_ele (* don't go down into the aliases *)
  | Element_class c ->
      Element_class (assoc_comments_class module_list c)
  | Element_class_type ct ->
      Element_class_type (assoc_comments_class_type module_list ct)
  | Element_value v ->
      Element_value (assoc_comments_value module_list v)
  | Element_type_extension te ->
      Element_type_extension (assoc_comments_type_extension parent_name module_list te)
  | Element_exception e ->
      Element_exception (assoc_comments_exception module_list e)
  | Element_type t ->
      Element_type (assoc_comments_type module_list t)
  | Element_module_comment t ->
      Element_module_comment (assoc_comments_text parent_name module_list t)

and assoc_comments_class_element parent_name module_list c_ele =
  match c_ele with
    Class_attribute a ->
      Class_attribute (assoc_comments_attribute module_list a)
  | Class_method m ->
      Class_method (assoc_comments_method module_list m)
  | Class_comment t ->
      Class_comment (assoc_comments_text parent_name module_list t)

and assoc_comments_module_kind parent_name module_list mk =
  match mk with
  | Module_struct eles ->
      Module_struct
        (List.map (assoc_comments_module_element parent_name module_list) eles)
  | Module_alias _
  | Module_functor _ ->
      mk
  | Module_apply (mk1, mk2) ->
      Module_apply (assoc_comments_module_kind parent_name module_list mk1,
                    assoc_comments_module_kind parent_name module_list mk2)
  | Module_with (mtk, s) ->
      Module_with (assoc_comments_module_type_kind parent_name module_list mtk, s)
  | Module_constraint (mk1, mtk) ->
      Module_constraint
        (assoc_comments_module_kind parent_name module_list mk1,
         assoc_comments_module_type_kind parent_name module_list mtk)
  | Module_typeof _ -> mk
  | Module_unpack _ -> mk

and assoc_comments_module_type_kind parent_name module_list mtk =
  match mtk with
  | Module_type_struct eles ->
      Module_type_struct
        (List.map (assoc_comments_module_element parent_name module_list) eles)
  | Module_type_functor (params, mtk1) ->
      Module_type_functor
        (params, assoc_comments_module_type_kind parent_name module_list mtk1)
  | Module_type_alias _ ->
      mtk
  | Module_type_with (mtk1, s) ->
      Module_type_with
        (assoc_comments_module_type_kind parent_name module_list mtk1, s)
  | Module_type_typeof _ -> mtk

and assoc_comments_class_kind parent_name module_list ck =
  match ck with
    Class_structure (inher, eles) ->
      let inher2 =
        List.map
          (fun ic ->
            { ic with
              ic_text = ao (assoc_comments_text parent_name module_list) ic.ic_text })
          inher
      in
      Class_structure
        (inher2, List.map (assoc_comments_class_element parent_name module_list) eles)

  | Class_apply _
  | Class_constr _ -> ck
  | Class_constraint (ck1, ctk) ->
      Class_constraint (assoc_comments_class_kind parent_name module_list ck1,
                        assoc_comments_class_type_kind parent_name module_list ctk)

and assoc_comments_class_type_kind parent_name module_list ctk =
  match ctk with
    Class_signature (inher, eles) ->
      let inher2 =
        List.map
          (fun ic -> { ic with
                       ic_text = ao (assoc_comments_text parent_name module_list) ic.ic_text })
          inher
      in
      Class_signature (inher2, List.map (assoc_comments_class_element parent_name module_list) eles)

  | Class_type _ -> ctk


and assoc_comments_module module_list m =
  m.m_info <- ao (assoc_comments_info m.m_name module_list) m.m_info ;
  m.m_kind <- assoc_comments_module_kind m.m_name module_list m.m_kind ;
  m

and assoc_comments_module_type module_list mt =
  mt.mt_info <- ao (assoc_comments_info mt.mt_name module_list) mt.mt_info ;
  mt.mt_kind <- ao (assoc_comments_module_type_kind mt.mt_name module_list) mt.mt_kind ;
  mt

and assoc_comments_class module_list c =
  c.cl_info <- ao (assoc_comments_info c.cl_name module_list) c.cl_info ;
  c.cl_kind <- assoc_comments_class_kind c.cl_name module_list c.cl_kind ;
  assoc_comments_parameter_list c.cl_name module_list c.cl_parameters;
  c

and assoc_comments_class_type module_list ct =
  ct.clt_info <- ao (assoc_comments_info ct.clt_name module_list) ct.clt_info ;
  ct.clt_kind <- assoc_comments_class_type_kind ct.clt_name module_list ct.clt_kind ;
  ct

and assoc_comments_parameter parent_name module_list p =
  match p with
    Simple_name sn ->
      sn.sn_text <- ao (assoc_comments_text parent_name module_list) sn.sn_text
  | Tuple (l, t) ->
      List.iter (assoc_comments_parameter parent_name module_list) l

and assoc_comments_parameter_list parent_name module_list pl =
  List.iter (assoc_comments_parameter parent_name module_list) pl

and assoc_comments_value module_list v =
  let parent = Name.father v.val_name in
  v.val_info <- ao (assoc_comments_info parent module_list) v.val_info ;
  assoc_comments_parameter_list parent module_list v.val_parameters;
  v

and assoc_comments_extension_constructor module_list x =
  let parent = Name.father x.xt_name in
  x.xt_text <- ao (assoc_comments_info parent module_list) x.xt_text

and assoc_comments_type_extension parent_name module_list te =
  te.te_info <- ao (assoc_comments_info parent_name module_list) te.te_info;
  List.iter (assoc_comments_extension_constructor module_list) te.te_constructors;
  te

and assoc_comments_exception module_list e =
  let parent = Name.father e.ex_name in
  e.ex_info <- ao (assoc_comments_info parent module_list) e.ex_info ;
  e

and assoc_comments_type module_list t =
  let parent = Name.father t.ty_name in
  t.ty_info <- ao (assoc_comments_info parent module_list) t.ty_info ;
  (match t.ty_kind with
    Type_abstract -> ()
  | Type_variant vl ->
      List.iter
        (fun vc -> vc.vc_text <- ao (assoc_comments_info parent module_list) vc.vc_text)
        vl
  | Type_record fl ->
      List.iter
        (fun rf -> rf.rf_text <- ao (assoc_comments_info parent module_list) rf.rf_text)
        fl
  | Type_open -> ()
  );
  t

and assoc_comments_attribute module_list a =
  let _ = assoc_comments_value module_list a.att_value in
  a

and assoc_comments_method module_list m =
  let parent_name = Name.father m.met_value.val_name in
  let _ = assoc_comments_value module_list m.met_value in
  assoc_comments_parameter_list parent_name module_list m.met_value.val_parameters;
  m


let associate_type_of_elements_in_comments module_list =
  List.map (assoc_comments_module module_list) module_list


(***********************************************************)
(** The function which performs all the cross referencing. *)
let associate module_list =
  get_alias_names module_list ;
  init_known_elements_map module_list;
  let rec remove_doubles acc = function
      [] -> acc
    | h :: q ->
        if List.mem h acc then remove_doubles acc q
        else remove_doubles (h :: acc) q
  in
  let rec iter incomplete_modules =
    let (b_modif, remaining_inc_modules, acc_names_not_found) =
      List.fold_left (associate_in_module module_list) (false, [], []) incomplete_modules
    in
    let remaining_no_doubles = remove_doubles [] remaining_inc_modules in
    let remaining_modules = List.filter
        (fun m -> List.mem m.m_name remaining_no_doubles)
        incomplete_modules
    in
    if b_modif then
      (* we may be able to associate something else *)
      iter remaining_modules
    else
      (* nothing changed, we won't be able to associate any more *)
      acc_names_not_found
  in
  let names_not_found = iter module_list in
  (
   match names_not_found with
     [] ->
       ()
   | l ->
       List.iter
         (fun nf ->
           Odoc_global.pwarning
             (
              match nf with
                NF_m n -> Odoc_messages.cross_module_not_found n
              | NF_mt n -> Odoc_messages.cross_module_type_not_found n
              | NF_mmt n -> Odoc_messages.cross_module_or_module_type_not_found n
              | NF_c n -> Odoc_messages.cross_class_not_found n
              | NF_ct n -> Odoc_messages.cross_class_type_not_found n
              | NF_cct n -> Odoc_messages.cross_class_or_class_type_not_found n
              | NF_xt n -> Odoc_messages.cross_extension_not_found n
              | NF_ex n -> Odoc_messages.cross_exception_not_found n
             );
         )
         l
  ) ;

  (* Find a type for each name of element which is referenced in comments. *)
  ignore (associate_type_of_elements_in_comments module_list)
