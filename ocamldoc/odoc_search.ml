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

(** Research of elements through modules. *)

module Name = Odoc_name
open Odoc_parameter
open Odoc_value
open Odoc_type
open Odoc_extension
open Odoc_exception
open Odoc_class
open Odoc_module

type result_element =
    Res_module of t_module
  | Res_module_type of t_module_type
  | Res_class of t_class
  | Res_class_type of t_class_type
  | Res_value of t_value
  | Res_type of t_type
  | Res_extension of t_extension_constructor
  | Res_exception of t_exception
  | Res_attribute of t_attribute
  | Res_method of t_method
  | Res_section of string * Odoc_types.text
  | Res_recfield of t_type * record_field
  | Res_const of t_type * variant_constructor

type result = result_element list

module type Predicates =
  sig
    type t
    val p_module : t_module -> t -> bool * bool
    val p_module_type : t_module_type -> t -> bool * bool
    val p_class : t_class -> t -> bool * bool
    val p_class_type : t_class_type -> t -> bool * bool
    val p_value : t_value -> t -> bool
    val p_recfield : t_type -> record_field -> t -> bool
    val p_const : t_type -> variant_constructor -> t -> bool
    val p_type : t_type -> t -> (bool * bool)
    val p_extension : t_extension_constructor -> t -> bool
    val p_exception : t_exception -> t -> bool
    val p_attribute : t_attribute -> t -> bool
    val p_method : t_method -> t -> bool
    val p_section : string -> t -> bool
  end

module Search =
  functor (P : Predicates) ->
  struct
    let search_section t s v = if P.p_section s v then [Res_section (s,t)] else []

    let rec search_text root t v =
      List.flatten (List.map (fun e -> search_text_ele root e v) t)

    and search_text_ele root e v =
      let module T = Odoc_types in
      match e with
      | T.Raw _
      | T.Code _
      | T.CodePre _
      | T.Latex _
      | T.Verbatim _
      | T.Ref (_, _, _) -> []
      | T.Bold t
      | T.Italic t
      | T.Center t
      | T.Left t
      | T.Right t
      | T.Emphasize t
      | T.Block t
      | T.Superscript t
      | T.Subscript t
      | T.Custom (_,t)
      | T.Link (_, t) -> search_text root t v
      | T.List l
      | T.Enum l -> List.flatten (List.map (fun t -> search_text root t v) l)
      | T.Newline
      | T.Module_list _
      | T.Index_list -> []
      | T.Target _ -> []
      | T.Title (n, l_opt, t) ->
          (match l_opt with
            None -> []
          | Some s -> search_section t (Name.concat root s) v) @
          (search_text root t v)

    let search_value va v = if P.p_value va v then [Res_value va] else []

    let search_recfield t f v =
      if P.p_recfield t f v then [Res_recfield (t,f)] else []

    let search_const t f v =
      if P.p_const t f v then [Res_const (t,f)] else []

    let search_type t v =
      let (go_deeper, ok) = P.p_type t v in
      let l =
        match go_deeper with
          false -> []
        | true ->
            match t.ty_kind with
              Type_abstract -> []
            | Type_record l ->
                List.flatten (List.map (fun rf -> search_recfield t rf v) l)
            | Type_variant l ->
                List.flatten (List.map (fun rf -> search_const t rf v) l)
            | Type_open -> []
      in
      if ok then (Res_type t) :: l else l

    let search_extension_constructor xt v =
      if P.p_extension xt v then [Res_extension xt] else []

    let search_type_extension te v =
      List.fold_left
        (fun acc -> fun xt -> acc @ (search_extension_constructor xt v))
        []
        (Odoc_extension.extension_constructors te)

    let search_exception e v = if P.p_exception e v then [Res_exception e] else []

    let search_attribute a v = if P.p_attribute a v then [Res_attribute a] else []

    let search_method m v = if P.p_method m v then [Res_method m] else []

    let search_class c v =
      let (go_deeper, ok) = P.p_class c v in
      let l =
        if go_deeper then
          let res_att =
            List.fold_left
              (fun acc -> fun att -> acc @ (search_attribute att v))
              []
              (Odoc_class.class_attributes c)
          in
          let res_met =
            List.fold_left
              (fun acc -> fun m -> acc @ (search_method m v))
              []
              (Odoc_class.class_methods c)
          in
          let res_sec =
            List.fold_left
              (fun acc -> fun t -> acc @ (search_text c.cl_name t v))
              []
              (Odoc_class.class_comments c)
          in
          let l = res_att @ res_met @ res_sec in
          l
        else
          []
      in
      if ok then
        (Res_class c) :: l
      else
        l

    let search_class_type ct v =
      let (go_deeper, ok) = P.p_class_type ct v in
      let l =
        if go_deeper then
          let res_att =
            List.fold_left
              (fun acc -> fun att -> acc @ (search_attribute att v))
              []
              (Odoc_class.class_type_attributes ct)
          in
          let res_met =
            List.fold_left
              (fun acc -> fun m -> acc @ (search_method m v))
              []
              (Odoc_class.class_type_methods ct)
          in
          let res_sec =
            List.fold_left
              (fun acc -> fun t -> acc @ (search_text ct.clt_name t v))
              []
              (Odoc_class.class_type_comments ct)
          in
          let l = res_att @ res_met @ res_sec in
          l
        else
          []
      in
      if ok then
        (Res_class_type ct) :: l
      else
        l

    let rec search_module_type mt v =
      let (go_deeper, ok) =  P.p_module_type mt v in
      let l =
        if go_deeper then
          let res_val =
            List.fold_left
              (fun acc -> fun va -> acc @ (search_value va v))
              []
              (Odoc_module.module_type_values mt)
          in
          let res_typ =
            List.fold_left
              (fun acc -> fun t -> acc @ (search_type t v))
              []
              (Odoc_module.module_type_types mt)
          in
          let res_ext =
            List.fold_left
              (fun acc -> fun te -> acc @ (search_type_extension te v))
              []
              (Odoc_module.module_type_type_extensions mt)
          in
          let res_exc =
            List.fold_left
              (fun acc -> fun e -> acc @ (search_exception e v))
              []
              (Odoc_module.module_type_exceptions mt)
          in
          let res_mod = search (Odoc_module.module_type_modules mt) v in
          let res_modtyp =
            List.fold_left
              (fun acc -> fun mt -> acc @ (search_module_type mt v))
              []
              (Odoc_module.module_type_module_types mt)
          in
          let res_cl =
            List.fold_left
              (fun acc -> fun cl -> acc @ (search_class cl v))
              []
              (Odoc_module.module_type_classes mt)
          in
          let res_cltyp =
            List.fold_left
              (fun acc -> fun clt -> acc @ (search_class_type clt v))
              []
              (Odoc_module.module_type_class_types mt)
          in
          let res_sec =
            List.fold_left
              (fun acc -> fun t -> acc @ (search_text mt.mt_name t v))
              []
              (Odoc_module.module_type_comments mt)
          in
          let l = res_val @ res_typ @ res_ext @ res_exc @ res_mod @
            res_modtyp @ res_cl @ res_cltyp @ res_sec
          in
          l
        else
          []
      in
      if ok then
        (Res_module_type mt) :: l
      else
        l

    and search_module m v =
      let (go_deeper, ok) =  P.p_module m v in
      let l =
        if go_deeper then
          let res_val =
            List.fold_left
              (fun acc -> fun va -> acc @ (search_value va v))
              []
              (Odoc_module.module_values m)
          in
          let res_typ =
            List.fold_left
              (fun acc -> fun t -> acc @ (search_type t v))
              []
              (Odoc_module.module_types m)
          in
          let res_ext =
            List.fold_left
              (fun acc -> fun te -> acc @ (search_type_extension te v))
              []
              (Odoc_module.module_type_extensions m)
          in
          let res_exc =
            List.fold_left
              (fun acc -> fun e -> acc @ (search_exception e v))
              []
              (Odoc_module.module_exceptions m)
          in
          let res_mod = search (Odoc_module.module_modules m) v in
          let res_modtyp =
            List.fold_left
              (fun acc -> fun mt -> acc @ (search_module_type mt v))
              []
              (Odoc_module.module_module_types m)
          in
          let res_cl =
            List.fold_left
              (fun acc -> fun cl -> acc @ (search_class cl v))
              []
              (Odoc_module.module_classes m)
          in
          let res_cltyp =
            List.fold_left
              (fun acc -> fun clt -> acc @ (search_class_type clt v))
              []
              (Odoc_module.module_class_types m)
          in
          let res_sec =
            List.fold_left
              (fun acc -> fun t -> acc @ (search_text m.m_name t v))
              []
              (Odoc_module.module_comments m)
          in
          let l = res_val @ res_typ @ res_ext @ res_exc @ res_mod @
            res_modtyp @ res_cl @ res_cltyp @ res_sec
          in
          l
        else
          []
      in
      if ok then
        (Res_module m) :: l
      else
        l

    and search module_list v =
      List.fold_left
        (fun acc -> fun m ->
          List.fold_left
            (fun acc2 -> fun ele ->
              if List.mem ele acc2 then acc2 else acc2 @ [ele]
            )
            acc
            (search_module m v)
        )
        []
        module_list
  end

module P_name =
  struct
    type t = Str.regexp
    let (=~) name regexp = Str.string_match regexp name 0
    let p_module m r = (true, m.m_name =~ r)
    let p_module_type mt r = (true, mt.mt_name =~ r)
    let p_class c r = (true, c.cl_name =~ r)
    let p_class_type ct r = (true, ct.clt_name =~ r)
    let p_value v r = v.val_name =~ r
    let p_recfield t f r =
      let name = Printf.sprintf "%s.%s" t.ty_name f.rf_name in
      name =~ r
    let p_const t f r =
      let name = Printf.sprintf "%s.%s" t.ty_name f.vc_name in
      name =~ r
    let p_type t r = (true, t.ty_name =~ r)
    let p_extension x r = x.xt_name =~ r
    let p_exception e r = e.ex_name =~ r
    let p_attribute a r = a.att_value.val_name =~ r
    let p_method m r = m.met_value.val_name =~ r
    let p_section s r = s =~ r
  end

module Search_by_name = Search ( P_name )

module P_values =
  struct
    type t = unit
    let p_module _ _ = (true, false)
    let p_module_type _ _ = (true, false)
    let p_class _ _ = (false, false)
    let p_class_type _ _ = (false, false)
    let p_value _ _ = true
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, false)
    let p_extension _ _ = false
    let p_exception _ _ = false
    let p_attribute _ _ = false
    let p_method _ _ = false
    let p_section _ _ = false
  end
module Search_values = Search ( P_values )
let values l =
  let l_ele = Search_values.search l () in
  let p v1 v2 = v1.val_name = v2.val_name in
  let rec iter acc = function
      (Res_value v) :: q -> if List.exists (p v) acc then iter acc q else iter (v :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

module P_extensions =
  struct
    type t = unit
    let p_module _ _ = (true, false)
    let p_module_type _ _ = (true, false)
    let p_class _ _ = (false, false)
    let p_class_type _ _ = (false, false)
    let p_value _ _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, false)
    let p_extension _ _ = true
    let p_exception _ _ = false
    let p_attribute _ _ = false
    let p_method _ _ = false
    let p_section _ _ = false
  end
module Search_extensions = Search ( P_extensions )
let extensions l =
  let l_ele = Search_extensions.search l () in
  let p x1 x2 = x1.xt_name = x2.xt_name in
  let rec iter acc = function
      (Res_extension x) :: q -> if List.exists (p x) acc then iter acc q else iter (x :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

module P_exceptions =
  struct
    type t = unit
    let p_module _ _ = (true, false)
    let p_module_type _ _ = (true, false)
    let p_class _ _ = (false, false)
    let p_class_type _ _ = (false, false)
    let p_value _ _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, false)
    let p_extension _ _ = false
    let p_exception _ _ = true
    let p_attribute _ _ = false
    let p_method _ _ = false
    let p_section _ _ = false
  end
module Search_exceptions = Search ( P_exceptions )
let exceptions l =
  let l_ele = Search_exceptions.search l () in
  let p e1 e2 = e1.ex_name = e2.ex_name in
  let rec iter acc = function
      (Res_exception t) :: q -> if List.exists (p t) acc then iter acc q else iter (t :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

module P_types =
  struct
    type t = unit
    let p_module _ _ = (true, false)
    let p_module_type _ _ = (true, false)
    let p_class _ _ = (false, false)
    let p_class_type _ _ = (false, false)
    let p_value _ _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, true)
    let p_extension _ _ = false
    let p_exception _ _ = false
    let p_attribute _ _ = false
    let p_method _ _ = false
    let p_section _ _ = false
  end
module Search_types = Search ( P_types )
let types l =
  let l_ele = Search_types.search l () in
  let p t1 t2 = t1.ty_name = t2.ty_name in
  let rec iter acc = function
      (Res_type t) :: q -> if List.exists (p t) acc then iter acc q else iter (t :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

module P_attributes =
  struct
    type t = unit
    let p_module _ _ = (true, false)
    let p_module_type _ _ = (true, false)
    let p_class _ _ = (true, false)
    let p_class_type _ _ = (true, false)
    let p_value _ _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, false)
    let p_extension _ _ = false
    let p_exception _ _ = false
    let p_attribute _ _ = true
    let p_method _ _ = false
    let p_section _ _ = false
  end
module Search_attributes = Search ( P_attributes )
let attributes l =
  let l_ele = Search_attributes.search l () in
  let p a1 a2 = a1.att_value.val_name = a2.att_value.val_name in
  let rec iter acc = function
      (Res_attribute t) :: q -> if List.exists (p t) acc then iter acc q else iter (t :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

module P_methods =
  struct
    type t = unit
    let p_module _ _ = (true, false)
    let p_module_type _ _ = (true, false)
    let p_class _ _ = (true, false)
    let p_class_type _ _ = (true, false)
    let p_value _ _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, false)
    let p_extension _ _ = false
    let p_exception _ _ = false
    let p_attribute _ _ = false
    let p_method _ _ = true
    let p_section _ _ = true
  end
module Search_methods = Search ( P_methods )
let methods l =
  let l_ele = Search_methods.search l () in
  let p m1 m2 = m1.met_value.val_name = m2.met_value.val_name in
  let rec iter acc = function
      (Res_method t) :: q -> if List.exists (p t) acc then iter acc q else iter (t :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

module P_classes =
  struct
    type t = unit
    let p_module _ _ = (true, false)
    let p_module_type _ _ = (true, false)
    let p_class _ _ = (false, true)
    let p_class_type _ _ = (false, false)
    let p_value _ _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, false)
    let p_extension _ _ = false
    let p_exception _ _ = false
    let p_attribute _ _ = false
    let p_method _ _ = false
    let p_section _ _ = false
  end
module Search_classes = Search ( P_classes )
let classes l =
  let l_ele = Search_classes.search l () in
  let p c1 c2 = c1.cl_name = c2.cl_name in
  let rec iter acc = function
      (Res_class c) :: q -> if List.exists (p c) acc then iter acc q else iter (c :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

module P_class_types =
  struct
    type t = unit
    let p_module _ _ = (true, false)
    let p_module_type _ _ = (true, false)
    let p_class _ _ = (false, false)
    let p_class_type _ _ = (false, true)
    let p_value _ _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, false)
    let p_extension _ _ = false
    let p_exception _ _ = false
    let p_attribute _ _ = false
    let p_method _ _ = false
    let p_section _ _ = false
  end
module Search_class_types = Search ( P_class_types )
let class_types l =
  let l_ele = Search_class_types.search l () in
  let p c1 c2 = c1.clt_name = c2.clt_name in
  let rec iter acc = function
      (Res_class_type c) :: q -> if List.exists (p c) acc then iter acc q else iter (c :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

module P_modules =
  struct
    type t = unit
    let p_module _ _ = (true, true)
    let p_module_type _ _ = (true, false)
    let p_class _ _ = (false, false)
    let p_class_type _ _ = (false, false)
    let p_value _ _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, false)
    let p_extension _ _ = false
    let p_exception _ _ = false
    let p_attribute _ _ = false
    let p_method _ _ = false
    let p_section _ _ = false
  end
module Search_modules = Search ( P_modules )
let modules l =
  let l_ele = Search_modules.search l () in
  let p m1 m2 = m1.m_name = m2.m_name in
  let rec iter acc = function
      (Res_module m) :: q -> if List.exists (p m) acc then iter acc q else iter (m :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

module P_module_types =
  struct
    type t = unit
    let p_module _ _ = (true, false)
    let p_module_type _ _ = (true, true)
    let p_class _ _ = (false, false)
    let p_class_type _ _ = (false, false)
    let p_value _ _ = false
    let p_recfield _ _ _ = false
    let p_const _ _ _ = false
    let p_type _ _ = (false, false)
    let p_extension _ _ = false
    let p_exception _ _ = false
    let p_attribute _ _ = false
    let p_method _ _ = false
    let p_section _ _ = false
  end
module Search_module_types = Search ( P_module_types )
let module_types l =
  let l_ele = Search_module_types.search l () in
  let p m1 m2 = m1.mt_name = m2.mt_name in
  let rec iter acc = function
      (Res_module_type m) :: q -> if List.exists (p m) acc then iter acc q else iter (m :: acc) q
    | _ :: q -> iter acc q
    | [] -> acc
  in
  iter [] l_ele

let type_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_type _ -> true
      | _ -> false
    )
    l

let value_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_value _ -> true
      | _ -> false
    )
    l

let class_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_class _ -> true
      | _ -> false
    )
    l

let class_type_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_class_type _ -> true
      | _ -> false
    )
    l

let module_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_module _ -> true
      | _ -> false
    )
    l

let module_type_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_module_type _ -> true
      | _ -> false
    )
    l

let extension_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_extension _ -> true
      | _ -> false
    )
    l

let exception_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_exception _ -> true
      | _ -> false
    )
    l

let attribute_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_attribute _ -> true
      | _ -> false
    )
    l

let method_exists mods regexp =
  let l = Search_by_name.search mods regexp in
  List.exists
    (function
        Res_method _ -> true
      | _ -> false
    )
    l

let find_section mods regexp =
  let l = Search_by_name.search mods regexp in
  match
    List.find
      (function
          Res_section _ -> true
        | _ -> false
      )
      l
  with
    Res_section (_,t) -> t
  | _ -> assert false
