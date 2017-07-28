(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*    Hongbo Zhang (University of Pennsylvania)                           *)
(*                                                                        *)
(*   Copyright 2007 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)


(*
  This module is mainly used to diff two parsetree, it helps to automate the
  test for parsing/pprintast.ml
 *)


open Parsetree
let curry f (g, h) = f g h
let eq_int : (int*int)->bool = curry (=)
let eq_char : (char*char)->bool=curry (=)
let eq_string : (string*string)->bool = curry (=)
let eq_int32 : (int32*int32)->bool=curry (=)
let eq_int64 : (int64*int64)->bool =curry (=)
let eq_nativeint : (nativeint*nativeint)->bool= curry (=)
let eq_bool :(bool*bool) -> bool = curry (=)
let eq_list mf_a (xs, ys) =
  let rec loop =
    function
      | ([], []) -> true
        | (x :: xs, y :: ys) -> (mf_a (x, y)) && (loop (xs, ys))
        | (_, _) -> false
  in loop (xs, ys)
let eq_option mf_a (x, y) =
  match (x, y) with
  | (None, None) -> true
  | (Some x, Some y) -> mf_a (x, y)
  | (_, _) -> false

module Location =struct
  include Location
  let eq_t : (t*t) -> bool = fun (_,_) -> true
end
module Longident = struct
  include Longident
  let rec eq_t : (t * t) -> 'result =
    function
    | (Lident a0, Lident b0) -> eq_string (a0, b0)
    | (Ldot (a0, a1), Ldot (b0, b1)) ->
      (eq_t (a0, b0)) && (eq_string (a1, b1))
    | (Lapply (a0, a1), Lapply (b0, b1)) ->
      (eq_t (a0, b0)) && (eq_t (a1, b1))
    | (_, _) -> false
end
module Asttypes = struct
  open Asttypes
  let eq_constant : (constant * constant) -> 'result =
    function
    | (Const_int a0, Const_int b0) -> eq_int (a0, b0)
    | (Const_char a0, Const_char b0) -> eq_char (a0, b0)
    | (Const_string a0, Const_string b0) -> eq_string (a0, b0)
    | (Const_float a0, Const_float b0) -> eq_string (a0, b0)
    | (Const_int32 a0, Const_int32 b0) -> eq_int32 (a0, b0)
    | (Const_int64 a0, Const_int64 b0) -> eq_int64 (a0, b0)
    | (Const_nativeint a0, Const_nativeint b0) -> eq_nativeint (a0, b0)
    | (_, _) -> false

  let eq_rec_flag : (rec_flag * rec_flag) -> 'result =
    function
    | (Nonrecursive, Nonrecursive) -> true
    | (Recursive, Recursive) -> true
    | (Default, Default) -> true
    | (_, _) -> false

  let eq_direction_flag :
    (direction_flag * direction_flag) -> 'result =
    function
    | (Upto, Upto) -> true
    | (Downto, Downto) -> true
    | (_, _) -> false

  let eq_private_flag : (private_flag * private_flag) -> 'result =
    function
    | (Private, Private) -> true
    | (Public, Public) -> true
    | (_, _) -> false

  let eq_mutable_flag : (mutable_flag * mutable_flag) -> 'result =
    function
    | (Immutable, Immutable) -> true
    | (Mutable, Mutable) -> true
    | (_, _) -> false

  let eq_virtual_flag : (virtual_flag * virtual_flag) -> 'result =
    function
    | (Virtual, Virtual) -> true
    | (Concrete, Concrete) -> true
    | (_, _) -> false

  let eq_override_flag : (override_flag * override_flag) -> 'result =
    function
    | (Override, Override) -> true
    | (Fresh, Fresh) -> true
    | (_, _) -> false

  let eq_closed_flag : (closed_flag * closed_flag) -> 'result =
    function
    | (Closed, Closed) -> true
    | (Open, Open) -> true
    | (_, _) -> false

  let eq_label : (label * label) -> 'result =
    fun (a0, a1) -> eq_string (a0, a1)

  let  eq_loc :
    'all_a0.
      (('all_a0 * 'all_a0) -> 'result) ->
        (('all_a0 loc) * ('all_a0 loc)) -> 'result =
    fun mf_a ({ txt = a0; loc = a1 }, { txt = b0; loc = b1 }) ->
      (mf_a (a0, b0)) && (Location.eq_t (a1, b1))

end

let rec eq_row_field : (row_field * row_field) -> 'result =
  function
  | (Rtag (a0, a1, a2), Rtag (b0, b1, b2)) ->
      ((Asttypes.eq_label (a0, b0)) && (eq_bool (a1, b1))) &&
        (eq_list eq_core_type (a2, b2))
  | (Rinherit a0, Rinherit b0) -> eq_core_type (a0, b0)
  | (_, _) -> false
and eq_core_field_desc :
  (core_field_desc * core_field_desc) -> 'result =
  function
  | (Pfield (a0, a1), Pfield (b0, b1)) ->
      (eq_string (a0, b0)) && (eq_core_type (a1, b1))
  | (Pfield_var, Pfield_var) -> true
  | (_, _) -> false
and eq_core_field_type :
  (core_field_type * core_field_type) -> 'result =
  fun
    ({ pfield_desc = a0; pfield_loc = a1 },
     { pfield_desc = b0; pfield_loc = b1 })
    -> (eq_core_field_desc (a0, b0)) && (Location.eq_t (a1, b1))
and eq_package_type : (package_type * package_type) -> 'result =
  fun (a0, a1) ->
    (fun ((a0, a1), (b0, b1)) ->
       (Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
         (eq_list
            (fun ((a0, a1), (b0, b1)) ->
               (Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
                 (eq_core_type (a1, b1)))
            (a1, b1)))
      (a0, a1)
and eq_core_type_desc :
  (core_type_desc * core_type_desc) -> 'result =
  function
  | (Ptyp_any, Ptyp_any) -> true
  | (Ptyp_var a0, Ptyp_var b0) -> eq_string (a0, b0)
  | (Ptyp_arrow (a0, a1, a2), Ptyp_arrow (b0, b1, b2)) ->
      ((Asttypes.eq_label (a0, b0)) && (eq_core_type (a1, b1))) &&
        (eq_core_type (a2, b2))
  | (Ptyp_tuple a0, Ptyp_tuple b0) -> eq_list eq_core_type (a0, b0)
  | (Ptyp_constr (a0, a1), Ptyp_constr (b0, b1)) ->
      (Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
        (eq_list eq_core_type (a1, b1))
  | (Ptyp_object a0, Ptyp_object b0) ->
      eq_list eq_core_field_type (a0, b0)
  | (Ptyp_class (a0, a1, a2), Ptyp_class (b0, b1, b2)) ->
      ((Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
         (eq_list eq_core_type (a1, b1)))
        && (eq_list Asttypes.eq_label (a2, b2))
  | (Ptyp_alias (a0, a1), Ptyp_alias (b0, b1)) ->
      (eq_core_type (a0, b0)) && (eq_string (a1, b1))
  | (Ptyp_variant (a0, a1, a2), Ptyp_variant (b0, b1, b2)) ->
      ((eq_list eq_row_field (a0, b0)) && (eq_bool (a1, b1))) &&
        (eq_option (eq_list Asttypes.eq_label) (a2, b2))
  | (Ptyp_poly (a0, a1), Ptyp_poly (b0, b1)) ->
      (eq_list eq_string (a0, b0)) && (eq_core_type (a1, b1))
  | (Ptyp_package a0, Ptyp_package b0) -> eq_package_type (a0, b0)
  | (_, _) -> false
and eq_core_type : (core_type * core_type) -> 'result =
  fun
    ({ ptyp_desc = a0; ptyp_loc = a1 },
     { ptyp_desc = b0; ptyp_loc = b1 })
    -> (eq_core_type_desc (a0, b0)) && (Location.eq_t (a1, b1))

let eq_class_infos :
  'all_a0.
    (('all_a0 * 'all_a0) -> 'result) ->
      (('all_a0 class_infos) * ('all_a0 class_infos)) -> 'result =
  fun mf_a
    ({
       pci_virt = a0;
       pci_params = a1;
       pci_name = a2;
       pci_expr = a3;
       pci_variance = a4;
       pci_loc = a5
     },
     {
       pci_virt = b0;
       pci_params = b1;
       pci_name = b2;
       pci_expr = b3;
       pci_variance = b4;
       pci_loc = b5
     })
    ->
    (((((Asttypes.eq_virtual_flag (a0, b0)) &&
          ((fun ((a0, a1), (b0, b1)) ->
              (eq_list (Asttypes.eq_loc eq_string) (a0, b0)) &&
                (Location.eq_t (a1, b1)))
             (a1, b1)))
         && (Asttypes.eq_loc eq_string (a2, b2)))
        && (mf_a (a3, b3)))
       &&
       (eq_list
          (fun ((a0, a1), (b0, b1)) ->
             (eq_bool (a0, b0)) && (eq_bool (a1, b1)))
          (a4, b4)))
      && (Location.eq_t (a5, b5))

let rec eq_pattern_desc : (pattern_desc * pattern_desc) -> 'result =
  function
  | (Ppat_any, Ppat_any) -> true
  | (Ppat_var a0, Ppat_var b0) -> Asttypes.eq_loc eq_string (a0, b0)
  | (Ppat_alias (a0, a1), Ppat_alias (b0, b1)) ->
      (eq_pattern (a0, b0)) && (Asttypes.eq_loc eq_string (a1, b1))
  | (Ppat_constant a0, Ppat_constant b0) ->
      Asttypes.eq_constant (a0, b0)
  | (Ppat_tuple a0, Ppat_tuple b0) -> eq_list eq_pattern (a0, b0)
  | (Ppat_construct (a0, a1), Ppat_construct (b0, b1)) ->
      ((Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
         (eq_option eq_pattern (a1, b1)))
  | (Ppat_variant (a0, a1), Ppat_variant (b0, b1)) ->
      (Asttypes.eq_label (a0, b0)) && (eq_option eq_pattern (a1, b1))
  | (Ppat_record (a0, a1), Ppat_record (b0, b1)) ->
      (eq_list
         (fun ((a0, a1), (b0, b1)) ->
            (Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
              (eq_pattern (a1, b1)))
         (a0, b0))
        && (Asttypes.eq_closed_flag (a1, b1))
  | (Ppat_array a0, Ppat_array b0) -> eq_list eq_pattern (a0, b0)
  | (Ppat_or (a0, a1), Ppat_or (b0, b1)) ->
      (eq_pattern (a0, b0)) && (eq_pattern (a1, b1))
  | (Ppat_constraint (a0, a1), Ppat_constraint (b0, b1)) ->
      (eq_pattern (a0, b0)) && (eq_core_type (a1, b1))
  | (Ppat_type a0, Ppat_type b0) ->
      Asttypes.eq_loc Longident.eq_t (a0, b0)
  | (Ppat_lazy a0, Ppat_lazy b0) -> eq_pattern (a0, b0)
  | (Ppat_unpack a0, Ppat_unpack b0) ->
      Asttypes.eq_loc eq_string (a0, b0)
  | (_, _) -> false
and eq_pattern : (pattern * pattern) -> 'result =
  fun
    ({ ppat_desc = a0; ppat_loc = a1 },
     { ppat_desc = b0; ppat_loc = b1 })
    -> (eq_pattern_desc (a0, b0)) && (Location.eq_t (a1, b1))

let rec eq_structure_item_desc :
  (structure_item_desc * structure_item_desc) -> 'result =
  function
  | (Pstr_eval a0, Pstr_eval b0) -> eq_expression (a0, b0)
  | (Pstr_value (a0, a1), Pstr_value (b0, b1)) ->
      (Asttypes.eq_rec_flag (a0, b0)) &&
        (eq_list
           (fun ((a0, a1), (b0, b1)) ->
              (eq_pattern (a0, b0)) && (eq_expression (a1, b1)))
           (a1, b1))
  | (Pstr_primitive (a0, a1), Pstr_primitive (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (eq_value_description (a1, b1))
  | (Pstr_type (a0, a1), Pstr_type (b0, b1)) ->
      (Asttypes.eq_rec_flag (a0, b0)) &&
      eq_list
        (fun ((a0, a1), (b0, b1)) ->
           (Asttypes.eq_loc eq_string (a0, b0)) &&
             (eq_type_declaration (a1, b1)))
        (a1, b1)
  | (Pstr_exception (a0, a1), Pstr_exception (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (eq_exception_declaration (a1, b1))
  | (Pstr_exn_rebind (a0, a1), Pstr_exn_rebind (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (Asttypes.eq_loc Longident.eq_t (a1, b1))
  | (Pstr_module (a0, a1), Pstr_module (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (eq_module_expr (a1, b1))
  | (Pstr_recmodule a0, Pstr_recmodule b0) ->
      eq_list
        (fun ((a0, a1, a2), (b0, b1, b2)) ->
           ((Asttypes.eq_loc eq_string (a0, b0)) &&
              (eq_module_type (a1, b1)))
             && (eq_module_expr (a2, b2)))
        (a0, b0)
  | (Pstr_modtype (a0, a1), Pstr_modtype (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (eq_module_type (a1, b1))
  | (Pstr_open a0, Pstr_open b0) ->
      Asttypes.eq_loc Longident.eq_t (a0, b0)
  | (Pstr_class a0, Pstr_class b0) ->
      eq_list eq_class_declaration (a0, b0)
  | (Pstr_class_type a0, Pstr_class_type b0) ->
      eq_list eq_class_type_declaration (a0, b0)
  | (Pstr_include a0, Pstr_include b0) -> eq_module_expr (a0, b0)
  | (_, _) -> false
and eq_structure_item :
  (structure_item * structure_item) -> 'result =
  fun
    ({ pstr_desc = a0; pstr_loc = a1 },
     { pstr_desc = b0; pstr_loc = b1 })
    -> (eq_structure_item_desc (a0, b0)) && (Location.eq_t (a1, b1))
and eq_structure : (structure * structure) -> 'result =
  fun (a0, a1) -> eq_list eq_structure_item (a0, a1)
and eq_module_expr_desc :
  (module_expr_desc * module_expr_desc) -> 'result =
  function
  | (Pmod_ident a0, Pmod_ident b0) ->
      Asttypes.eq_loc Longident.eq_t (a0, b0)
  | (Pmod_structure a0, Pmod_structure b0) -> eq_structure (a0, b0)
  | (Pmod_functor (a0, a1, a2), Pmod_functor (b0, b1, b2)) ->
      ((Asttypes.eq_loc eq_string (a0, b0)) &&
         (eq_module_type (a1, b1)))
        && (eq_module_expr (a2, b2))
  | (Pmod_apply (a0, a1), Pmod_apply (b0, b1)) ->
      (eq_module_expr (a0, b0)) && (eq_module_expr (a1, b1))
  | (Pmod_constraint (a0, a1), Pmod_constraint (b0, b1)) ->
      (eq_module_expr (a0, b0)) && (eq_module_type (a1, b1))
  | (Pmod_unpack a0, Pmod_unpack b0) -> eq_expression (a0, b0)
  | (_, _) -> false
and eq_module_expr : (module_expr * module_expr) -> 'result =
  fun
    ({ pmod_desc = a0; pmod_loc = a1 },
     { pmod_desc = b0; pmod_loc = b1 })
    -> (eq_module_expr_desc (a0, b0)) && (Location.eq_t (a1, b1))
and eq_with_constraint :
  (with_constraint * with_constraint) -> 'result =
  function
  | (Pwith_type a0, Pwith_type b0) -> eq_type_declaration (a0, b0)
  | (Pwith_module a0, Pwith_module b0) ->
      Asttypes.eq_loc Longident.eq_t (a0, b0)
  | (Pwith_typesubst a0, Pwith_typesubst b0) ->
      eq_type_declaration (a0, b0)
  | (Pwith_modsubst a0, Pwith_modsubst b0) ->
      Asttypes.eq_loc Longident.eq_t (a0, b0)
  | (_, _) -> false
and eq_modtype_declaration :
  (modtype_declaration * modtype_declaration) -> 'result =
  function
  | (Pmodtype_abstract, Pmodtype_abstract) -> true
  | (Pmodtype_manifest a0, Pmodtype_manifest b0) ->
      eq_module_type (a0, b0)
  | (_, _) -> false
and eq_signature_item_desc :
  (signature_item_desc * signature_item_desc) -> 'result =
  function
  | (Psig_value (a0, a1), Psig_value (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (eq_value_description (a1, b1))
  | (Psig_type (a0, a1), Psig_type (b0, b1)) ->
      (Asttypes.eq_rec_flag (a0, b0)) &&
      eq_list
        (fun ((a0, a1), (b0, b1)) ->
           (Asttypes.eq_loc eq_string (a0, b0)) &&
             (eq_type_declaration (a1, b1)))
        (a1, b1)
  | (Psig_exception (a0, a1), Psig_exception (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (eq_exception_declaration (a1, b1))
  | (Psig_module (a0, a1), Psig_module (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (eq_module_type (a1, b1))
  | (Psig_recmodule a0, Psig_recmodule b0) ->
      eq_list
        (fun ((a0, a1), (b0, b1)) ->
           (Asttypes.eq_loc eq_string (a0, b0)) &&
             (eq_module_type (a1, b1)))
        (a0, b0)
  | (Psig_modtype (a0, a1), Psig_modtype (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (eq_modtype_declaration (a1, b1))
  | (Psig_open a0, Psig_open b0) ->
      Asttypes.eq_loc Longident.eq_t (a0, b0)
  | (Psig_include a0, Psig_include b0) -> eq_module_type (a0, b0)
  | (Psig_class a0, Psig_class b0) ->
      eq_list eq_class_description (a0, b0)
  | (Psig_class_type a0, Psig_class_type b0) ->
      eq_list eq_class_type_declaration (a0, b0)
  | (_, _) -> false
and eq_signature_item :
  (signature_item * signature_item) -> 'result =
  fun
    ({ psig_desc = a0; psig_loc = a1 },
     { psig_desc = b0; psig_loc = b1 })
    -> (eq_signature_item_desc (a0, b0)) && (Location.eq_t (a1, b1))
and eq_signature : (signature * signature) -> 'result =
  fun (a0, a1) -> eq_list eq_signature_item (a0, a1)
and eq_module_type_desc :
  (module_type_desc * module_type_desc) -> 'result =
  function
  | (Pmty_ident a0, Pmty_ident b0) ->
      Asttypes.eq_loc Longident.eq_t (a0, b0)
  | (Pmty_signature a0, Pmty_signature b0) -> eq_signature (a0, b0)
  | (Pmty_functor (a0, a1, a2), Pmty_functor (b0, b1, b2)) ->
      ((Asttypes.eq_loc eq_string (a0, b0)) &&
         (eq_module_type (a1, b1)))
        && (eq_module_type (a2, b2))
  | (Pmty_with (a0, a1), Pmty_with (b0, b1)) ->
      (eq_module_type (a0, b0)) &&
        (eq_list
           (fun ((a0, a1), (b0, b1)) ->
              (Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
                (eq_with_constraint (a1, b1)))
           (a1, b1))
  | (Pmty_typeof a0, Pmty_typeof b0) -> eq_module_expr (a0, b0)
  | (_, _) -> false
and eq_module_type : (module_type * module_type) -> 'result =
  fun
    ({ pmty_desc = a0; pmty_loc = a1 },
     { pmty_desc = b0; pmty_loc = b1 })
    -> (eq_module_type_desc (a0, b0)) && (Location.eq_t (a1, b1))
and eq_class_declaration :
  (class_declaration * class_declaration) -> 'result =
  fun (a0, a1) -> eq_class_infos eq_class_expr (a0, a1)
and eq_class_field_desc :
  (class_field_desc * class_field_desc) -> 'result =
  function
  | (Pcf_inher (a0, a1, a2), Pcf_inher (b0, b1, b2)) ->
      ((Asttypes.eq_override_flag (a0, b0)) &&
         (eq_class_expr (a1, b1)))
        && (eq_option eq_string (a2, b2))
  | (Pcf_valvirt a0, Pcf_valvirt b0) ->
      (fun ((a0, a1, a2), (b0, b1, b2)) ->
         ((Asttypes.eq_loc eq_string (a0, b0)) &&
            (Asttypes.eq_mutable_flag (a1, b1)))
           && (eq_core_type (a2, b2)))
        (a0, b0)
  | (Pcf_val a0, Pcf_val b0) ->
      (fun ((a0, a1, a2, a3), (b0, b1, b2, b3)) ->
         (((Asttypes.eq_loc eq_string (a0, b0)) &&
             (Asttypes.eq_mutable_flag (a1, b1)))
            && (Asttypes.eq_override_flag (a2, b2)))
           && (eq_expression (a3, b3)))
        (a0, b0)
  | (Pcf_virt a0, Pcf_virt b0) ->
      (fun ((a0, a1, a2), (b0, b1, b2)) ->
         ((Asttypes.eq_loc eq_string (a0, b0)) &&
            (Asttypes.eq_private_flag (a1, b1)))
           && (eq_core_type (a2, b2)))
        (a0, b0)
  | (Pcf_meth a0, Pcf_meth b0) ->
      (fun ((a0, a1, a2, a3), (b0, b1, b2, b3)) ->
         (((Asttypes.eq_loc eq_string (a0, b0)) &&
             (Asttypes.eq_private_flag (a1, b1)))
            && (Asttypes.eq_override_flag (a2, b2)))
           && (eq_expression (a3, b3)))
        (a0, b0)
  | (Pcf_constr a0, Pcf_constr b0) ->
      (fun ((a0, a1), (b0, b1)) ->
         (eq_core_type (a0, b0)) && (eq_core_type (a1, b1)))
        (a0, b0)
  | (Pcf_init a0, Pcf_init b0) -> eq_expression (a0, b0)
  | (_, _) -> false
and eq_class_field : (class_field * class_field) -> 'result =
  fun
    ({ pcf_desc = a0; pcf_loc = a1 }, { pcf_desc = b0; pcf_loc = b1
     })
    -> (eq_class_field_desc (a0, b0)) && (Location.eq_t (a1, b1))
and eq_class_structure :
  (class_structure * class_structure) -> 'result =
  fun
    ({ pcstr_self = a0; pcstr_fields = a1 },
     { pcstr_self = b0; pcstr_fields = b1 })
    -> (eq_pattern (a0, b0)) && (eq_list eq_class_field (a1, b1))
and eq_class_expr_desc :
  (class_expr_desc * class_expr_desc) -> 'result =
  function
  | (Pcl_constr (a0, a1), Pcl_constr (b0, b1)) ->
      (Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
        (eq_list eq_core_type (a1, b1))
  | (Pcl_structure a0, Pcl_structure b0) ->
      eq_class_structure (a0, b0)
  | (Pcl_fun (a0, a1, a2, a3), Pcl_fun (b0, b1, b2, b3)) ->
      (((Asttypes.eq_label (a0, b0)) &&
          (eq_option eq_expression (a1, b1)))
         && (eq_pattern (a2, b2)))
        && (eq_class_expr (a3, b3))
  | (Pcl_apply (a0, a1), Pcl_apply (b0, b1)) ->
      (eq_class_expr (a0, b0)) &&
        (eq_list
           (fun ((a0, a1), (b0, b1)) ->
              (Asttypes.eq_label (a0, b0)) &&
                (eq_expression (a1, b1)))
           (a1, b1))
  | (Pcl_let (a0, a1, a2), Pcl_let (b0, b1, b2)) ->
      ((Asttypes.eq_rec_flag (a0, b0)) &&
         (eq_list
            (fun ((a0, a1), (b0, b1)) ->
               (eq_pattern (a0, b0)) && (eq_expression (a1, b1)))
            (a1, b1)))
        && (eq_class_expr (a2, b2))
  | (Pcl_constraint (a0, a1), Pcl_constraint (b0, b1)) ->
      (eq_class_expr (a0, b0)) && (eq_class_type (a1, b1))
  | (_, _) -> false
and eq_class_expr : (class_expr * class_expr) -> 'result =
  fun
    ({ pcl_desc = a0; pcl_loc = a1 }, { pcl_desc = b0; pcl_loc = b1
     })
    -> (eq_class_expr_desc (a0, b0)) && (Location.eq_t (a1, b1))
and eq_class_type_declaration :
  (class_type_declaration * class_type_declaration) -> 'result =
  fun (a0, a1) -> eq_class_infos eq_class_type (a0, a1)
and eq_class_description :
  (class_description * class_description) -> 'result =
  fun (a0, a1) -> eq_class_infos eq_class_type (a0, a1)
and eq_class_type_field_desc :
  (class_type_field_desc * class_type_field_desc) -> 'result =
  function
  | (Pctf_inher a0, Pctf_inher b0) -> eq_class_type (a0, b0)
  | (Pctf_val a0, Pctf_val b0) ->
      (fun ((a0, a1, a2, a3), (b0, b1, b2, b3)) ->
         (((eq_string (a0, b0)) &&
             (Asttypes.eq_mutable_flag (a1, b1)))
            && (Asttypes.eq_virtual_flag (a2, b2)))
           && (eq_core_type (a3, b3)))
        (a0, b0)
  | (Pctf_virt a0, Pctf_virt b0) ->
      (fun ((a0, a1, a2), (b0, b1, b2)) ->
         ((eq_string (a0, b0)) && (Asttypes.eq_private_flag (a1, b1)))
           && (eq_core_type (a2, b2)))
        (a0, b0)
  | (Pctf_meth a0, Pctf_meth b0) ->
      (fun ((a0, a1, a2), (b0, b1, b2)) ->
         ((eq_string (a0, b0)) && (Asttypes.eq_private_flag (a1, b1)))
           && (eq_core_type (a2, b2)))
        (a0, b0)
  | (Pctf_cstr a0, Pctf_cstr b0) ->
      (fun ((a0, a1), (b0, b1)) ->
         (eq_core_type (a0, b0)) && (eq_core_type (a1, b1)))
        (a0, b0)
  | (_, _) -> false
and eq_class_type_field :
  (class_type_field * class_type_field) -> 'result =
  fun
    ({ pctf_desc = a0; pctf_loc = a1 },
     { pctf_desc = b0; pctf_loc = b1 })
    ->
    (eq_class_type_field_desc (a0, b0)) && (Location.eq_t (a1, b1))
and eq_class_signature :
  (class_signature * class_signature) -> 'result =
  fun
    ({ pcsig_self = a0; pcsig_fields = a1; pcsig_loc = a2 },
     { pcsig_self = b0; pcsig_fields = b1; pcsig_loc = b2 })
    ->
    ((eq_core_type (a0, b0)) &&
       (eq_list eq_class_type_field (a1, b1)))
      && (Location.eq_t (a2, b2))
and eq_class_type_desc :
  (class_type_desc * class_type_desc) -> 'result =
  function
  | (Pcty_constr (a0, a1), Pcty_constr (b0, b1)) ->
      (Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
        (eq_list eq_core_type (a1, b1))
  | (Pcty_signature a0, Pcty_signature b0) ->
      eq_class_signature (a0, b0)
  | (Pcty_arrow (a0, a1, a2), Pcty_arrow (b0, b1, b2)) ->
      ((Asttypes.eq_label (a0, b0)) && (eq_core_type (a1, b1))) &&
        (eq_class_type (a2, b2))
  | (_, _) -> false
and eq_class_type : (class_type * class_type) -> 'result =
  fun
    ({ pcty_desc = a0; pcty_loc = a1 },
     { pcty_desc = b0; pcty_loc = b1 })
    -> (eq_class_type_desc (a0, b0)) && (Location.eq_t (a1, b1))
and eq_exception_declaration :
  (exception_declaration * exception_declaration) -> 'result =
  fun (a0, a1) -> eq_list eq_core_type (a0, a1)
and eq_type_kind : (type_kind * type_kind) -> 'result =
  function
  | (Ptype_abstract, Ptype_abstract) -> true
  | (Ptype_variant a0, Ptype_variant b0) ->
      eq_list
        (fun ((a0, a1, a2, a3), (b0, b1, b2, b3)) ->
           (((Asttypes.eq_loc eq_string (a0, b0)) &&
               (eq_list eq_core_type (a1, b1)))
              && (eq_option eq_core_type (a2, b2)))
             && (Location.eq_t (a3, b3)))
        (a0, b0)
  | (Ptype_record a0, Ptype_record b0) ->
      eq_list
        (fun ((a0, a1, a2, a3), (b0, b1, b2, b3)) ->
           (((Asttypes.eq_loc eq_string (a0, b0)) &&
               (Asttypes.eq_mutable_flag (a1, b1)))
              && (eq_core_type (a2, b2)))
             && (Location.eq_t (a3, b3)))
        (a0, b0)
  | (_, _) -> false
and eq_type_declaration :
  (type_declaration * type_declaration) -> 'result =
  fun
    ({
       ptype_params = a0;
       ptype_cstrs = a1;
       ptype_kind = a2;
       ptype_private = a3;
       ptype_manifest = a4;
       ptype_variance = a5;
       ptype_loc = a6
     },
     {
       ptype_params = b0;
       ptype_cstrs = b1;
       ptype_kind = b2;
       ptype_private = b3;
       ptype_manifest = b4;
       ptype_variance = b5;
       ptype_loc = b6
     })
    ->
    ((((((eq_list (eq_option (Asttypes.eq_loc eq_string)) (a0, b0))
           &&
           (eq_list
              (fun ((a0, a1, a2), (b0, b1, b2)) ->
                 ((eq_core_type (a0, b0)) && (eq_core_type (a1, b1)))
                   && (Location.eq_t (a2, b2)))
              (a1, b1)))
          && (eq_type_kind (a2, b2)))
         && (Asttypes.eq_private_flag (a3, b3)))
        && (eq_option eq_core_type (a4, b4)))
       &&
       (eq_list
          (fun ((a0, a1), (b0, b1)) ->
             (eq_bool (a0, b0)) && (eq_bool (a1, b1)))
          (a5, b5)))
      && (Location.eq_t (a6, b6))
and eq_value_description :
  (value_description * value_description) -> 'result =
  fun
    ({ pval_type = a0; pval_prim = a1; pval_loc = a2 },
     { pval_type = b0; pval_prim = b1; pval_loc = b2 })
    ->
    ((eq_core_type (a0, b0)) && (eq_list eq_string (a1, b1))) &&
      (Location.eq_t (a2, b2))
and eq_expression_desc :
  (expression_desc * expression_desc) -> 'result =
  function
  | (Pexp_ident a0, Pexp_ident b0) ->
      Asttypes.eq_loc Longident.eq_t (a0, b0)
  | (Pexp_constant a0, Pexp_constant b0) ->
      Asttypes.eq_constant (a0, b0)
  | (Pexp_let (a0, a1, a2), Pexp_let (b0, b1, b2)) ->
      ((Asttypes.eq_rec_flag (a0, b0)) &&
         (eq_list
            (fun ((a0, a1), (b0, b1)) ->
               (eq_pattern (a0, b0)) && (eq_expression (a1, b1)))
            (a1, b1)))
        && (eq_expression (a2, b2))
  | Pexp_fun (a1, a1, a2, a3), Pexp_function (b0, b1, b2, b3) ->
      ((Asttypes.eq_label (a0, b0)) &&
       (eq_option eq_expression (a1, b1)) &&
       (eq_pattern a2 b2) &&
       (eq_expression (a3, b3)))
  | (Pexp_function (a0, a1, a2), Pexp_function (b0, b1, b2)) ->
      (* FIX *)
      eq_list
        (fun ((a0, a1), (b0, b1)) ->
          (eq_pattern (a0, b0)) && (eq_expression (a1, b1)))
        (a2, b2)
  | (Pexp_apply (a0, a1), Pexp_apply (b0, b1)) ->
      (eq_expression (a0, b0)) &&
        (eq_list
           (fun ((a0, a1), (b0, b1)) ->
              (Asttypes.eq_label (a0, b0)) &&
                (eq_expression (a1, b1)))
           (a1, b1))
  | (Pexp_match (a0, a1), Pexp_match (b0, b1)) ->
      (eq_expression (a0, b0)) &&
        (eq_list
           (fun ((a0, a1), (b0, b1)) ->
              (eq_pattern (a0, b0)) && (eq_expression (a1, b1)))
           (a1, b1))
  | (Pexp_try (a0, a1), Pexp_try (b0, b1)) ->
      (eq_expression (a0, b0)) &&
        (eq_list
           (fun ((a0, a1), (b0, b1)) ->
              (eq_pattern (a0, b0)) && (eq_expression (a1, b1)))
           (a1, b1))
  | (Pexp_tuple a0, Pexp_tuple b0) -> eq_list eq_expression (a0, b0)
  | (Pexp_construct (a0, a1), Pexp_construct (b0, b1)) ->
      ((Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
         (eq_option eq_expression (a1, b1)))
  | (Pexp_variant (a0, a1), Pexp_variant (b0, b1)) ->
      (Asttypes.eq_label (a0, b0)) &&
        (eq_option eq_expression (a1, b1))
  | (Pexp_record (a0, a1), Pexp_record (b0, b1)) ->
      (eq_list
         (fun ((a0, a1), (b0, b1)) ->
            (Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
              (eq_expression (a1, b1)))
         (a0, b0))
        && (eq_option eq_expression (a1, b1))
  | (Pexp_field (a0, a1), Pexp_field (b0, b1)) ->
      (eq_expression (a0, b0)) &&
        (Asttypes.eq_loc Longident.eq_t (a1, b1))
  | (Pexp_setfield (a0, a1, a2), Pexp_setfield (b0, b1, b2)) ->
      ((eq_expression (a0, b0)) &&
         (Asttypes.eq_loc Longident.eq_t (a1, b1)))
        && (eq_expression (a2, b2))
  | (Pexp_array a0, Pexp_array b0) -> eq_list eq_expression (a0, b0)
  | (Pexp_ifthenelse (a0, a1, a2), Pexp_ifthenelse (b0, b1, b2)) ->
      ((eq_expression (a0, b0)) && (eq_expression (a1, b1))) &&
        (eq_option eq_expression (a2, b2))
  | (Pexp_sequence (a0, a1), Pexp_sequence (b0, b1)) ->
      (eq_expression (a0, b0)) && (eq_expression (a1, b1))
  | (Pexp_while (a0, a1), Pexp_while (b0, b1)) ->
      (eq_expression (a0, b0)) && (eq_expression (a1, b1))
  | (Pexp_for (a0, a1, a2, a3, a4), Pexp_for (b0, b1, b2, b3, b4)) ->
      ((((Asttypes.eq_loc eq_string (a0, b0)) &&
           (eq_expression (a1, b1)))
          && (eq_expression (a2, b2)))
         && (Asttypes.eq_direction_flag (a3, b3)))
        && (eq_expression (a4, b4))
  | (Pexp_constraint (a0, a1, a2), Pexp_constraint (b0, b1, b2)) ->
      ((eq_expression (a0, b0)) && (eq_option eq_core_type (a1, b1)))
        && (eq_option eq_core_type (a2, b2))
  | (Pexp_when (a0, a1), Pexp_when (b0, b1)) ->
      (eq_expression (a0, b0)) && (eq_expression (a1, b1))
  | (Pexp_send (a0, a1), Pexp_send (b0, b1)) ->
      (eq_expression (a0, b0)) && (eq_string (a1, b1))
  | (Pexp_new a0, Pexp_new b0) ->
      Asttypes.eq_loc Longident.eq_t (a0, b0)
  | (Pexp_setinstvar (a0, a1), Pexp_setinstvar (b0, b1)) ->
      (Asttypes.eq_loc eq_string (a0, b0)) &&
        (eq_expression (a1, b1))
  | (Pexp_override a0, Pexp_override b0) ->
      eq_list
        (fun ((a0, a1), (b0, b1)) ->
           (Asttypes.eq_loc eq_string (a0, b0)) &&
             (eq_expression (a1, b1)))
        (a0, b0)
  | (Pexp_letmodule (a0, a1, a2), Pexp_letmodule (b0, b1, b2)) ->
      ((Asttypes.eq_loc eq_string (a0, b0)) &&
         (eq_module_expr (a1, b1)))
        && (eq_expression (a2, b2))
  | (Pexp_assert a0, Pexp_assert b0) -> eq_expression (a0, b0)
  | (Pexp_lazy a0, Pexp_lazy b0) -> eq_expression (a0, b0)
  | (Pexp_poly (a0, a1), Pexp_poly (b0, b1)) ->
      (eq_expression (a0, b0)) && (eq_option eq_core_type (a1, b1))
  | (Pexp_object a0, Pexp_object b0) -> eq_class_structure (a0, b0)
  | (Pexp_newtype (a0, a1), Pexp_newtype (b0, b1)) ->
      (eq_string (a0, b0)) && (eq_expression (a1, b1))
  | (Pexp_pack a0, Pexp_pack b0) -> eq_module_expr (a0, b0)
  | (Pexp_open (a0, a1), Pexp_open (b0, b1)) ->
      (Asttypes.eq_loc Longident.eq_t (a0, b0)) &&
        (eq_expression (a1, b1))
  | (_, _) -> false
and eq_expression : (expression * expression) -> 'result =
  fun
    ({ pexp_desc = a0; pexp_loc = a1 },
     { pexp_desc = b0; pexp_loc = b1 })
    -> (eq_expression_desc (a0, b0)) && (Location.eq_t (a1, b1))

let rec eq_directive_argument :
  (directive_argument * directive_argument) -> 'result =
  function
  | (Pdir_none, Pdir_none) -> true
  | (Pdir_string a0, Pdir_string b0) -> eq_string (a0, b0)
  | (Pdir_int a0, Pdir_int b0) -> eq_int (a0, b0)
  | (Pdir_ident a0, Pdir_ident b0) -> Longident.eq_t (a0, b0)
  | (Pdir_bool a0, Pdir_bool b0) -> eq_bool (a0, b0)
  | (_, _) -> false
and eq_toplevel_phrase :
  (toplevel_phrase * toplevel_phrase) -> 'result =
  function
  | (Ptop_def a0, Ptop_def b0) -> eq_structure (a0, b0)
  | (Ptop_dir (a0, a1), Ptop_dir (b0, b1)) ->
      (eq_string (a0, b0)) && (eq_directive_argument (a1, b1))
  | (_, _) -> false
