(* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

[@@@ocaml.warning "+9"]

type ident = Ident.t

type record_representation =
  | Record_regular
  | Record_inlined of { tag : int; name : string; num_nonconsts : int } (* Inlined record *)
  | Record_extension
(* Inlined record under extension *)

type t =
  | Pbytes_to_string
  (* Operations on heap blocks *)
  | Pmakeblock of int * Lam_tag_info.t * Asttypes.mutable_flag
  | Pfield of int * Lam_compat.field_dbg_info
  | Psetfield of int * Lam_compat.set_field_dbg_info
  (* could have field info at least for record *)
  | Pduprecord
  (* Force lazy values *)
  | Plazyforce
  (* External call *)
  | Pccall of { prim_name : string }
  | Pjs_call of {
      prim_name : string;
      arg_types : External_arg_spec.params;
      ffi : External_ffi_types.external_spec;
    }
  | Pjs_object_create of External_arg_spec.obj_params
  (* Exceptions *)
  | Praise
  (* Boolean operations *)
  | Psequand
  | Psequor
  | Pnot
  (* Integer operations *)
  | Pnegint
  | Paddint
  | Psubint
  | Pmulint
  | Pdivint
  | Pmodint
  | Pandint
  | Porint
  | Pxorint
  | Plslint
  | Plsrint
  | Pasrint
  | Poffsetint of int
  | Poffsetref of int
  (* Float operations *)
  | Pintoffloat
  | Pfloatofint
  | Pnegfloat
  | Paddfloat
  | Psubfloat
  | Pmulfloat
  | Pdivfloat
  | Pintcomp of Lam_compat.comparison
  | Pfloatcomp of Lam_compat.comparison
  | Pjscomp of Lam_compat.comparison
  | Pint64comp of Lam_compat.comparison
  | Pjs_apply (*[f;arg0;arg1; arg2; ... argN]*)
  | Pjs_runtime_apply (* [f; [...]] *)
  (* String operations *)
  | Pstringlength
  | Pstringrefu
  | Pstringrefs
  | Pstringadd
  | Pbyteslength
  | Pbytesrefu
  | Pbytessetu
  | Pbytesrefs
  | Pbytessets
  (* Array operations *)
  | Pmakearray
  | Parraylength
  | Parrayrefu
  | Parraysetu
  | Parrayrefs
  | Parraysets
  (* Test if the argument is a block or an immediate integer *)
  | Pisint
  | Pis_poly_var_block
  (* Test if the (integer) argument is outside an interval *)
  | Pisout of int
  | Pint64ofint
  | Pintofint64
  | Pnegint64
  | Paddint64
  | Psubint64
  | Pmulint64
  | Pdivint64
  | Pmodint64
  | Pandint64
  | Porint64
  | Pxorint64
  | Plslint64
  | Plsrint64
  | Pasrint64
  (* Compile time constants *)
  | Pctconst of Lam_compat.compile_time_constant (* Integer to external pointer *)
  | Pdebugger
  | Pjs_unsafe_downgrade of { name : string; setter : bool }
  | Pinit_mod
  | Pupdate_mod
  | Praw_js_code of Js_raw_info.t
  | Pjs_fn_make of int
  | Pvoid_run
  | Pfull_apply
  (* we wrap it when do the conversion to prevent
     accendential optimization
     play safe first
  *)
  | Pjs_fn_method
  | Pundefined_to_opt
  | Pnull_to_opt
  | Pnull_undefined_to_opt
  | Pis_null
  | Pis_undefined
  | Pis_null_undefined
  | Pimport
  | Pjs_typeof
  | Pjs_function_length
  | Pcaml_obj_length
  | Pwrap_exn (* convert either JS exception or OCaml exception into OCaml format *)
  | Pcreate_extension of string
  | Pis_not_none (* no info about its type *)
  | Pval_from_option
  | Pval_from_option_not_nest
  | Psome
  | Psome_not_nest

let eq_field_dbg_info (x : Lam_compat.field_dbg_info)
    (y : Lam_compat.field_dbg_info) =
  x = y
(* save it to avoid conditional compilation, fix it later *)

let eq_set_field_dbg_info (x : Lam_compat.set_field_dbg_info)
    (y : Lam_compat.set_field_dbg_info) =
  x = y
(* save it to avoid conditional compilation, fix it later *)

let eq_tag_info (x : Lam_tag_info.t) y = x = y

let eq_primitive_approx (lhs : t) (rhs : t) =
  match lhs with
  | Pcreate_extension a -> (
      match rhs with Pcreate_extension b -> a = (b : string) | _ -> false)
  | Pwrap_exn -> rhs = Pwrap_exn
  | Pbytes_to_string -> rhs = Pbytes_to_string
  | Praise -> rhs = Praise
  | Psequand -> rhs = Psequand
  | Psequor -> rhs = Psequor
  | Pnot -> rhs = Pnot
  | Pnegint -> rhs = Pnegint
  | Paddint -> rhs = Paddint
  | Psubint -> rhs = Psubint
  | Pmulint -> rhs = Pmulint
  | Pdivint -> rhs = Pdivint
  | Pmodint -> rhs = Pmodint
  | Pandint -> rhs = Pandint
  | Porint -> rhs = Porint
  | Pxorint -> rhs = Pxorint
  | Plslint -> rhs = Plslint
  | Plsrint -> rhs = Plsrint
  | Pasrint -> rhs = Pasrint
  | Pval_from_option -> rhs = Pval_from_option
  | Pval_from_option_not_nest -> rhs = Pval_from_option_not_nest
  | Plazyforce -> rhs = Plazyforce
  | Pintoffloat -> rhs = Pintoffloat
  | Pfloatofint -> rhs = Pfloatofint
  | Pnegfloat -> rhs = Pnegfloat
  (* | Pabsfloat -> rhs = Pabsfloat *)
  | Paddfloat -> rhs = Paddfloat
  | Psubfloat -> rhs = Psubfloat
  | Pmulfloat -> rhs = Pmulfloat
  | Pdivfloat -> rhs = Pdivfloat
  | Pjs_apply -> rhs = Pjs_apply
  | Pjs_runtime_apply -> rhs = Pjs_runtime_apply
  | Pstringlength -> rhs = Pstringlength
  | Pstringrefu -> rhs = Pstringrefu
  | Pstringrefs -> rhs = Pstringrefs
  | Pstringadd -> rhs = Pstringadd
  | Pbyteslength -> rhs = Pbyteslength
  | Pbytesrefu -> rhs = Pbytesrefu
  | Pbytessetu -> rhs = Pbytessetu
  | Pbytesrefs -> rhs = Pbytesrefs
  | Pbytessets -> rhs = Pbytessets
  | Pundefined_to_opt -> rhs = Pundefined_to_opt
  | Pnull_to_opt -> rhs = Pnull_to_opt
  | Pnull_undefined_to_opt -> rhs = Pnull_undefined_to_opt
  | Pis_null -> rhs = Pis_null
  | Pis_not_none -> rhs = Pis_not_none
  | Psome -> rhs = Psome
  | Psome_not_nest -> rhs = Psome_not_nest
  | Pis_undefined -> rhs = Pis_undefined
  | Pis_null_undefined -> rhs = Pis_null_undefined
  | Pimport -> rhs = Pimport
  | Pjs_typeof -> rhs = Pjs_typeof
  | Pisint -> rhs = Pisint
  | Pis_poly_var_block -> rhs = Pis_poly_var_block
  | Pisout l -> ( match rhs with Pisout r -> l = r | _ -> false)
  | Pdebugger -> rhs = Pdebugger
  | Pinit_mod -> rhs = Pinit_mod
  | Pupdate_mod -> rhs = Pupdate_mod
  | Pjs_function_length -> rhs = Pjs_function_length
  (* | Pjs_string_of_small_array -> rhs = Pjs_string_of_small_array *)
  (* | Pjs_is_instance_array -> rhs = Pjs_is_instance_array *)
  | Pcaml_obj_length -> rhs = Pcaml_obj_length
  (* | Pcaml_obj_set_length -> rhs = Pcaml_obj_set_length *)
  | Pccall { prim_name = n0 } -> (
      match rhs with Pccall { prim_name = n1 } -> n0 = n1 | _ -> false)
  | Pfield (n0, info0) -> (
      match rhs with
      | Pfield (n1, info1) -> n0 = n1 && eq_field_dbg_info info0 info1
      | _ -> false)
  | Psetfield (i0, info0) -> (
      match rhs with
      | Psetfield (i1, info1) -> i0 = i1 && eq_set_field_dbg_info info0 info1
      | _ -> false)
  | Pmakeblock (i0, info0, flag0) -> (
      match rhs with
      | Pmakeblock (i1, info1, flag1) ->
          i0 = i1 && flag0 = flag1 && eq_tag_info info0 info1
      | _ -> false)
  | Pduprecord -> rhs = Pduprecord
  | Pjs_call { prim_name; arg_types; ffi } -> (
      match rhs with
      | Pjs_call rhs ->
          prim_name = rhs.prim_name && arg_types = rhs.arg_types
          && ffi = rhs.ffi
      | _ -> false)
  | Pjs_object_create obj_create -> (
      match rhs with
      | Pjs_object_create obj_create1 -> obj_create = obj_create1
      | _ -> false)
  | Pintcomp comparison -> (
      match rhs with
      | Pintcomp comparison1 -> Lam_compat.eq_comparison comparison comparison1
      | _ -> false)
  | Pfloatcomp comparison -> (
      match rhs with
      | Pfloatcomp comparison1 ->
          Lam_compat.eq_comparison comparison comparison1
      | _ -> false)
  | Pjscomp comparison -> (
      match rhs with
      | Pjscomp comparison1 -> Lam_compat.eq_comparison comparison comparison1
      | _ -> false)
  | Poffsetint i0 -> ( match rhs with Poffsetint i1 -> i0 = i1 | _ -> false)
  | Poffsetref i0 -> ( match rhs with Poffsetref i1 -> i0 = i1 | _ -> false)
  | Pmakearray -> rhs = Pmakearray
  | Parraylength -> rhs = Parraylength
  | Parrayrefu -> rhs = Parrayrefu
  | Parraysetu -> rhs = Parraysetu
  | Parrayrefs -> rhs = Parrayrefs
  | Parraysets -> rhs = Parraysets
  | Pint64ofint -> rhs = Pint64ofint
  | Pintofint64 -> rhs = Pintofint64
  | Pnegint64 -> rhs = Pnegint64
  | Paddint64 -> rhs = Paddint64
  | Psubint64 -> rhs = Psubint64
  | Pmulint64 -> rhs = Pmulint64
  | Pdivint64 -> rhs = Pdivint64
  | Pmodint64 -> rhs = Pmodint64
  | Pandint64 -> rhs = Pandint64
  | Porint64 -> rhs = Porint64
  | Pxorint64 -> rhs = Pxorint64
  | Plslint64 -> rhs = Plslint64
  | Plsrint64 -> rhs = Plsrint64
  | Pasrint64 -> rhs = Pasrint64
  | Pint64comp comparison -> (
      match rhs with
      | Pint64comp comparison1 ->
          Lam_compat.eq_comparison comparison comparison1
      | _ -> false)
  | Pctconst compile_time_constant -> (
      match rhs with
      | Pctconst compile_time_constant1 ->
          Lam_compat.eq_compile_time_constant compile_time_constant
            compile_time_constant1
      | _ -> false)
  | Pjs_unsafe_downgrade { name; setter } -> (
      match rhs with
      | Pjs_unsafe_downgrade rhs -> name = rhs.name && setter = rhs.setter
      | _ -> false)
  | Pjs_fn_make i -> ( match rhs with Pjs_fn_make i1 -> i = i1 | _ -> false)
  | Pvoid_run -> rhs = Pvoid_run
  | Pfull_apply -> rhs = Pfull_apply
  | Pjs_fn_method -> rhs = Pjs_fn_method
  | Praw_js_code _ -> false
(* TOO lazy, here comparison is only approximation*)
