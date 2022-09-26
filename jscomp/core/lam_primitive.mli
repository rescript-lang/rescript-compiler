(* Copyright (C) 2018 - Hongbo Zhang, Authors of ReScript
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

type ident = Ident.t

type record_representation =
  | Record_regular
  | Record_inlined of { tag : int; name : string; num_nonconsts : int } (* Inlined record *)
  | Record_extension
(* Inlined record under extension *)

type t =
  | Pbytes_to_string
  | Pmakeblock of int * Lam_tag_info.t * Asttypes.mutable_flag
  | Pfield of int * Lambda.field_dbg_info
  | Psetfield of int * Lambda.set_field_dbg_info
  | Pduprecord
  | Plazyforce
  | Pccall of { prim_name : string }
  | Pjs_call of {
      (* Location.t *  [loc] is passed down *)
      prim_name : string;
      arg_types : External_arg_spec.params;
      ffi : External_ffi_types.external_spec;
    }
  | Pjs_object_create of External_arg_spec.obj_params
  | Praise
  | Psequand
  | Psequor
  | Pnot
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
  (* Operations on boxed integers (Nativeint.t, Int32.t, Int64.t) *)
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
  | Pctconst of Lam_compat.compile_time_constant
  (* Integer to external pointer *)
  | Pdebugger
  | Pjs_unsafe_downgrade of { name : string; setter : bool }
  | Pinit_mod
  | Pupdate_mod
  | Praw_js_code of Js_raw_info.t
  | Pjs_fn_make of int
  | Pvoid_run
  | Pfull_apply
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
  | Pis_not_none
  | Pval_from_option
  | Pval_from_option_not_nest
  | Psome
  | Psome_not_nest

val eq_primitive_approx : t -> t -> bool
