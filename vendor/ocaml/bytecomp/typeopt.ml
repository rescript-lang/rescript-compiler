(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Auxiliaries for type-based optimizations, e.g. array kinds *)

open Path
open Types
open Typedtree
open Lambda

let scrape env ty =
  (Ctype.repr (Ctype.expand_head_opt env (Ctype.correct_levels ty))).desc

let has_base_type exp base_ty_path =
  match scrape exp.exp_env exp.exp_type with
  | Tconstr(p, _, _) -> Path.same p base_ty_path
  | _ -> false

let maybe_pointer exp =
  match scrape exp.exp_env exp.exp_type with
  | Tconstr(p, args, abbrev) ->
      not (Path.same p Predef.path_int) &&
      not (Path.same p Predef.path_char) &&
      begin try
        match Env.find_type p exp.exp_env with
        | {type_kind = Type_variant []} -> true (* type exn *)
        | {type_kind = Type_variant cstrs} ->
            List.exists (fun c -> c.Types.cd_args <> []) cstrs
        | _ -> true
      with Not_found -> true
        (* This can happen due to e.g. missing -I options,
           causing some .cmi files to be unavailable.
           Maybe we should emit a warning. *)
      end
  | Tvariant row ->
      let row = Btype.row_repr row in
      not row.row_closed
      || List.exists
           (function
             | _, (Rpresent (Some _)|Reither(false,_,_,_)) -> true
             | _ -> false )
           row.row_fields
  | _ -> true

let array_element_kind env ty =
  match scrape env ty with
  | Tvar _ | Tunivar _ ->
      Pgenarray
  | Tconstr(p, args, abbrev) ->
      if Path.same p Predef.path_int || Path.same p Predef.path_char then
        Pintarray
      else if Path.same p Predef.path_float then
        Pfloatarray
      else if Path.same p Predef.path_string
           || Path.same p Predef.path_array
           || Path.same p Predef.path_nativeint
           || Path.same p Predef.path_int32
           || Path.same p Predef.path_int64 then
        Paddrarray
      else begin
        try
          match Env.find_type p env with
            {type_kind = Type_abstract} ->
              Pgenarray
          | {type_kind = Type_variant cstrs}
            when List.for_all (fun c -> c.Types.cd_args = []) cstrs ->
              Pintarray
          | {type_kind = _} ->
              Paddrarray
        with Not_found ->
          (* This can happen due to e.g. missing -I options,
             causing some .cmi files to be unavailable.
             Maybe we should emit a warning. *)
          Pgenarray
      end
  | _ ->
      Paddrarray

let array_kind_gen ty env =
  match scrape env ty with
  | Tconstr(p, [elt_ty], _) | Tpoly({desc = Tconstr(p, [elt_ty], _)}, _)
    when Path.same p Predef.path_array ->
      array_element_kind env elt_ty
  | _ ->
      (* This can happen with e.g. Obj.field *)
      Pgenarray

let array_kind exp = array_kind_gen exp.exp_type exp.exp_env

let array_pattern_kind pat = array_kind_gen pat.pat_type pat.pat_env

let bigarray_decode_type env ty tbl dfl =
  match scrape env ty with
  | Tconstr(Pdot(Pident mod_id, type_name, _), [], _)
    when Ident.name mod_id = "Bigarray" ->
      begin try List.assoc type_name tbl with Not_found -> dfl end
  | _ ->
      dfl

let kind_table =
  ["float32_elt", Pbigarray_float32;
   "float64_elt", Pbigarray_float64;
   "int8_signed_elt", Pbigarray_sint8;
   "int8_unsigned_elt", Pbigarray_uint8;
   "int16_signed_elt", Pbigarray_sint16;
   "int16_unsigned_elt", Pbigarray_uint16;
   "int32_elt", Pbigarray_int32;
   "int64_elt", Pbigarray_int64;
   "int_elt", Pbigarray_caml_int;
   "nativeint_elt", Pbigarray_native_int;
   "complex32_elt", Pbigarray_complex32;
   "complex64_elt", Pbigarray_complex64]

let layout_table =
  ["c_layout", Pbigarray_c_layout;
   "fortran_layout", Pbigarray_fortran_layout]

let bigarray_kind_and_layout exp =
  match scrape exp.exp_env exp.exp_type with
  | Tconstr(p, [caml_type; elt_type; layout_type], abbrev) ->
      (bigarray_decode_type exp.exp_env elt_type kind_table Pbigarray_unknown,
       bigarray_decode_type exp.exp_env layout_type layout_table
                            Pbigarray_unknown_layout)
  | _ ->
      (Pbigarray_unknown, Pbigarray_unknown_layout)
