(* Copyright (C) 2018 Authors of BuckleScript
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

type array_kind = Lambda.array_kind =
  | Pgenarray
  | Paddrarray
  | Pintarray
  | Pfloatarray

let eq_array_kind (p : array_kind) (p1 : array_kind) =
  match p with
  | Pgenarray -> p1 = Pgenarray
  | Paddrarray -> p1 = Paddrarray
  | Pintarray -> p1 = Pintarray
  | Pfloatarray -> p1 = Pfloatarray

type boxed_integer = Lambda.boxed_integer = Pnativeint | Pint32 | Pint64

let eq_boxed_integer (p : boxed_integer) (p1 : boxed_integer) =
  match p with
  | Pnativeint -> p1 = Pnativeint
  | Pint32 -> p1 = Pint32
  | Pint64 -> p1 = Pint64

type comparison = Lambda.comparison = Ceq | Cneq | Clt | Cgt | Cle | Cge

let eq_comparison (p : comparison) (p1 : comparison) =
  match p with
  | Cge -> p1 = Cge
  | Cgt -> p1 = Cgt
  | Cle -> p1 = Cle
  | Clt -> p1 = Clt
  | Ceq -> p1 = Ceq
  | Cneq -> p1 = Cneq

let cmp_int32 (cmp : comparison) (a : int32) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cneq -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

let cmp_int64 (cmp : comparison) (a : int64) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cneq -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

let cmp_nativeint (cmp : comparison) (a : nativeint) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cneq -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

let cmp_float (cmp : comparison) (a : float) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cneq -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

let cmp_int (cmp : comparison) (a : int) b : bool =
  match cmp with
  | Ceq -> a = b
  | Cneq -> a <> b
  | Cgt -> a > b
  | Cle -> a <= b
  | Clt -> a < b
  | Cge -> a >= b

type bigarray_kind = Lambda.bigarray_kind =
  | Pbigarray_unknown
  | Pbigarray_float32
  | Pbigarray_float64
  | Pbigarray_sint8
  | Pbigarray_uint8
  | Pbigarray_sint16
  | Pbigarray_uint16
  | Pbigarray_int32
  | Pbigarray_int64
  | Pbigarray_caml_int
  | Pbigarray_native_int
  | Pbigarray_complex32
  | Pbigarray_complex64

let eq_bigarray_kind (p : bigarray_kind) (p1 : bigarray_kind) =
  match p with
  | Pbigarray_unknown -> p1 = Pbigarray_unknown
  | Pbigarray_float32 -> p1 = Pbigarray_float32
  | Pbigarray_float64 -> p1 = Pbigarray_float64
  | Pbigarray_sint8 -> p1 = Pbigarray_sint8
  | Pbigarray_uint8 -> p1 = Pbigarray_uint8
  | Pbigarray_sint16 -> p1 = Pbigarray_sint16
  | Pbigarray_uint16 -> p1 = Pbigarray_uint16
  | Pbigarray_int32 -> p1 = Pbigarray_int32
  | Pbigarray_int64 -> p1 = Pbigarray_int64
  | Pbigarray_caml_int -> p1 = Pbigarray_caml_int
  | Pbigarray_native_int -> p1 = Pbigarray_native_int
  | Pbigarray_complex32 -> p1 = Pbigarray_complex32
  | Pbigarray_complex64 -> p1 = Pbigarray_complex64

type bigarray_layout = Lambda.bigarray_layout =
  | Pbigarray_unknown_layout
  | Pbigarray_c_layout
  | Pbigarray_fortran_layout

let eq_bigarray_layout (p : bigarray_layout) (p1 : bigarray_layout) =
  match p with
  | Pbigarray_unknown_layout -> p1 = Pbigarray_unknown_layout
  | Pbigarray_c_layout -> p1 = Pbigarray_c_layout
  | Pbigarray_fortran_layout -> p1 = Pbigarray_fortran_layout

type compile_time_constant = Lambda.compile_time_constant =
  | Big_endian
  | Word_size
  | Int_size
  | Max_wosize
  | Ostype_unix
  | Ostype_win32
  | Ostype_cygwin
  | Backend_type

(** relies on the fact that [compile_time_constant] is enum type *)
let eq_compile_time_constant (p : compile_time_constant)
    (p1 : compile_time_constant) =
  p = p1

type let_kind = Lambda.let_kind = Strict | Alias | StrictOpt | Variable
type meth_kind = Lambda.meth_kind = Self | Public of string option | Cached

type field_dbg_info = Lambda.field_dbg_info =
  | Fld_na
  | Fld_record of string
  | Fld_module of string
  | Fld_record_inline of string
  | Fld_record_extension of string

type set_field_dbg_info = Lambda.set_field_dbg_info =
  | Fld_set_na
  | Fld_record_set of string
  | Fld_record_inline_set of string
  | Fld_record_extension_set of string
