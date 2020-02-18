(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Manuel Serrano and Xavier Leroy, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Bigarray types. These must be kept in sync with the tables in
   ../typing/typeopt.ml *)

type float32_elt = Float32_elt
type float64_elt = Float64_elt
type int8_signed_elt = Int8_signed_elt
type int8_unsigned_elt = Int8_unsigned_elt
type int16_signed_elt = Int16_signed_elt
type int16_unsigned_elt = Int16_unsigned_elt
type int32_elt = Int32_elt
type int64_elt = Int64_elt
type int_elt = Int_elt
type nativeint_elt = Nativeint_elt
type complex32_elt = Complex32_elt
type complex64_elt = Complex64_elt

type ('a, 'b) kind =
    Float32 : (float, float32_elt) kind
  | Float64 : (float, float64_elt) kind
  | Int8_signed : (int, int8_signed_elt) kind
  | Int8_unsigned : (int, int8_unsigned_elt) kind
  | Int16_signed : (int, int16_signed_elt) kind
  | Int16_unsigned : (int, int16_unsigned_elt) kind
  | Int32 : (int32, int32_elt) kind
  | Int64 : (int64, int64_elt) kind
  | Int : (int, int_elt) kind
  | Nativeint : (nativeint, nativeint_elt) kind
  | Complex32 : (Complex.t, complex32_elt) kind
  | Complex64 : (Complex.t, complex64_elt) kind
  | Char : (char, int8_unsigned_elt) kind

type c_layout = C_layout_typ
type fortran_layout = Fortran_layout_typ

type 'a layout =
    C_layout: c_layout layout
  | Fortran_layout: fortran_layout layout

type ('a, 'b, 'c) genarray
