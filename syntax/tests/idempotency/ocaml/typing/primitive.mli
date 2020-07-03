(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Description of primitive functions *)

type boxed_integer = Pnativeint | Pint32 | Pint64

(* Representation of arguments/result for the native code version
   of a primitive *)
type native_repr =
  | Same_as_ocaml_repr
  | Unboxed_float
  | Unboxed_integer of boxed_integer
  | Untagged_int

type description = private
  { prim_name: string;         (* Name of primitive  or C function *)
    prim_arity: int;           (* Number of arguments *)
    prim_alloc: bool;          (* Does it allocates or raise? *)
    prim_native_name: string;  (* Name of C function for the nat. code gen. *)
    prim_native_repr_args: native_repr list;
    prim_native_repr_res: native_repr }

(* Invariant [List.length d.prim_native_repr_args = d.prim_arity] *)

val simple
  :  name:string
  -> arity:int
  -> alloc:bool
  -> description

val make
  :  name:string
  -> alloc:bool
  -> native_name:string
  -> native_repr_args: native_repr list
  -> native_repr_res: native_repr
  -> description

val parse_declaration
  :  Parsetree.value_description
  -> native_repr_args:native_repr list
  -> native_repr_res:native_repr
  -> description

val print
  :  description
  -> Outcometree.out_val_decl
  -> Outcometree.out_val_decl

val native_name: description -> string
val byte_name: description -> string

type error =
  | Old_style_float_with_native_repr_attribute
  | Old_style_noalloc_with_noalloc_attribute
  | No_native_primitive_with_repr_attribute

exception Error of Location.t * error
