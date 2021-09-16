(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Types
open Format

type 'a class_info = {
  cls_id : Ident.t;
  cls_id_loc : string loc;
  cls_decl : class_declaration;
  cls_ty_id : Ident.t;
  cls_ty_decl : class_type_declaration;
  cls_obj_id : Ident.t;
  cls_obj_abbr : type_declaration;
  cls_typesharp_id : Ident.t;
  cls_abbr : type_declaration;
  cls_arity : int;
  cls_pub_methods : string list;
  cls_info : 'a;
}

type class_type_info = {
  clsty_ty_id : Ident.t;
  clsty_id_loc : string loc;
  clsty_ty_decl : class_type_declaration;
  clsty_obj_id : Ident.t;
  clsty_obj_abbr : type_declaration;
  clsty_typesharp_id : Ident.t;
  clsty_abbr : type_declaration;
  clsty_info : Typedtree.class_type_declaration;
}





val class_type_declarations:
  Env.t -> Parsetree.class_type_declaration list -> class_type_info list * Env.t


val approx_class_declarations:
  Env.t -> Parsetree.class_type_declaration list -> class_type_info list

val virtual_methods: Types.class_signature -> label list


type error 

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val report_error : Env.t -> formatter -> error -> unit
