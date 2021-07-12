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

(* Translation from typed abstract syntax to lambda terms,
   for the module language *)

open Typedtree


val eval_rec_bindings:
      ((Ident.t * (Lambda.lambda * Lambda.lambda) option * Lambda.lambda) list  -> 
      Lambda.lambda -> Lambda.lambda) ref

val transl_implementation:
      string -> structure * module_coercion -> Lambda.program


val primitive_declarations: Primitive.description list ref

type error =
  Circular_dependency of Ident.t
| Conflicting_inline_attributes

(* exception Error of Location.t * error *)

val report_error: Format.formatter -> error -> unit

val reset: unit -> unit

(** make it an array for better performance*)
val get_export_identifiers : unit -> Ident.t list 
