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

open Lambda

val oo_prim: string -> lambda

val share: structured_constant -> lambda
val meth: lambda -> string -> lambda * lambda list

val reset_labels: unit -> unit
val transl_label_init: (unit -> lambda * 'a) -> lambda * 'a
val transl_store_label_init:
    Ident.t -> int -> ('a -> lambda) -> 'a -> int * lambda

val method_ids: IdentSet.t ref (* reset when starting a new wrapper *)

val oo_wrap: Env.t -> bool -> ('a -> lambda) -> 'a -> lambda
val oo_add_class: Ident.t -> Env.t * bool

val reset: unit -> unit
