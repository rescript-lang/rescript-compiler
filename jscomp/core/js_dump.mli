(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
(* Authors: Jérôme Vouillon, Hongbo Zhang  *)



(** Print JS IR to vanilla Javascript code *)



val pp_deps_program :
  output_prefix:string ->
  Js_packages_info.module_system -> J.deps_program -> Ext_pp.t -> unit

val dump_deps_program :
  output_prefix:string ->
  Js_packages_info.module_system  -> J.deps_program -> out_channel -> unit

(** 2 functions Only used for debugging *)
val string_of_block : J.block -> string

val dump_program : J.program -> out_channel -> unit

val string_of_expression : J.expression -> string
