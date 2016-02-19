(* BuckleScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

(* Author: Hongbo Zhang  *)



(** Utilities for quering typing inforaation from {!Env.t}, this part relies
    on compiler API
*)

val query : Path.t -> Env.t -> Types.signature option 

val name_of_signature_item : Types.signature_item -> Ident.t 

val get_name : Types.signature -> int -> string

val filter_serializable_signatures : Types.signature -> Types.signature

val find_serializable_signatures_by_path : Path.t -> Env.t -> Types.signature option

val list_of_arrow : Types.type_expr -> Types.type_desc * (string * Types.type_expr) list

val label_name : string -> [ `Label of string | `Optional of string ]
