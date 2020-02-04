(* Copyright (C) 2017- Authors of BuckleScript
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

val make : ?ns:string -> string -> string
(** [make ~ns:"Ns" "a"] A typical example would return "a-Ns" Note the namespace
    comes from the output of [namespace_of_package_name] *)

val try_split_module_name : string -> (string * string) option

val replace_namespace_with_extension : name:string -> ext:string -> string
(** [replace_namespace_with_extension ~name ~ext] removes the part of [name]
    after [ns_sep_char], if any; and appends [ext].
*)

type file_kind = Upper_js | Upper_bs | Little_js | Little_bs

val js_name_of_modulename : string -> file_kind -> string
(** Predicts the JavaScript filename for a given (possibly namespaced) module-
    name; i.e. [js_name_of_modulename "AA-Ns" Little_bs] would produce
    ["aA.bs.js"]. *)

val is_valid_npm_package_name : string -> bool

val namespace_of_package_name : string -> string
