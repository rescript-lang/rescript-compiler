(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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



val get_ocamllex : unit -> string
val set_ocamllex : cwd:string -> string -> unit


val set_bs_external_includes : Bsb_json.t array -> unit
val get_bs_external_includes : unit -> string list




val set_bsc_flags : Bsb_json.t array -> unit
val get_bsc_flags : unit -> string list

val set_ppx_flags : cwd:string -> Bsb_json.t array -> unit
val get_ppx_flags : unit -> string list

val set_package_name : string -> unit
val get_package_name : unit -> string option

val set_refmt : cwd:string -> string -> unit
val get_refmt : unit -> string


val get_bs_dependencies : unit  -> string list
val set_bs_dependencies : Bsb_json.t array  -> unit


val get_js_post_build_cmd : unit -> string option
val set_js_post_build_cmd : cwd:string -> string -> unit

val get_ninja : unit -> string 
val set_ninja : cwd:string -> string -> unit
