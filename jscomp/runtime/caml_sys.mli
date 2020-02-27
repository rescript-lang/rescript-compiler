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



val caml_sys_getenv : string -> string (* [@@dead "caml_sys_getenv"] *)

val caml_sys_time : unit -> float  (* [@@dead "caml_sys_time"] *)

val os_type : unit -> string  (* [@@dead "os_type"] *)

val caml_sys_random_seed : unit -> nativeint array (* [@@dead "caml_sys_random_seed"] *)

val caml_sys_system_command : string -> int (* [@@dead "caml_sys_system_command"] *)

val caml_sys_getcwd : unit -> string  (* [@@dead "caml_sys_getcwd"] *)

val caml_sys_get_argv : unit -> string * string array (* [@@dead "caml_sys_get_argv"] *)

val caml_sys_exit : int -> unit  (* [@@dead "caml_sys_exit"] *)

val caml_sys_is_directory : string -> bool  (* [@@dead "caml_sys_is_directory"] *)
val caml_sys_file_exists : string -> bool  (* [@@dead "caml_sys_file_exists"] *)