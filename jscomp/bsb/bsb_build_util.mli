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

(** 
Use:
{[
flag_concat "-ppx" [ppxs]
]}
*)
val flag_concat : string -> string list -> string


(**
    It does several conversion:
    First, it will convert unix path to windows backward on windows platform.
    Then if it is absolute path, it will do thing
    Else if it is relative path, it will be rebased on project's root directory 
*)
val convert_and_resolve_path : string -> string

(**
   The difference between [convert_path] is that if the file is [ocamlc.opt] 
   it will not do any conversion to it (maybe environment variable will help it get picked up)
*)
(* val convert_and_resolve_file : string -> string *)

val mkp : string -> unit


(* The path of [bsc] and [bsdep] is normalized so that the invokation of [./jscomp/bin/bsb.exe] 
   and [bsb.exe] (combined with a dirty bsconfig.json) will not trigger unnecessary rebuild.
   
   The location of [bsc] and [bsdep] is configured by the combination of [Sys.executable_name] 
   and [cwd].
   
   In theory, we should also check the integrity of [bsb.exe], if it is changed, the rebuild 
   should be regen, but that is too much in practice, not only you need check the integrity of 
   path of [bsb.exe] but also the timestamp, to make it 100% correct, also the integrity of 
   [bsdep.exe] [bsc.exe] etc.
*)
val get_bsc_bsdep : string -> string * string
val get_bsc_dir : string -> string                               


val get_list_string_acc : 
    Ext_json_types.t array -> 
    string list -> 
    string list

val get_list_string : 
    Ext_json_types.t array -> 
    string list

val string_of_bsb_dev_include : int -> string 

val resolve_bsb_magic_file : cwd:string -> desc:string -> string -> string

type package_context = {
  cwd : string ; 
  top : bool ; 
}

val walk_all_deps : string -> (package_context -> unit) -> unit

(* Given the root project directory, will return an absolute path to either a globally installed 
   ocaml or Bucklescript's vendored ocaml if there is no global ocaml. *)
val get_ocaml_dir: string -> string
