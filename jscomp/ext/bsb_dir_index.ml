(* Copyright (C) 2017 Authors of BuckleScript
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

type t = int

external of_int : int -> t = "%identity"
(** 0 : lib 1 : dev 1 2 : dev 2 *)

let lib_dir_index = 0
let is_lib_dir x = x = lib_dir_index
let dir_index = ref 0
let get_dev_index () = incr dir_index ; !dir_index
let get_current_number_of_dev_groups () = !dir_index

(** bsb generate pre-defined variables [bsc_group_i_includes] for each rule,
    there is variable [bsc_extra_excludes] [g_dev_incls] are for app test etc
    it will be like {[
    g_dev_incls = ${bsc_group_1_includes}
                    ]} where [bsc_group_1_includes] will be pre-calcuated *)
let bsc_group_1_includes = "bsc_group_1_includes"

let bsc_group_2_includes = "bsc_group_2_includes"
let bsc_group_3_includes = "bsc_group_3_includes"
let bsc_group_4_includes = "bsc_group_4_includes"

let string_of_bsb_dev_include i =
  match i with
  | 1 -> bsc_group_1_includes
  | 2 -> bsc_group_2_includes
  | 3 -> bsc_group_3_includes
  | 4 -> bsc_group_4_includes
  | _ -> "bsc_group_" ^ string_of_int i ^ "_includes"

let reset () = dir_index := 0
