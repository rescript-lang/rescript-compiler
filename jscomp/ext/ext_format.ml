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

open Format

type t = formatter

let string = pp_print_string
let break fmt = pp_print_break fmt 0 0
let break1 fmt = pp_print_break fmt 0 1
let space fmt = pp_print_break fmt 1 0

let vgroup fmt indent u =
  pp_open_vbox fmt indent ;
  let v = u () in
  pp_close_box fmt () ; v

let group fmt indent u =
  pp_open_hovbox fmt indent ;
  let v = u () in
  pp_close_box fmt () ; v

let paren fmt u =
  string fmt "(" ;
  let v = u () in
  string fmt ")" ; v

let brace fmt u =
  string fmt "{" ;
  (* break1 fmt ; *)
  let v = u () in
  string fmt "}" ; v

let bracket fmt u =
  string fmt "[" ;
  let v = u () in
  string fmt "]" ; v

let paren_group st n action = group st n (fun _ -> paren st action)
let brace_group st n action = group st n (fun _ -> brace st action)

let brace_vgroup st n action =
  vgroup st n (fun _ ->
      string st "{" ;
      pp_print_break st 0 2 ;
      let v = vgroup st 0 action in
      pp_print_break st 0 0 ; string st "}" ; v)

let bracket_group st n action = group st n (fun _ -> bracket st action)
let newline fmt = pp_print_newline fmt ()
let to_out_channel = formatter_of_out_channel

(* let non_breaking_space fmt = string fmt " " *)
(* let set_needed_space_function _ _ = () *)
let flush = pp_print_flush
let list = pp_print_list

let rec pp_print_queue ?(pp_sep = pp_print_cut) pp_v ppf q =
  Queue.iter (fun q -> pp_v ppf q ; pp_sep ppf ()) q
