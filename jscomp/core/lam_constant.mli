(* Copyright (C) 2018- Authors of BuckleScript
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

type t =
  | Const_js_null
  | Const_js_undefined
  | Const_js_true
  | Const_js_false
  | Const_int of int
  | Const_char of char
  | Const_string of string  (* use record later *)
  | Const_unicode of string
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint
  | Const_pointer of int * Lam_pointer_info.t
  | Const_block of int * Lam_tag_info.t * t list
  | Const_float_array of string list
  | Const_immstring of string
  | Const_some of t 
    (* eventually we can remove it, since we know
      [constant] is [undefined] or not 
    *) 
val eq_approx : t -> t -> bool
val convert_constant : 
  Lambda.structured_constant -> 
  t    