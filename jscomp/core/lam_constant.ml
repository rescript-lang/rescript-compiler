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
  | Const_string of string (* use record later *)
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

(* eventually we can remove it, since we know [constant] is [undefined] or not *)

let rec eq_approx (x : t) (y : t) =
  match x with
  | Const_js_null -> y = Const_js_null
  | Const_js_undefined -> y = Const_js_undefined
  | Const_js_true -> y = Const_js_true
  | Const_js_false -> y = Const_js_false
  | Const_int ix -> ( match y with Const_int iy -> ix = iy | _ -> false )
  | Const_char ix -> ( match y with Const_char iy -> ix = iy | _ -> false )
  | Const_string ix -> (
    match y with Const_string iy -> ix = iy | _ -> false )
  | Const_unicode ix -> (
    match y with Const_unicode iy -> ix = iy | _ -> false )
  | Const_float ix -> ( match y with Const_float iy -> ix = iy | _ -> false )
  | Const_int32 ix -> ( match y with Const_int32 iy -> ix = iy | _ -> false )
  | Const_int64 ix -> ( match y with Const_int64 iy -> ix = iy | _ -> false )
  | Const_nativeint ix -> (
    match y with Const_nativeint iy -> ix = iy | _ -> false )
  | Const_pointer (ix, _) -> (
    match y with Const_pointer (iy, _) -> ix = iy | _ -> false )
  | Const_block (ix, _, ixs) -> (
    match y with
    | Const_block (iy, _, iys) ->
        ix = iy && Ext_list.for_all2_no_exn ixs iys eq_approx
    | _ -> false )
  | Const_float_array ixs -> (
    match y with
    | Const_float_array iys ->
        Ext_list.for_all2_no_exn ixs iys Ext_string.equal
    | _ -> false )
  | Const_immstring ix -> (
    match y with Const_immstring iy -> ix = iy | _ -> false )
  | Const_some ix -> (
    match y with Const_some iy -> eq_approx ix iy | _ -> false )

let lam_none : t = Const_js_undefined
