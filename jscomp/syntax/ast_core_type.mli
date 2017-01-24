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

type t = Parsetree.core_type 


val extract_option_type_exn : t -> t 
val lift_option_type : t -> t 
val is_any : t -> bool 
val replace_result : t -> t -> t

val is_unit : t -> bool 
val is_array : t -> bool 
type arg_label =
  | Label of string 
(*| Label_int_lit of string * int 
  | Label_string_lit of string * string *)
  | Optional of string 
  | Empty
type arg_type = 
  | NullString of (int * string) list 
  | NonNullString of (int * string) list 
  | Int of (int * int ) list 
  | Arg_int_lit of int 
  | Arg_string_lit of string 
  | Array 
  | Extern_unit
  | Nothing
  | Ignore

(** for 
       [x:t] -> "x"
       [?x:t] -> "?x"
*)
val label_name : string -> arg_label





(** return a function type 
    [from_labels ~loc tyvars labels]
    example output:
    {[x:'a0 -> y:'a1 -> < x :'a0 ;y :'a1  > Js.t]}
*)
val from_labels :
  loc:Location.t -> int ->  string Asttypes.loc list -> t

val make_obj :
  loc:Location.t ->
  (string * Parsetree.attributes * t) list ->
  t

val is_optional_label : string -> bool 