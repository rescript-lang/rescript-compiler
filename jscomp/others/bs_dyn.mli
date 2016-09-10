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

(** An experimentation of runtime types in OCaml,
    unstable API, only for internal use
*)


type 'a im_array = 'a array

type variant_shape = private
  { constructors : string im_array ;
    arities : int im_array
  }

type record_shape =   string im_array


type value = private
  | Int32 of  int32
  | Int64 of int64
  | Int of int 
  | Nativeint of nativeint 
  | Bool of bool 
  | Float of float 
  | Char of char 
  | String of string

  | OptionNone
  | OptionSome of value 

  | Tuple of value im_array
  | Array of value im_array
  | List of value im_array
  | Record of record_shape * value im_array
  | Variant of variant_shape * int * value im_array

type 'a to_value = 'a -> value [@bs]
val int32_to_value : int32 to_value
val int64_to_value : int64 to_value
val int_to_value : int to_value
val nativeint_to_value : nativeint to_value 
val bool_to_value : bool to_value
val float_to_value : float to_value
val char_to_value : char to_value
val string_to_value : string to_value

(** Make sure [value_of*] below are  always fully applied *)
val array_to_value : 'a to_value -> 'a array to_value
val list_to_value : 'a to_value  -> 'a list to_value
val option_to_value : 'a to_value -> 'a option to_value

val record_to_value : record_shape  -> value im_array -> value
val variant_to_value : variant_shape -> int -> value im_array -> value



val tuple_2_to_value : 
  'a to_value ->
  'b to_value ->
  ('a * 'b ) to_value

val tuple_3_to_value : 
  'a to_value ->
  'b to_value ->
  'c to_value ->
  ('a * 'b * 'c) to_value 

val tuple_4_to_value : 
  'a to_value ->
  'b to_value -> 
  'c to_value ->
  'd to_value -> 
  ('a * 'b * 'c * 'd) to_value

val tuple_5_to_value : 
  'a0 to_value -> 
  'a1 to_value -> 
  'a2 to_value -> 
  'a3 to_value -> 
  'a4 to_value -> 
  ('a0 * 'a1 * 'a2 * 'a3 * 'a4 ) to_value

val tuple_6_to_value : 
  'a0 to_value -> 
  'a1 to_value -> 
  'a2 to_value -> 
  'a3 to_value -> 
  'a4 to_value -> 
  'a5 to_value -> 
  ('a0 * 'a1 * 'a2 * 'a3 * 'a4 * 'a5) to_value


val shape_of_variant : 
  string im_array -> int array -> variant_shape
val shape_of_record : 
  string im_array -> record_shape
