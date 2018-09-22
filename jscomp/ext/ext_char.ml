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





(** {!Char.escaped} is locale sensitive in 4.02.3, fixed in the trunk,
    backport it here
 *)
#if OCAML_VERSION =~ ">4.03.0" then 
let escaped = Char.escaped
#else
module Unsafe = struct 
    external bytes_unsafe_set : string -> int -> char -> unit
                           = "%string_unsafe_set"
    external string_create: int -> string = "caml_create_string"
    external unsafe_chr: int -> char = "%identity"
end 
let escaped ch = 
  let open Unsafe in 
  match ch with 
  | '\'' -> "\\'"
  | '\\' -> "\\\\"
  | '\n' -> "\\n"
  | '\t' -> "\\t"
  | '\r' -> "\\r"
  | '\b' -> "\\b"
  | ' ' .. '~' as c ->
      let s = string_create 1 in
      bytes_unsafe_set s 0 c;
      s
  | c ->
      let n = Char.code c in
      let s = string_create 4 in
      bytes_unsafe_set s 0 '\\';
      bytes_unsafe_set s 1 (unsafe_chr (48 + n / 100));
      bytes_unsafe_set s 2 (unsafe_chr (48 + (n / 10) mod 10));
      bytes_unsafe_set s 3 (unsafe_chr (48 + n mod 10));
      s
#end

let valid_hex x = 
    match x with 
    | '0' .. '9'
    | 'a' .. 'f'
    | 'A' .. 'F' -> true
    | _ -> false 



let is_lower_case c =
  (c >= 'a' && c <= 'z')
  || (c >= '\224' && c <= '\246')
  || (c >= '\248' && c <= '\254')    
let uppercase_ascii =
#if OCAML_VERSION =~ ">4.3.0" then
    Char.uppercase_ascii
#else
    Char.uppercase
#end      

let lowercase_ascii = 
#if OCAML_VERSION =~ ">4.3.0" then
    Char.lowercase_ascii
#else
    Char.lowercase
#end      
