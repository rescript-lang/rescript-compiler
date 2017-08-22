
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


 (* Note the build system should check the validity of filenames
    espeically, it should not contain '-'
 *)
 let package_sep_char = '-'
 let package_sep = "-"

 let make ~ns cunit  = 
    cunit ^ package_sep ^ ns
    

let rec rindex_rec s i c =
  if i < 0 then i else
  if String.unsafe_get s i = c then i else rindex_rec s (i - 1) c;;
    
let remove_package_suffix name =
    let i = rindex_rec name (String.length name - 1) package_sep_char in 
    if i < 0 then name 
    else String.sub name 0 i 


let js_name_of_basename s = 
  remove_package_suffix (String.uncapitalize s) ^ Literals.suffix_js
  
  
let namespace_of_package_name (s : string) : string = 
  let len = String.length s in 
  let buf = Buffer.create len in 
  let add capital ch = 
    Buffer.add_char buf 
      (if capital then 
        (Char.uppercase ch)
      else ch) in    
  let rec aux capital off len =     
      if off >= len then ()
      else 
        let ch = String.unsafe_get s off in
        match ch with 
        | 'a' .. 'z' 
        | 'A' .. 'Z' 
        | '0' .. '9'
          ->
          add capital ch ; 
          aux false (off + 1) len 
        | '-' -> 
          aux true (off + 1) len 
        | _ -> aux capital (off+1) len
         in 
   aux true 0 len ;
   Buffer.contents buf 
