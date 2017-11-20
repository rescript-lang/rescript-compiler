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
 

let rec searchAux i (xs : (int * _) array) (k : int) =  
  let (a,b) = Array.unsafe_get xs i in 
  if a = k then b 
  else searchAux (succ i) xs k 

let searchForSureExists xs k =   
  searchAux 0 xs k 


let search (id : int) array =   
  searchAux 0 array id 

let rec revSearchAux 
    i len (xs : (int * string) array) (k : string) = 
  if i = len then None 
  else 
    let (idx,s) = Array.unsafe_get xs i  in 
    if s = k then 
      Some idx 
    else 
      revSearchAux (i + 1) len xs k 

let revSearch len array (x : string)  : int option =  
   (revSearchAux 0 len array x)

let toInt (i : int) (xs : int array) =   
  Array.unsafe_get xs i

let rec fromIntAux (enum : int) i len xs = 
  if i = len then None
  else 
    let k = Array.unsafe_get xs i in 
    if k = enum then Some i 
    else fromIntAux enum (i + 1) len xs 

let fromInt len (xs : int array) (enum : int )  : 'variant option =   
    fromIntAux enum 0 len xs 