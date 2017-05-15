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










(** *)

(* TODO: make it resizable instead 
    option1: set new index directly
    option2: create a new array    
    Note [Array.make] is recognized 
    as a special primitive for better 
    side effect analysis
*)
let caml_methods_cache = 
    Array.make 1000 0 

external get_methods : CamlinternalOO.obj -> CamlinternalOO.closure array =
  "%field0"


let caml_get_public_method 
    (obj : CamlinternalOO.obj) 
    (tag : int) (cacheid  : int) : CamlinternalOO.closure =
  let meths = get_methods obj in
  let offs =  caml_methods_cache.(cacheid) in
  if (Bs_obj.magic meths.(offs) : int) = tag then meths.(offs - 1)
  else
    (** TODO: binary search *)    
    let rec aux (i : int) : int =     
      if i < 3 then assert false       
      else if (Bs_obj.magic meths.(i) : int) = tag then
        begin        
          caml_methods_cache.(cacheid) <- i;         
          i
        end
      else         
        aux (i - 2)
    in
    meths.(aux (Bs_obj.magic ((Bs_obj.magic meths.(0) : int) * 2 + 1) : int) - 1)     

