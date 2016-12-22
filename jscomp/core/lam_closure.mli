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

(** [is_closed_by map lam]
    return [true] if all unbound variables
    belongs to the given [map] *)
val is_closed_by : Ident_set.t -> Lam.t -> bool

val is_closed : Lam.t -> bool





type stats = 
  { 
    top : bool ; 
    (* all appearances are in the top,  substitution is fine 
       whether it is pure or not
       {[
         (fun x y          
           ->  x + y + (f x )) (32) (console.log('hi'), 33)
       ]}       
       since in ocaml, the application order is intentionally undefined, 
       note if [times] is not one, this field does not make sense       
    *)    
    times : int ; 
  }

val is_closed_with_map : 
  Ident_set.t ->
  Ident.t list -> Lam.t -> bool * stats Ident_map.t

(* val param_map_of_list : Ident.t list -> stats Ident_map.t *)

val free_variables : Ident_set.t -> stats Ident_map.t -> Lam.t -> stats Ident_map.t

