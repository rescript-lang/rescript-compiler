(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)

(** A module which provides some basic analysis over lambda expression *)

(** No side effect, but it might depend on data store *)
val no_side_effects : Lambda.lambda -> bool 

val size : Lambda.lambda -> int

val eq_lambda : Lambda.lambda -> Lambda.lambda -> bool 
(** a conservative version of comparing two lambdas, mostly 
    for looking for similar cases in switch
 *)

(** [is_closed_by map lam]
    return [true] if all unbound variables
    belongs to the given [map] *)
val is_closed_by : (* Lambda. *) Ident_set.t -> Lambda.lambda -> bool

val is_closed : Lambda.lambda -> bool





type stats = 
  { 
    mutable top : bool ; 
    (* all appearances are in the top,  substitution is fine 
       whether it is pure or not
       {[
         (fun x y          
           ->  x + y + (f x )) (32) (console.log('hi'), 33)
       ]}       
       since in ocaml, the application order is intentionally undefined, 
       note if [times] is not one, this field does not make sense       
    *)    
    mutable times : int ; 
  }

val is_closed_with_map : 
  Ident_set.t ->
  Ident.t list -> Lambda.lambda -> bool * stats Ident_map.t

val param_map_of_list : Ident.t list -> stats Ident_map.t

val free_variables : Ident_set.t -> stats Ident_map.t -> Lambda.lambda -> stats Ident_map.t

val small_inline_size : int 
val exit_inline_size : int 


val safe_to_inline : Lambda.lambda -> bool
