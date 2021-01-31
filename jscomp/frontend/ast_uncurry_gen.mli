(* Copyright (C) 2020- Authors of BuckleScript
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


(** 
    [function] can only take one argument, that is the reason we did not adopt it
    syntax:
    {[ fun [@bs] pat pat1-> body ]}
    [to_uncurry_fn (fun pat -> (fun pat1 -> ...  body))]

*)
val to_uncurry_fn :  
  Location.t -> 
  Bs_ast_mapper.mapper -> 
  Asttypes.arg_label ->   
  Parsetree.pattern ->
  Parsetree.expression ->
  Parsetree.expression_desc



(** syntax: 
    {[fun [@bs.this] obj pat pat1 -> body]}    
*)
val to_method_callback : 
  Location.t -> 
  Bs_ast_mapper.mapper ->
  Asttypes.arg_label ->  
  Parsetree.pattern ->
  Parsetree.expression ->
  Parsetree.expression_desc
