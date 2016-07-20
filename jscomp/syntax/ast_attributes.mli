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
type attr =  Parsetree.attribute
type t =  attr list 

type ('a,'b) st = 
  { get : 'a option ; 
    set : 'b option }

val process_method_attributes_rev : 
  t ->
  (bool * bool , [`Get | `No_get ]) st * t 

val process_attributes_rev : 
  t -> [ `Meth_callback | `Nothing | `Uncurry | `Method ] * t 

val process_class_type_decl_rev : 
  t -> [ `Nothing | `Has] * t 

val process_external : t -> bool 
val process_bs_type : t -> Parsetree.core_type option * t 
type derive_attr = {
  explict_nonrec : bool;
  bs_deriving : [`Has_deriving of Ast_payload.action list | `Nothing ]
}
val process_bs_name : t -> string option
val process_derive_type : 
  t -> derive_attr * t 


val bs_obj : Parsetree.core_type -> t 
val bs : attr 
val bs_this : attr
val bs_method : attr

val mk_bs_type : ?loc:Location.t -> Parsetree.core_type -> attr
