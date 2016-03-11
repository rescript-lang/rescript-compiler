(* BuckleScript compiler
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



module E = Js_exp_make 

let empty_record_info = Lambda.Record [||] (* careful to share*)
(* TODO: add label to the comment *)
let make  mutable_flag (args : (string * J.expression) list) =
  E.make_block ~comment:"record" 
    E.zero_int_literal empty_record_info (List.map snd args) mutable_flag



let field field_info  e i =
  match field_info with 
  | Lambda.Fld_na -> 
    E.index e i 
  | Lambda.Fld_record s 
  | Lambda.Fld_module s 
    -> E.index ~comment:s e i

(**
   used in [Pduprecord]
   this is due to we encode record as an array, it is going to change
   if we have another encoding       
*)    
let copy  = E.array_copy

