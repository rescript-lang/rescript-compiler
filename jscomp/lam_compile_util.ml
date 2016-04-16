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





let jsop_of_comp (cmp : Lambda.comparison) : Js_op.binop = 
  match cmp with 
  | Ceq -> EqEqEq (* comparison*)
  | Cneq -> NotEqEq
  | Clt -> Lt 
  | Cgt  -> Gt 
  | Cle -> Le 
  | Cge  -> Ge

let comment_of_tag_info  (x : Lambda.tag_info) = 
  match x with 
  | Blk_constructor (n, _) -> Some n 
  | Blk_tuple -> Some "tuple"
  | Blk_variant x -> Some ("`" ^  x)
  | Blk_record _ -> Some "record"

  | Blk_array -> Some "array"
  | Blk_module _ ->  
     (* Turn it on next time to save some noise diff*)
    Some "module"
  | Blk_na -> None 
let comment_of_pointer_info (x :  Lambda.pointer_info)= 
  match x with 
  | Pt_constructor x -> Some x 
  | Pt_variant x -> Some x 
  | Lambda.Pt_module_alias -> None (* FIXME *)
  | Pt_na -> None
