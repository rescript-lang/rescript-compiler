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

(** An pattern match on {!caml_set_oo_id args}
    Note that in the trunk, it is immutable by default now 
 *)
module E = Js_exp_make 

let match_exception_def (args : J.expression list) = 
  match args with   
  | [{ expression_desc  = 
               Caml_block (
                 [ exception_str; 
                   {expression_desc = J.Number (Int { i = 0l; _}); _}
                 ],
                 mutable_flag, 
                 {expression_desc = J.Number (Int {i = object_tag; _}); _}, _ );
              _} ] -> 
    if object_tag = 248l (* Obj.object_tag *) then
      Some ( exception_str, mutable_flag)    
    else
      None
  | _ -> None

(* Sync up with [caml_set_oo_id] *)
let make_exception id  exception_str mutable_flag : J.expression = 
  { expression_desc = 
      Caml_block (
        [exception_str ; 
         id],  
        mutable_flag,
        (* TODO: combined with `_001` optimization *)
        E.obj_int_tag_literal (* (Obj.object_tag) *),
        NA                    
      );
    comment = None
  }
