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








module E = Js_exp_make

(* TODO: it would be even better, if the [tag_info] contains more information
   about immutablility
 *)
let make_block mutable_flag (tag_info : Lambda.tag_info) tag args  = 

  match mutable_flag, tag_info with
  | _, Blk_array -> Js_of_lam_array.make_array mutable_flag  Pgenarray args
  | _ , _ -> E.make_block tag tag_info args mutable_flag
  (* | _, (  Tuple | Variant _ ) -> (\** TODO: check with inline record *\) *)
  (*     E.arr Immutable *)
  (*       (E.small_int  ?comment:(Lam_compile_util.comment_of_tag_info tag_info) tag   *)
  (*        :: args) *)
  (* | _, _  ->  *)
  (*     E.arr mutable_flag *)
  (*       (E.int  ?comment:(Lam_compile_util.comment_of_tag_info tag_info) tag   *)
  (*        :: args) *)

let field field_info e i =
  match field_info with 
  | Lambda.Fld_na -> 
    E.index e i 
  | Lambda.Fld_record s 
  | Lambda.Fld_module s 
    -> E.index ~comment:s e i



let set_field field_info e i e0 =
  let comment = 
    match field_info with 
    | Lambda.Fld_set_na 
      -> None
    | Fld_record_set s -> Some (s)
  in (* see GPR#631*)
  E.assign_addr 
    ?comment e i 
   ~assigned_value:e0 





