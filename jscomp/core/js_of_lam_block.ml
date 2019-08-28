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
let make_block mutable_flag (tag_info : Lam_tag_info.t) tag args  = 

  match tag_info with
  | Blk_array -> Js_of_lam_array.make_array mutable_flag  args
  |  _ -> E.make_block tag tag_info args mutable_flag
  (* | _, (  Tuple | Variant _ ) -> (\** TODO: check with inline record *\) *)
  (*     E.arr Immutable *)
  (*       (E.small_int  ?comment:(Lam_compile_util.comment_of_tag_info tag_info) tag   *)
  (*        :: args) *)
  (* | _, _  ->  *)
  (*     E.arr mutable_flag *)
  (*       (E.int  ?comment:(Lam_compile_util.comment_of_tag_info tag_info) tag   *)
  (*        :: args) *)

let field (field_info : Lam_compat.field_dbg_info) e i =
  match field_info with 
  | Fld_na ->
    (* let comment = "NA" in *)
    E.array_index_by_int (* ~comment *) e i 
#if OCAML_VERSION =~ ">4.03.0" then 
  | Fld_record_inline comment
  | Fld_record_extension comment
#end
  | Fld_record comment ->
    E.dot e comment
  | Fld_module name
    -> E.module_access e name i

  let field_by_exp e i = 
  E.array_index e i 


let set_field (field_info : Lam_compat.set_field_dbg_info) e i e0 =
  let comment = 
    match field_info with 
    | Fld_set_na 
      -> None
#if OCAML_VERSION =~ ">4.03.0" then
    | Fld_record_inline_set s
    | Fld_record_extension_set s
#end    
    | Fld_record_set s -> Some (s)
  in (* see GPR#631*)
  match comment with
  | None -> E.assign_by_int ?comment e i e0 
  | Some s -> E.assign (E.dot e s) e0

let set_field_by_exp self index value = 
  E.assign_by_exp self index value




