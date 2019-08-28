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

let get_double_feild (field_info : Lam_compat.field_dbg_info) e i = 
  match field_info with 
  | Fld_na -> 
    E.array_index_by_int e i 
#if OCAML_VERSION =~ ">4.03.0" then 
  | Fld_record_inline s
  | Fld_record_extension s
#end
  | Fld_record s 
  | Fld_module s 
    -> E.dot e s

let set_double_field (field_info : Lam_compat.set_field_dbg_info) e  i e0 = 
  let v = 
    match field_info with 
    | Fld_set_na 
      -> 
      E.array_index_by_int e i 
#if OCAML_VERSION =~ ">4.03.0" then 
    | Fld_record_inline_set s
    | Fld_record_extension_set s 
#end      
    | Fld_record_set s -> 
      E.dot e s
  in 
  E.assign v  e0

