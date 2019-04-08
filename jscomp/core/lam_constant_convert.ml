(* Copyright (C) 2018- Authors of BuckleScript
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

let rec convert_constant ( const : Lambda.structured_constant) : Lam_constant.t =
  match const with
  | Const_base (Const_int i) -> (Const_int i)
  | Const_base (Const_char i) -> (Const_char i)
  | Const_base (Const_string(i,opt)) ->
      (match opt with
      | Some opt when
          Ast_utf8_string_interp.is_unicode_string opt  ->
        Const_unicode i
      | _ ->
        Const_string i)
    
  | Const_base (Const_float i) -> (Const_float i)
  | Const_base (Const_int32 i) -> (Const_int32 i)
  | Const_base (Const_int64 i) -> (Const_int64 i)
  | Const_base (Const_nativeint i) -> (Const_nativeint i)
  | Const_pointer(i,p) ->
    begin match p with 
    | Pt_constructor p -> Const_pointer(i, Pt_constructor p)
    | Pt_variant p -> Const_pointer(i,Pt_variant p)
    | Pt_module_alias -> Const_pointer(i, Pt_module_alias)
    | Pt_builtin_boolean -> if i = 0 then Const_js_false else Const_js_true
    | Pt_shape_none ->
       Lam_constant.lam_none
    | Pt_na ->  Const_pointer(i, Pt_na)      
     end 
  | Const_float_array (s) -> Const_float_array(s)
  | Const_immstring s -> Const_immstring s
  | Const_block (i,t,xs) ->
    begin match t with 
    | Blk_some_not_nested -> 
      Const_some (convert_constant (Ext_list.singleton_exn xs))
    | Blk_some -> 
      Const_some (convert_constant (Ext_list.singleton_exn xs))        
    | Blk_constructor(a,b) ->   
      let t : Lam_tag_info.t = Blk_constructor(a,b) in 
      Const_block (i,t, Ext_list.map xs convert_constant )
    | Blk_tuple ->   
      let t : Lam_tag_info.t = Blk_tuple in 
      Const_block (i,t, Ext_list.map xs convert_constant )
    | Blk_array -> 
      let t : Lam_tag_info.t = Blk_array in 
      Const_block (i,t, Ext_list.map xs convert_constant )
    | Blk_variant s -> 
      let t : Lam_tag_info.t = Blk_variant s in 
      Const_block (i,t, Ext_list.map xs convert_constant )      
    | Blk_record s -> 
      let t : Lam_tag_info.t = Blk_record s in 
      Const_block (i,t, Ext_list.map xs convert_constant )
    | Blk_module s -> 
      let t : Lam_tag_info.t = Blk_module s in 
      Const_block (i,t, Ext_list.map xs convert_constant )    
    | Blk_extension_slot -> 
      let t : Lam_tag_info.t = Blk_extension_slot in 
      Const_block (i,t, Ext_list.map xs convert_constant )      
    | Blk_na -> 
      let t : Lam_tag_info.t = Blk_na in 
      Const_block (i,t, Ext_list.map xs convert_constant )      
#if OCAML_VERSION =~ ">4.03.0" then
    | Blk_record_inlined (s,ctor,ix)  -> 
      let t : Lam_tag_info.t = Blk_record_inlined (s, ctor,ix) in 
      Const_block (i,t, Ext_list.map xs convert_constant )      
    | Blk_record_ext s -> 
      let t : Lam_tag_info.t = Blk_record_ext s in 
      Const_block(i,t, Ext_list.map xs convert_constant)
#end        
    end
    

    

