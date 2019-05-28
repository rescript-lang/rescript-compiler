(* Copyright (C) 2019- Authors of BuckleScript
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


let tag_is_zero (tag : J.expression) = 
  match tag.expression_desc with 
  | Number (Int {i = 0l; _}) -> true 
  | _ -> false;;

 let needBlockRuntimeInDebugMode 
  (tag : J.expression)
  (tag_info : J.tag_info) = 
  match tag_info with 
  | Blk_variant _ 
  | Blk_module _    
  | Blk_record _   
  | Blk_constructor _   -> true
#if OCAML_VERSION =~ ">4.03.0" then
  | Blk_record_inlined _ -> true  
#end  

  | Blk_tuple 
  | Blk_array   
#if OCAML_VERSION =~ ">4.03.0" then
  | Blk_record_ext _ -> false
#end  
  | Blk_extension_slot -> false 
  | Blk_na  ->  not (tag_is_zero tag )

let needBlockRuntimeInReleaseMode (tag : J.expression) (tag_info : J.tag_info) = 
  match  tag_info with 
  | Blk_variant _ 
  | Blk_module _
  | Blk_record _  
  | Blk_tuple 
  | Blk_array -> false   
#if OCAML_VERSION =~ ">4.03.0" then
  | Blk_record_inlined (_,_,1)  
#end  
  | Blk_constructor (_, 1)      
  | Blk_na -> not (tag_is_zero tag)
#if OCAML_VERSION =~ ">4.03.0" then
  | Blk_record_inlined _ 
#end  
  | Blk_constructor _   -> true  
#if OCAML_VERSION =~ ">4.03.0" then
  | Blk_record_ext _ 
#end  
  | Blk_extension_slot -> false 
    (* converted to [Pcreate_extension] in the beginning*)
 

(* Used to decide whether we should add [require('block')]*)
let needBlockRuntime tag info = 
  if !Js_config.debug then 
    needBlockRuntimeInDebugMode tag info
  else needBlockRuntimeInReleaseMode tag info  
