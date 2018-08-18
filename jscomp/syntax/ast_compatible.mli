(* Copyright (C) 2018 Authors of BuckleScript
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

#if OCAML_VERSION =~ ">4.3.0" then 
type arg_label = Asttypes.arg_label
type label = arg_label
external convert: arg_label -> label = "%identity"
#else
type arg_label = string 
type label = 
  | Nolabel
  | Labelled of string
  | Optional of string
val convert: arg_label -> label
#end



val no_label: arg_label

type loc = Location.t 
type attrs = Parsetree.attribute list 
open Parsetree


val const_exp_string:
  ?loc:Location.t -> 
  ?attrs:attrs ->    
  ?delimiter:string -> 
  string -> 
  expression

val const_exp_int:
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  int -> 
  expression 

val const_exp_int_list_as_array:  
  int list -> 
  expression 

val const_exp_string_list_as_array:  
  string list -> 
  expression 

  
val apply_simple:
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  expression ->   
  expression list -> 
  expression 

val app1:
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  expression ->   
  expression -> 
  expression 

val app2:
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  expression ->   
  expression -> 
  expression -> 
  expression 

val app3:
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  expression ->   
  expression -> 
  expression -> 
  expression ->   
  expression 

(** Note this function would slightly 
  change its semantics depending on compiler versions
  for newer version: it means always label
  for older version: it could be optional (which we should avoid)
*)  
val apply_labels:  
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  expression ->   
  (string * expression) list -> 
  (* [(label,e)] [label] is strictly interpreted as label *)
  expression 

val fun_ :  
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  pattern -> 
  expression -> 
  expression

val is_arg_label_simple : 
  arg_label -> bool   

val arrow :
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  core_type -> 
  core_type ->
  core_type

val label_arrow :
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  string -> 
  core_type -> 
  core_type ->
  core_type

val opt_arrow:
  ?loc:Location.t -> 
  ?attrs:attrs -> 
  string -> 
  core_type -> 
  core_type ->
  core_type

val object_: 
  ?loc:loc -> 
  ?attrs:attrs ->
  (string * attributes * core_type) list -> 
  (*FIXME shall we use [string loc] instead?*)
  Asttypes.closed_flag ->
  core_type  

val rec_type_str:  
  ?loc:loc -> 
  type_declaration list -> 
  structure_item

val nonrec_type_str:  
  ?loc:loc -> 
  type_declaration list -> 
  structure_item

val rec_type_str:  
  ?loc:loc -> 
  type_declaration list -> 
  structure_item

val nonrec_type_sig:  
  ?loc:loc -> 
  type_declaration list -> 
  signature_item 

val rec_type_sig:  
  ?loc:loc -> 
  type_declaration list -> 
  signature_item

val mk_fn_type:  
  (arg_label * core_type * attributes * loc) list -> 
  core_type -> 
  core_type

type object_field = 
#if OCAML_VERSION =~ ">4.03.0" then 
  Parsetree.object_field 
val object_field : label Asttypes.loc ->  attributes -> core_type -> object_field
#else   
  string * attributes * core_type
val object_field : string ->  attributes -> core_type -> object_field  
#end  

  