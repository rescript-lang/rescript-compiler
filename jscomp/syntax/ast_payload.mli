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



(** A utility module used when destructuring parsetree attributes, used for 
    compiling FFI attributes and built-in ppx  *)

type t = Parsetree.payload
type lid = string Asttypes.loc
type label_expr = lid  * Parsetree.expression
type action = 
   lid * Parsetree.expression option

val is_single_string : t -> (string * string option) option
val is_single_int : t -> int option 

type rtn = Not_String_Lteral | JS_Regex_Check_Failed | Correct of Parsetree.expression
val as_string_exp : check_js_regex: bool -> t -> rtn
val as_core_type : Location.t -> t -> Parsetree.core_type    
val as_empty_structure :  t -> bool 
val as_ident : t -> Longident.t Asttypes.loc option
val raw_string_payload : Location.t -> string -> t 
val assert_strings :
  Location.t -> t -> string list  

(** if only [abstract] happens  [true]
    if [abstract] does not appear [false]
    if [abstract] happens with other, raise exception
*)  
val isAbstract : action list -> bool   
(** as a record or empty 
    it will accept 

    {[ [@@@bs.config ]]}
    or 
    {[ [@@@bs.config no_export ] ]}
    or 
    {[ [@@@bs.config { property  .. } ]]}    
    Note that we only 
    {[
      { flat_property}
    ]}
    below  is not allowed 
    {[
      {M.flat_property}
    ]}
*)

val ident_or_record_as_config : 
  Location.t ->
  t -> action list 

val assert_bool_lit : Parsetree.expression -> bool

val empty : t 

val table_dispatch : 
  (Parsetree.expression option  -> 'a) String_map.t -> action -> 'a
