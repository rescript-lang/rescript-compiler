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




type _ kind =
  | Ml_kind : Parsetree.structure kind
  | Mli_kind : Parsetree.signature kind

type module_name = private string
  
module String_set = Depend.StringSet

val read_parse_and_extract : 'a kind -> 'a -> String_set.t

type ('a,'b) ast_info =
  | Ml of
      string * (* sourcefile *)
      'a *
      string (* opref *)      
  | Mli of string * (* sourcefile *)
           'b *
           string (* opref *)
  | Ml_mli of
      string * (* sourcefile *)
      'a *
      string  * (* opref1 *)
      string * (* sourcefile *)      
      'b *
      string (* opref2*)

type ('a,'b) t =
  { module_name : string ; ast_info : ('a,'b) ast_info }

val sort_files_by_dependencies :
  domain:String_set.t -> String_set.t String_map.t -> string Queue.t
(** 
   {[ let stack,mapping = prepare ast_table ]}

   {[ 
     [ast_table] : (string, ast) Hashtbl.t 
   ]}

   key  is the filename,
   [stack] is the reverse order 
   for mapping, the key is the module and value is filename
*)


val sort :
  (Parsetree.structure, Parsetree.signature) t  String_map.t -> string Queue.t  





