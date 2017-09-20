
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

type case = bool 

(** Store a file called [.bsbuild] that can be communicated 
  between [bsb.exe] and [bsb_helper.exe]. 
  [bsb.exe] stores such data which would be retrieved by 
  [bsb_helper.exe]
*) 
type ml_kind =
  | Ml_source of string * bool  * bool
     (* No extension stored
      Ml_source(name,is_re)
      [is_re] default to false
      *)
  
  | Ml_empty
type mli_kind = 
  | Mli_source of string  * bool * bool
  | Mli_empty

type module_info = 
  {
    mli : mli_kind ; 
    ml : ml_kind ; 
  }

type t = module_info String_map.t 

(** store  the meta data indexed by {!Bsb_dir_index}
  {[
    0 --> lib group
    1 --> dev 1 group
    .
    
  ]}
*)

val dir_of_module_info : module_info -> string


val filename_sans_suffix_of_module_info : module_info -> string 
type ts = t array 

val write_build_cache : dir:string -> ts -> unit

val read_build_cache : dir:string -> ts







(** 
  Currently it is okay to have duplicated module, 
  In the future, we may emit a warning 
*)
val map_update : 
  dir:string -> t ->  string -> t

val sanity_check : t -> unit   