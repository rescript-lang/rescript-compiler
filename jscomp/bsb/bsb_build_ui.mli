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

type public = 
  | Export_all 
  | Export_set of String_set.t 
  | Export_none
    
type dir_index = int 

type build_generator = 
  { input : string list ;
    output : string list;
    command : string}

type  file_group = 
  { dir : string ; (* currently relative path expected for ninja file generation *)
    sources : Binary_cache.file_group_rouces ; 
    resources : string list ; (* relative path *)
    public : public;
    dir_index : dir_index; 
    generators : build_generator list;
  } 



type t = 
  { files :  file_group list ; 
    intervals :  Ext_file_pp.interval list ;
    globbed_dirs : string list ; 

  }

val lib_dir_index : dir_index 

val get_current_number_of_dev_groups : unit -> int 

type parsing_cxt = {
  no_dev : bool ;
  dir_index : dir_index ; 
  cwd : string ;
  root : string ;
  cut_generators : bool
}


(** entry is to the 
    [sources] in the schema

    [parsing_sources cxt json]
    given a root, return an object which is
    all relative paths, this function will do the IO
*)
val parsing_sources : 
  parsing_cxt ->
  Ext_json_types.t  ->
  t 
  
