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



type build_generator = 
  { input : string list ;
    output : string list;
    command : string}

type  file_group = 
  { dir : string ; 
    (* currently relative path expected for ninja file generation *)
    sources : Bsb_build_cache.t ; 
    resources : string list ; 
    (* relative path *)
    public : public;
    dir_index : Bsb_dir_index.t; 
    generators : build_generator list;
  } 



type t = 
  { files :  file_group list ;
    (* flattened list of directories *)
    intervals :  Ext_file_pp.interval list ;
    globbed_dirs : string list ; 

  }





type cxt = {
  not_dev : bool ;
  dir_index : Bsb_dir_index.t ; 
  cwd : string ;
  root : string ;
  cut_generators : bool;
  traverse : bool
}


val parsing_simple_dir : 
  cxt -> 
  string -> 
  t

val parsing_source_dir_map :
  cxt ->
  Ext_json_types.t String_map.t -> 
  t

val parsing_source : 
  cxt -> 
  Ext_json_types.t ->     
  t 

val parsing_arr_sources :  
  cxt ->
  Ext_json_types.t array ->
  t

(** entry is to the 
    [sources] in the schema

    [parse_sources cxt json]
    given a root, return an object which is
    all relative paths, this function will do the IO
*)
val parse_sources : 
  cxt ->
  Ext_json_types.t  ->
  t 

