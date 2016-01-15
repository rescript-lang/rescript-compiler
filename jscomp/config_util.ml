(* OCamlScript compiler
 * Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(* Author: Hongbo Zhang  *)




(* ATTENTION: lazy to wait [Config.load_path] populated *)
let find file =  Misc.find_in_path_uncap !Config.load_path file 

module Cmj_data_set_map 
  =  Ext_map.Make(String)



(* strategy:
   If not installed, use the distributed [cmj] files, 
   make sure that the distributed files are platform independent
*)
let find_cmj file = 
  begin match find file with
  | f
    -> 
    Js_cmj_format.from_file f             
  | exception Not_found -> 
    (* TODO: add an logger module *)
    begin match 
        String_map.find (String.uncapitalize (Filename.basename file)) 
          Js_cmj_datasets.cmj_data_sets with 
    | v
      -> Lazy.force v 
    | exception Not_found 
      ->     
      Ext_log.warn __LOC__ "@[%s not found @]@." file ;
      Js_cmj_format.dummy  (); (* FIXME *)
    end
  end
