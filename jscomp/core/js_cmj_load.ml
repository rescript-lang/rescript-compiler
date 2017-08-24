(* Copyright (C) Authors of BuckleScript
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

(* strategy:
   If not installed, use the distributed [cmj] files, 
   make sure that the distributed files are platform independent
*)
let find_cmj file : string * Js_cmj_format.t = 
  match Config_util.find_opt file with
  | Some f
    -> 
    f, Js_cmj_format.from_file f             
  | None -> 
    (* ONLY read the stored cmj data in browser environment *)
#if BS_COMPILER_IN_BROWSER then  
        "BROWSER", (   
        let target = String.uncapitalize (Filename.basename file) in
        match String_map.find_exn  target Js_cmj_datasets.data_sets with 
        | v
          -> 
          begin match Lazy.force v with
            | exception _ 
              -> 
              Ext_log.warn __LOC__ 
                "@[%s corrupted in database, when looking %s while compiling %s please update @]"           file target (Js_config.get_current_file ())  ;
              Js_cmj_format.no_pure_dummy; (* FIXME *)
            | v ->  v 
            (* see {!Js_packages_info.string_of_module_id} *)
          end
        | exception Not_found 
          ->     
          Ext_log.warn __LOC__ "@[%s not found @]" file ;
          Js_cmj_format.no_pure_dummy )
#else
        Bs_exception.error (Cmj_not_found file)
#end        

