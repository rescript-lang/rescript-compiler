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






type arity = 
  | Single of Lam_arity.t
  | Submodule of Lam_arity.t array

(* TODO: add a magic number *)
type cmj_value = {
  arity : arity ;
  closed_lambda : Lam.t option ; 
  (** Either constant or closed functor *)
}

type effect = string option


let single_na = Single NA
(** we don't force people to use package *)

type t = {
  values : cmj_value String_map.t;
  effect : effect;
  npm_package_path : Js_packages_info.t ;
}

let cmj_magic_number =  "BUCKLE20170811"
let cmj_magic_number_length = 
  String.length cmj_magic_number

let pure_dummy = 
  {
    values = String_map.empty;
    effect = None;
    npm_package_path = Empty;
  }

let no_pure_dummy = 
  {
    values = String_map.empty;
    effect = Some Ext_string.empty;
    npm_package_path = Empty;  
  }



let from_file name : t =
  let ic = open_in_bin name in 
  let buffer = really_input_string ic cmj_magic_number_length in 
  if buffer <> cmj_magic_number then
    Ext_pervasives.failwithf ~loc:__LOC__ 
      "cmj files have incompatible versions, please rebuilt using the new compiler : %s" 
        __LOC__
  else 
    let v  : t = input_value ic in 
    close_in ic ;
    v 


let from_string s : t = 
  let magic_number = String.sub s 0 cmj_magic_number_length in 
  if magic_number = cmj_magic_number then 
    Marshal.from_string s  cmj_magic_number_length
  else 
    Ext_pervasives.failwithf ~loc:__LOC__ 
      "cmj files have incompatible versions, please rebuilt using the new compiler : %s"
        __LOC__

let to_file name (v : t) = 
  let oc = open_out_bin name in 
  output_string oc cmj_magic_number;
  output_value oc v;
  close_out oc 


(* strategy:
   If not installed, use the distributed [cmj] files, 
   make sure that the distributed files are platform independent
*)
let find_cmj file : string * t = 
  match Config_util.find_opt file with
  | Some f
    -> 
    f, from_file f             
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
            (* see {!Js_program_loader.string_of_module_id} *)
          end
        | exception Not_found 
          ->     
          Ext_log.warn __LOC__ "@[%s not found @]" file ;
          Js_cmj_format.no_pure_dummy )
#else
        Bs_exception.error (Cmj_not_found file)
#end        


  