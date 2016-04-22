(* BuckleScript compiler
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



module E = Js_exp_make
module S = Js_stmt_make



open Js_output.Ops

let string_of_module_id (x : Lam_module_ident.t) : string =           
  match x.kind  with 
  | Runtime  
  | Ml  -> 
    let id = x.id in
    let file = Printf.sprintf "%s.js" id.name in
    begin match Js_config.get_env () with 
    | Goog _ -> 
      (*TODO: we should store 
        the goog module name in the [cmj] file
      *)
      let base =  String.uncapitalize id.name in
      begin match Lam_compile_env.get_goog_package_name x with 
      | None 
      | Some "" -> 
        base 
      | Some v -> v ^ "." ^ base 
      end
    | Browser 
      (* In browser *)
      ->  
      let target = Filename.chop_extension @@ String.uncapitalize file in
      if String_set.mem target Js_config.runtime_set   then
        "./runtime/" ^  target
      else
        "./stdlib/" ^ target 
    | AmdJS
    | NodeJS -> 
      let filename = String.uncapitalize id.name in
      if String_set.mem filename Js_config.runtime_set  then 
        let path = 
          (* For the runtime, only [JS] files are needed, and 
             unlike the stdlib, [bsc] have some pre-built knowledge 
             about where it is, since in general, [runtime] 
             is *transparent* to the user
          *)        
          match Sys.getenv "OCAML_JS_RUNTIME_PATH" with 
          | exception Not_found -> 
            Filename.concat 
              (Filename.dirname (Filename.dirname Sys.executable_name))
              "runtime"
          | f ->  f  in
        Ext_filename.node_relative_path !Location.input_name
          (Filename.concat path filename)        
      else 
        begin match Config_util.find file with   
          (* for some primitive files, no cmj support *)
          | exception Not_found ->
            Ext_log.warn __LOC__ "@[%s not found in search path - while compiling %s @] "
              file !Location.input_name ;
            Printf.sprintf "%s" 
              (String.uncapitalize id.name) 
          (* maybe from third party library*)
          (* Check: be consistent when generating js files
             A.ml -> a.js
             a.ml -> a.js
             check generated [js] file if it's capital or not
             Actually, we can not tell its original name just from [id], 
             so we just always general litte_case.js
          *)
          | path ->
            Ext_filename.node_relative_path !Location.input_name path

        end
    end
  | External name -> name



(* support es6 modules instead
   TODO: enrich ast to support import export 
   http://www.ecma-international.org/ecma-262/6.0/#sec-imports
   For every module, we need [Ident.t] for accessing and [filename] for import, 
   they are not necessarily the same.

   Es6 modules is not the same with commonjs, we use commonjs currently
   (play better with node)

   FIXME: the module order matters?
*)

let make_program name  export_idents block : J.program = 

  {
    name;

    exports = export_idents ; 
    export_set = Ident_set.of_list export_idents;
    block = block;

  }
let decorate_deps modules side_effect program : J.deps_program = 

  { program ; modules ; side_effect }

