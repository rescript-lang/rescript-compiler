(* Copyright (C) 2017 Authors of BuckleScript
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
(*
let (=)  (x : int) (y:float) = assert false 
*)

(* "xx/lib/ocaml/js.cmj" 
   Enhancement: This can be delegated to build system
*)
let runtime_package_path : string Lazy.t = 
  lazy (Filename.dirname (Filename.dirname 
    (Filename.dirname 
      (match Config_util.find_opt "js.cmj" with 
      | None -> assert false
      | Some x -> x))))

let (//) = Filename.concat 


let fix_path_for_windows : string -> string = 
  if Ext_sys.is_windows_or_cygwin then Ext_string.replace_backward_slash
  else fun s -> s 


let get_runtime_module_path 
    (dep_module_id : Lam_module_ident.t) 
    current_package_info module_system = 
  let current_info_query = 
    Js_packages_info.query_package_infos current_package_info
      module_system  in
  let js_file =  Ext_namespace.js_name_of_modulename Little_js dep_module_id.id.name in     
  match current_info_query with        
  | Package_not_found -> assert false
  | Package_script -> 
    Js_packages_info.runtime_package_path module_system js_file          
  | Package_found(cur_package_name, cur_path) -> 
    let  dep_path  = 
      "lib" // Js_packages_info.runtime_dir_of_module_system module_system in 
    if  Js_packages_info.is_runtime_package cur_package_name then 
      Ext_path.node_rebase_file
        ~from:cur_path
        ~to_:dep_path 
        js_file
        (** TODO: we assume that both [x] and [path] could only be relative path
            which is guaranteed by [-bs-package-output]
        *)
    else  
      match module_system with 
      | NodeJS | Es6 -> 
        Js_packages_info.runtime_package_path module_system js_file              
      (** Note we did a post-processing when working on Windows *)
      | Es6_global 
        -> 
        (** lib/ocaml/xx.cmj --               
            HACKING: FIXME
            maybe we can caching relative package path calculation or employ package map *)
        (* assert false  *)
        Ext_path.rel_normalized_absolute_path              
          ~from:(
            Js_packages_info.get_output_dir 
              current_package_info
              ~package_dir:(Lazy.force Ext_filename.package_dir)
              module_system )
          (Lazy.force runtime_package_path // dep_path // js_file)  



(* [output_dir] is decided by the command line argument *)
let string_of_module_id 
    (dep_module_id : Lam_module_ident.t) 
    ~(output_dir : string )
    (module_system : Js_packages_info.module_system)        
  : string =
  let current_package_info = Js_packages_state.get_packages_info ()  in 
  fix_path_for_windows (    
    match dep_module_id.kind  with 
    | External name -> name (* the literal string for external package *)
    (** This may not be enough, 
        1. For cross packages, we may need settle 
        down a single js package
        2. We may need es6 path for dead code elimination
         But frankly, very few JS packages have no dependency, 
         so having plugin may sound not that bad   
    *)
    | Runtime  -> 
      get_runtime_module_path dep_module_id current_package_info module_system
    | Ml  -> 
      let id = dep_module_id.id in      
      let current_pkg_info = 
        Js_packages_info.query_package_infos current_package_info
          module_system  
      in
      match Lam_compile_env.get_package_path_from_cmj dep_module_id with 
      | None -> 
        Bs_exception.error (Missing_ml_dependency dep_module_id.id.name)
      | Some (cmj_path, package_info, little) -> 
        let js_file =  Ext_namespace.js_name_of_modulename little id.name in 
        let dependency_pkg_info =  
          Js_packages_info.query_package_infos package_info module_system 
        in 
        match dependency_pkg_info, current_pkg_info with
        | Package_not_found , _  -> 
          Bs_exception.error (Missing_ml_dependency dep_module_id.id.name)
        | Package_script , Package_found _  -> 
          Bs_exception.error (Dependency_script_module_dependent_not js_file)
        | (Package_script  | Package_found _ ), Package_not_found -> assert false

        | Package_found(dep_package_name, dep_path), 
          Package_script 
          ->    
#if BS_NATIVE then
          if Filename.is_relative dep_path then 
            dep_package_name // dep_path // js_file
          else 
            dep_path // js_file
#else
          dep_package_name // dep_path // js_file
#end

        | Package_found(dep_package_name, dep_path),
          Package_found(cur_package_name, cur_path) -> 
          if  Js_packages_info.same_package cur_package_name  dep_package_name then 
            Ext_path.node_rebase_file
              ~from:cur_path
              ~to_:dep_path 
              js_file
              (** TODO: we assume that both [x] and [path] could only be relative path
                  which is guaranteed by [-bs-package-output]
              *)
          else  
            begin match module_system with 
              | NodeJS | Es6 -> 
#if BS_NATIVE then
          if Filename.is_relative dep_path then 
            dep_package_name // dep_path // js_file
          else 
            dep_path // js_file
#else
                dep_package_name // dep_path // js_file
#end
              (** Note we did a post-processing when working on Windows *)
              | Es6_global 
              -> 
                (** lib/ocaml/xx.cmj --               
                    HACKING: FIXME
                    maybe we can caching relative package path calculation or employ package map *)
                (* assert false  *)

                begin 
                  Ext_path.rel_normalized_absolute_path              
                    ~from:(
                      Js_packages_info.get_output_dir 
                        current_package_info
                        ~package_dir:(Lazy.force Ext_filename.package_dir)
                        module_system 
                    )
                    ((Filename.dirname 
                        (Filename.dirname (Filename.dirname cmj_path))) // dep_path // js_file)              
                end
            end
        | Package_script, Package_script 
          -> 
          match Config_util.find_opt js_file with 
          | Some file -> 
            let basename = Filename.basename file in 
            let dirname = Filename.dirname file in 
            Ext_path.node_rebase_file
              ~from:(
                Ext_path.absolute_path 
                  Ext_filename.cwd output_dir)
              ~to_:(
                Ext_path.absolute_path 
                  Ext_filename.cwd
                  dirname)
              basename  
          | None -> 
            Bs_exception.error (Js_not_found js_file))

  

(* Override it in browser *)
#if BS_COMPILER_IN_BROWSER then   
let string_of_module_id_in_browser (x : Lam_module_ident.t) =  
   match x.kind with
   | External name -> name
   | Runtime | Ml -> 
                   "./stdlib/" ^  Ext_string.uncapitalize_ascii x.id.name ^ ".js"
let string_of_module_id 
    (id : Lam_module_ident.t)
    ~output_dir:(_:string)
    (_module_system : Js_packages_info.module_system)
     = string_of_module_id_in_browser id
#end
