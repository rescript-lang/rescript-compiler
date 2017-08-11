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








module E = Js_exp_make
module S = Js_stmt_make



(** Design guides:
    1. We don't want to force user to have 
       [-bs-package-name] and [-bs-package-output] set

       [bsc.exe -c hello.ml] should just work 
       by producing a [hello.js] file in the same directory

    Some designs due to legacy reasons that we don't have all runtime
    written in OCaml, so it might only have js files (no cmjs) for Runtime kind
    {[
      begin match Config_util.find file with   
        (* maybe from third party library*)
        (* Check: be consistent when generating js files
           A.ml -> a.js
           a.ml -> a.js
           check generated [js] file if it's capital or not
           Actually, we can not tell its original name just from [id], 
           so we just always general litte_case.js
        *)
        | file ->
          rebase (`File file)
        (* for some primitive files, no cmj support *)
        | exception Not_found ->
          Ext_pervasives.failwithf ~loc:__LOC__ 
            "@[%s not found in search path - while compiling %s @] "
            file !Location.input_name 
      end

    ]}

*)

let (//) = Filename.concat 

let string_of_module_id ~output_prefix
    (module_system : Js_packages_info.module_system)
    (x : Lam_module_ident.t) : string =
#if BS_COMPILER_IN_BROWSER then   
    match x.kind with
    | Runtime | Ml -> 
      "stdlib" // String.uncapitalize x.id.name
    | External name -> name
#else

    let result = 
      match x.kind  with 
      | Runtime  
      | Ml  -> 
        let id = x.id in
        let js_file = Ext_filename.output_js_basename id.name in 
        let current_package_info = Js_config.get_packages_info () in 
        let rebase different_package package_dir dep =
          let current_unit_dir =
            `Dir (Js_packages_info.get_output_dir 
                  ~pkg_dir:package_dir module_system 
                  output_prefix 
                  current_package_info
                  ) in
          Ext_filename.node_relative_path  different_package current_unit_dir dep 
        in 
        let cmj_path, dependency_pkg_info = 
          Lam_compile_env.get_package_path_from_cmj module_system x 
        in
        let current_pkg_info = 
            Js_packages_info.query_package_infos current_package_info
            module_system  
        in
        begin match module_system,  dependency_pkg_info, current_pkg_info with
          | _, Package_not_found , _ 
            -> 
            Bs_exception.error (Missing_ml_dependency x.id.name)
          (*TODO: log which module info is not done
          *)
          | Goog, (Package_empty | Package_script _), _ 
            -> 
            Bs_exception.error (Dependency_script_module_dependent_not js_file)
          | (AmdJS | NodeJS | Es6 | Es6_global | AmdJS_global),
            ( Package_empty | Package_script _) ,
            Package_found _  -> 
            Bs_exception.error (Dependency_script_module_dependent_not js_file)
          | Goog , Package_found (package_name, x), _  -> 
            package_name  ^ "." ^  String.uncapitalize id.name
          | (AmdJS | NodeJS| Es6 | Es6_global|AmdJS_global),
           (Package_empty | Package_script _ | Package_found _ ), Package_not_found -> assert false

          | (AmdJS | NodeJS | Es6 | Es6_global|AmdJS_global), 
            Package_found(package_name, x),
            Package_found(current_package, path) -> 
            if  current_package = package_name then 
              let package_dir = Lazy.force Ext_filename.package_dir in
              rebase false package_dir (`File (package_dir // x // js_file)) 
            else 
              begin match module_system with 
              | AmdJS | NodeJS | Es6 -> 
                package_name // x // js_file
              | Goog -> assert false (* see above *)
              | Es6_global 
              | AmdJS_global -> 
               (** lib/ocaml/xx.cmj --               
                HACKING: FIXME
                maybe we can caching relative package path calculation or employ package map *)
                (* assert false  *)
                
                begin 
                  Ext_filename.rel_normalized_absolute_path              
                    (Js_packages_info.get_output_dir 
                      ~pkg_dir:(Lazy.force Ext_filename.package_dir)
                       module_system 
                       output_prefix
                       (Js_config.get_packages_info()) 
                       )
                    ((Filename.dirname 
                        (Filename.dirname (Filename.dirname cmj_path))) // x // js_file)              
                end
              end
          | (AmdJS | NodeJS | Es6 | AmdJS_global | Es6_global), Package_found(package_name, x), 
            Package_script(current_package)
            ->    
            if current_package = package_name then 
              let package_dir = Lazy.force Ext_filename.package_dir in
              rebase false package_dir (`File (
                  package_dir // x // js_file)) 
            else 
              package_name // x // js_file
          | (AmdJS | NodeJS | Es6 | AmdJS_global | Es6_global), 
            Package_found(package_name, x), Package_empty 
            ->    package_name // x // js_file
          |  (AmdJS | NodeJS | Es6 | AmdJS_global | Es6_global), 
             (Package_empty | Package_script _) , 
             (Package_empty  | Package_script _)
            -> 
            begin match Config_util.find_opt js_file with 
              | Some file -> 
                let package_dir = Lazy.force Ext_filename.package_dir in
                rebase true package_dir (`File file) 
              (* Code path: when dependency is commonjs 
                 while depedent is Empty or PackageScript
              *)
              | None -> 
                Bs_exception.error (Js_not_found js_file)
            end
          
        end
      | External name -> name (* the literal string for external package *)
        (** This may not be enough, 
          1. For cross packages, we may need settle 
            down a single js package
          2. We may need es6 path for dead code elimination
             But frankly, very few JS packages have no dependency, 
             so having plugin may sound not that bad   
        *)
      in 
    if Ext_sys.is_windows_or_cygwin then Ext_string.replace_backward_slash result 
    else result 
#end


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

