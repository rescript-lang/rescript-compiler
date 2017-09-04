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


type path = string

type module_system =
  | NodeJS 
  | AmdJS 
  | Es6
  | Es6_global (* ignore node_modules, just calcluating relative path *)
  | AmdJS_global (* see ^ *)

(* ocamlopt could not optimize such simple case..*)
let compatible (exist : module_system) 
    (query : module_system) =
  match query with 
  | NodeJS -> exist = NodeJS 
  | AmdJS -> exist = AmdJS
  | Es6  -> exist = Es6
  | Es6_global  
    -> exist = Es6_global || exist = Es6
  | AmdJS_global 
    -> exist = AmdJS_global || exist = AmdJS
(* As a dependency Leaf Node, it is the same either [global] or [not] *)


type package_info =
  ( module_system * string )

type package_name  = string
type t =
  { 
    name : package_name ;
    module_systems: package_info  list
  }
  (* we don't want force people to use package *) 

(** 
  TODO: not allowing user to provide such specific package name 
  For empty package, [-bs-package-output] does not make sense
  it is only allowed to generate commonjs file in the same directory
*)  
let empty = 
  { name = "_";
    module_systems =  []
  }
let from_name name =
   {
  name ;
  module_systems = [] 
}
let is_empty  (x : t) =
  match x with 
  | { name = "_" } -> true 
  | _ -> false 

let string_of_module_system (ms : module_system) = 
    match ms with 
   | NodeJS -> "NodeJS"
   | AmdJS -> "AmdJS"
   | Es6 -> "Es6"
   | Es6_global -> "Es6_global"
   | AmdJS_global -> "AmdJS_globl"


let module_system_of_string package_name : module_system option = 
  match package_name with
  | "commonjs" -> Some NodeJS
  | "amdjs" -> Some AmdJS
  | "es6" -> Some Es6
  | "es6-global" -> Some Es6_global
  | "amdjs-global" -> Some AmdJS_global 
  | _ -> None 

let dump_package_info 
    (fmt : Format.formatter)
    ((ms, name) : package_info)
  = 
  Format.fprintf
    fmt 
    "@[%s:@ %s@]"
    (string_of_module_system ms)
    name 


let dump_packages_info 
    (fmt : Format.formatter) 
    ({name ; module_systems = ls } : t) = 
    Format.fprintf fmt "@[%s;@ @[%a@]@]"
      name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())
         dump_package_info 
      ) ls

type info_query =
  | Package_script 
  | Package_found of package_name * string
  | Package_not_found 



let query_package_infos 
    (package_info : t) module_system : info_query =
    if is_empty package_info then Package_script 
    else 
     match List.find (fun (k, _) -> 
        compatible k  module_system) package_info.module_systems with
      | (_, x) -> Package_found (package_info.name, x)
      | exception _ -> Package_not_found



(* for a single pass compilation, [output_dir]
   can be cached
*)
let get_output_dir ~pkg_dir module_system 
    ({module_systems } : t ) =
    match List.find (fun (k,_) -> 
        compatible k  module_system) module_systems with
    | (_, path) -> Filename.concat pkg_dir  path
    |  exception _ -> assert false



let add_npm_package_path s (packages_info : t)  : t =
    if is_empty packages_info then 
      Ext_pervasives.bad_argf "please set package name first using -bs-package-name "
    else   
    let env, path =
      match Ext_string.split ~keep_empty:false s ':' with
      | [ module_system; path]  ->
        (match module_system_of_string module_system with
         | Some x -> x
         | None ->
           Ext_pervasives.bad_argf "invalid module system %s" module_system), path
      | [path] ->
        NodeJS, path
      | _ ->
        Ext_pervasives.bad_argf "invalid npm package path: %s" s
    in
    { packages_info with module_systems = (env,path)::packages_info.module_systems}
    



let (//) = Filename.concat 


let string_of_module_id_in_browser (x : Lam_module_ident.t) =  
    match x.kind with
    | External name -> name
    | Runtime | Ml -> 
      "stdlib" // String.uncapitalize x.id.name
    


let string_of_module_id 
    (module_system : module_system)
    (current_package_info : t)
    (get_package_path_from_cmj : 
     Lam_module_ident.t -> (string * t) option
    )
    (x : Lam_module_ident.t) : string =
#if BS_COMPILER_IN_BROWSER then   
    string_of_module_id_in_browser x 
#else
    let result = 
      match x.kind  with 
      | External name -> name (* the literal string for external package *)
        (** This may not be enough, 
          1. For cross packages, we may need settle 
            down a single js package
          2. We may need es6 path for dead code elimination
             But frankly, very few JS packages have no dependency, 
             so having plugin may sound not that bad   
        *)
      | Runtime  
      | Ml  -> 
        let id = x.id in
        let js_file =  Ext_namespace.js_name_of_basename id.name in 
        let rebase  ~dependency  ~different_package package_dir=
          let current_unit_dir =
            `Dir (get_output_dir 
                  ~pkg_dir:package_dir module_system 
                  current_package_info
                  ) in
          Ext_filename.node_relative_path  different_package current_unit_dir dependency 
        in 
        let cmj_path, dependency_pkg_info = 
          match get_package_path_from_cmj x with 
          | None -> Ext_string.empty, Package_not_found
          | Some (cmj_path, package_info) -> 
            cmj_path, query_package_infos package_info module_system
        in
        let current_pkg_info = 
            query_package_infos current_package_info
            module_system  
        in
        begin match module_system,  dependency_pkg_info, current_pkg_info with
          | _, Package_not_found , _ 
            -> 
            Bs_exception.error (Missing_ml_dependency x.id.name)
          | (AmdJS | NodeJS | Es6 | Es6_global | AmdJS_global),
            Package_script ,
            Package_found _  -> 
            Bs_exception.error (Dependency_script_module_dependent_not js_file)
          | (AmdJS | NodeJS| Es6 | Es6_global|AmdJS_global),
           (Package_script  | Package_found _ ), Package_not_found -> assert false
          | (AmdJS | NodeJS | Es6 | Es6_global|AmdJS_global), 
            Package_found(package_name, x),
            Package_found(current_package, path) -> 
            if  current_package = package_name then 
              let package_dir = Lazy.force Ext_filename.package_dir in
              rebase ~different_package:false package_dir ~dependency:(`File (package_dir // x // js_file)) 
            else 
              begin match module_system with 
              | AmdJS | NodeJS | Es6 -> 
                package_name // x // js_file
              | Es6_global 
              | AmdJS_global -> 
               (** lib/ocaml/xx.cmj --               
                HACKING: FIXME
                maybe we can caching relative package path calculation or employ package map *)
                (* assert false  *)
                
                begin 
                  Ext_filename.rel_normalized_absolute_path              
                    (get_output_dir 
                      ~pkg_dir:(Lazy.force Ext_filename.package_dir)
                       module_system 
                       current_package_info
                       )
                    ((Filename.dirname 
                        (Filename.dirname (Filename.dirname cmj_path))) // x // js_file)              
                end
              end
          | (AmdJS | NodeJS | Es6 | AmdJS_global | Es6_global), Package_found(package_name, x), 
            Package_script 
            ->    
              package_name // x // js_file
          |  (AmdJS | NodeJS | Es6 | AmdJS_global | Es6_global), 
            Package_script , 
             Package_script 
            -> 
            begin match Config_util.find_opt js_file with 
              | Some file -> 
                let package_dir = Lazy.force Ext_filename.package_dir in
                rebase ~different_package:true package_dir ~dependency:(`File file) 
              (* Code path: when dependency is commonjs 
                 while depedent is Empty or PackageScript
              *)
              | None -> 
                Bs_exception.error (Js_not_found js_file)
            end
          
        end

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

    