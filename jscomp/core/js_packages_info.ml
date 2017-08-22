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
  | Goog
  | Es6
  | Es6_global (* ignore node_modules, just calcluating relative path *)
  | AmdJS_global (* see ^ *)

(* ocamlopt could not optimize such simple case..*)
let compatible (exist : module_system) 
    (query : module_system) =
  match query with 
  | NodeJS -> exist = NodeJS 
  | AmdJS -> exist = AmdJS
  | Goog -> exist = Goog
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
  | Empty (* No set *)
  | NonBrowser of (package_name * package_info  list)
  (* we don't want force people to use package *) 


let string_of_module_system (ms : module_system) = 
  (match ms with 
   | NodeJS -> "NodeJS"
   | AmdJS -> "AmdJS"
   | Goog -> "Goog"
   | Es6 -> "Es6"
   | Es6_global -> "Es6_global"
   | AmdJS_global -> "AmdJS_globl"
  )

let module_system_of_string package_name : module_system option = 
  match package_name with
  | "commonjs" -> Some NodeJS
  | "amdjs" -> Some AmdJS
  | "goog" -> Some Goog
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
    (p : t) = 
  match p with 
  | Empty -> Format.pp_print_string fmt  "<Empty>"
  | NonBrowser (name, ls) ->
    Format.fprintf fmt "@[%s;@ @[%a@]@]"
      name
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.pp_print_space fmt ())
         dump_package_info 
      ) ls

type info_query =
  | Package_empty
  | Package_script of string
  | Package_found of package_name * string
  | Package_not_found 



let query_package_infos 
    (package_infos : t) module_system : info_query =
  match package_infos with
  | Empty -> Package_empty
  | NonBrowser (name, []) -> Package_script name
  | NonBrowser (name, paths) ->
    begin match List.find (fun (k, _) -> 
        compatible k  module_system) paths with
    | (_, x) -> Package_found (name, x)
    | exception _ -> Package_not_found
    end


(* for a single pass compilation, [output_dir]
   can be cached
*)
let get_output_dir ~pkg_dir module_system 
    ~hint_output_dir 
    (* output_prefix   *)
    packages_info =
  match packages_info with
  | Empty | NonBrowser (_, [])->
    if Filename.is_relative hint_output_dir then
      Filename.concat (Lazy.force Ext_filename.cwd )
        hint_output_dir
        (* (Filename.dirname output_prefix) *)
    else
      hint_output_dir
      (* Filename.dirname output_prefix *)
  | NonBrowser (_,  modules) ->
    begin match List.find (fun (k,_) -> 
        compatible k  module_system) modules with
    | (_, path) -> Filename.concat pkg_dir  path
    |  exception _ -> assert false
    end


let add_npm_package_path s (packages_info : t)  : t =
  match packages_info  with
  | Empty ->
    Ext_pervasives.bad_argf "please set package name first using -bs-package-name ";
  | NonBrowser(name,  envs) ->
    let env, path =
      match Ext_string.split ~keep_empty:false s ':' with
      | [ package_name; path]  ->
        (match module_system_of_string package_name with
         | Some x -> x
         | None ->
           Ext_pervasives.bad_argf "invalid module system %s" package_name), path
      | [path] ->
        NodeJS, path
      | _ ->
        Ext_pervasives.bad_argf "invalid npm package path: %s" s
    in
    NonBrowser (name,  ((env,path) :: envs))    



let (//) = Filename.concat 


let string_of_module_id_in_browser (x : Lam_module_ident.t) =  
    match x.kind with
    | External name -> name
    | Runtime | Ml -> 
      "stdlib" // String.uncapitalize x.id.name
    


let string_of_module_id 
    ~hint_output_dir
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
        let rebase different_package package_dir dep =
          let current_unit_dir =
            `Dir (get_output_dir 
                  ~pkg_dir:package_dir module_system 
                  ~hint_output_dir
                  current_package_info
                  ) in
          Ext_filename.node_relative_path  different_package current_unit_dir dep 
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
                    (get_output_dir 
                      ~pkg_dir:(Lazy.force Ext_filename.package_dir)
                       module_system 
                       ~hint_output_dir
                       current_package_info
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

    