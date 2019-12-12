
(* Copyright (C) 2017- Authors of BuckleScript
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

let (//) = Filename.concat

type t = Bsb_pkg_types.t

(* TODO: be more restrict 
  [bsconfig.json] does not always make sense, 
  when resolving [ppx-flags]
*)
let make_sub_path (x : t) : string = 
  Literals.node_modules // Bsb_pkg_types.to_string x

let node_paths : string list Lazy.t =     
  lazy (try Ext_string.split (Sys.getenv "NODE_PATH")
              (if Sys.win32 then ';' else ':')   
        with _ -> [])
(** It makes sense to have this function raise, when [bsb] could not resolve a package, it used to mean
    a failure
*)
let check_dir dir =
  match Sys.file_exists dir with
  | true -> Some(dir)
  | false -> None

let  resolve_bs_package_aux  ~cwd (pkg : t) =
  (* First try to resolve recursively from the current working directory  *)
  let sub_path = make_sub_path pkg   in
  let rec aux  cwd  =
    let abs_marker =  cwd // sub_path in
    if Sys.file_exists abs_marker then abs_marker
    else
      let another_cwd = Filename.dirname cwd in (* TODO: may non-terminating when see symlinks *)
      if String.length another_cwd < String.length cwd then
        aux    another_cwd
      else (* To the end try other possiblilities [NODE_PATH]*)
        (match Ext_list.find_opt (Lazy.force node_paths)
                 (fun dir -> check_dir (dir // Bsb_pkg_types.to_string pkg))  with
        | Some(resolved_dir) -> resolved_dir
        | None -> Bsb_exception.package_not_found ~pkg ~json:None)    
  in
   aux cwd 
    
    
    
    
    

module Coll = Hash.Make(struct
  type nonrec t = t 
  let equal = Bsb_pkg_types.equal
  let hash (x : t) = Hashtbl.hash x     
end)


let cache : string Coll.t = Coll.create 0


let to_list cb  =   
  Coll.to_list cache  cb 
  
(* Some package managers will implement "postinstall" caches, that do not
 * keep their build artifacts in the local node_modules. Similar to
 * npm_config_prefix, bs_custom_resolution allows these to specify the
 * exact location of build cache, but on a per-package basis. Implemented as
 * environment lookup to avoid invasive changes to bsconfig and mandates. *)
let custom_resolution = lazy
  (match Sys.getenv "bs_custom_resolution" with
  | exception Not_found  -> false
  | "true"  -> true
  | _ -> false)

let pkg_name_as_variable package =
  Bsb_pkg_types.to_string package
  |> fun s -> Ext_string.split s '@'
  |> String.concat ""
  |> fun s -> Ext_string.split s '_'
  |> String.concat "__"
  |> fun s -> Ext_string.split s '/'
  |> String.concat "__slash__"
  |> fun s -> Ext_string.split s '.'
  |> String.concat "__dot__"
  |> fun s -> Ext_string.split s '-'
  |> String.concat "_"

(** TODO: collect all warnings and print later *)
let resolve_bs_package ~cwd (package : t) =
  if Lazy.force custom_resolution then
  begin
    Bsb_log.info "@{<info>Using Custom Resolution@}@.";
    let custom_pkg_loc = pkg_name_as_variable package ^ "__install" in
    let custom_pkg_location = lazy (Sys.getenv custom_pkg_loc) in
    match Lazy.force custom_pkg_location with
    | exception Not_found ->
        begin
          Bsb_log.error
            "@{<error>Custom resolution of package %s does not exist in var %s @}@."
            (Bsb_pkg_types.to_string package)
            custom_pkg_loc;
          Bsb_exception.package_not_found ~pkg:package ~json:None
        end
    | path when not (Sys.file_exists path) ->
        begin
          Bsb_log.error
            "@{<error>Custom resolution of package %s does not exist on disk: %s=%s @}@."
            (Bsb_pkg_types.to_string package)
            custom_pkg_loc
            path;
          Bsb_exception.package_not_found ~pkg:package ~json:None
        end
    | path ->
      begin
        Bsb_log.info
          "@{<info>Custom Resolution of package %s in var %s found at %s@}@."
          (Bsb_pkg_types.to_string package)
          custom_pkg_loc
          path;
        path
      end
    end
  else
    match Coll.find_opt cache package with
    | None ->
      let result = resolve_bs_package_aux ~cwd package in
      Bsb_log.info "@{<info>Package@} %a -> %s@." Bsb_pkg_types.print package result ;
      Coll.add cache package result ;
      result
    | Some x
      ->
      let result = resolve_bs_package_aux ~cwd package in
      if result <> x then
        begin
          Bsb_log.warn
            "@{<warning>Duplicated package:@} %a %s (chosen) vs %s in %s @." 
              Bsb_pkg_types.print package x result cwd;
        end;
      x


(** The package does not need to be a bspackage
    example:
    {[
      resolve_npm_package_file ~cwd "reason/refmt";;
      resolve_npm_package_file ~cwd "reason/refmt/xx/yy"
    ]}
    It also returns the path name
    Note the input [sub_path] is already converted to physical meaning path according to OS
*)
(* let resolve_npm_package_file ~cwd sub_path = *) 
(*   let rec aux  cwd  =  *)
(*     let abs_marker =  cwd // Literals.node_modules // sub_path in  *)
(*     if Sys.file_exists abs_marker then Some abs_marker *)
(*     else  *)
(*       let cwd' = Filename.dirname cwd in  *)
(*       if String.length cwd' < String.length cwd then   *)
(*         aux cwd'  *)
(*       else  *)
(*         try  *)
(*           let abs_marker =  *)
(*             Sys.getenv "npm_config_prefix"  *)
(*             // "lib" // Literals.node_modules // sub_path in *)
(*           if Sys.file_exists abs_marker *)
(*           then Some  abs_marker *)
(*           else None *)
(*             (\* Bs_exception.error (Bs_package_not_found name) *\) *)
(*         with  *)
(*           Not_found -> None *)
(*           (\* Bs_exception.error (Bs_package_not_found name)           *\) *)
(*   in *)
(*   aux cwd *)
