
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




(** It makes sense to have this function raise, when [bsb] could not resolve a package, it used to mean
    a failure 
*)
let  resolve_bs_package  
    ~cwd
    pkg = 
  let marker = Literals.bsconfig_json in 
  let sub_path = pkg // marker  in
  let rec aux  cwd  = 
    let abs_marker =  cwd // Literals.node_modules // sub_path in 
    if Sys.file_exists abs_marker then (* Some *) (Filename.dirname abs_marker)
    else 
      let cwd' = Filename.dirname cwd in (* TODO: may non-terminating when see symlinks *)
      if String.length cwd' < String.length cwd then  
        aux    cwd' 
      else 
        try 
          let abs_marker = 
            Sys.getenv "npm_config_prefix" 
            // "lib" // Literals.node_modules // sub_path in
          if Sys.file_exists abs_marker
          then 
            Filename.dirname abs_marker
          else
            begin 
              Format.fprintf Format.err_formatter 
                "@{<error>Package not found: resolving package %s in %s  @}@." pkg cwd ;             
              Bsb_exception.package_not_found ~pkg ~json:None
            end
        with 
          Not_found -> 
          begin 
            Format.fprintf Format.err_formatter 
              "@{<error>Package not found: resolving package %s in %s  @}@." pkg cwd ;             
            Bsb_exception.package_not_found ~pkg ~json:None
          end
  in
  aux cwd 


let cache = String_hashtbl.create 0 

(** TODO: collect all warnings and print later *)
let resolve_bs_package ~cwd package = 
  match String_hashtbl.find_opt cache package with 
  | None -> 
    let result = resolve_bs_package ~cwd package in 
    Format.fprintf Format.std_formatter "@{<info>Package@} %s -> %s@." package result ; 
    String_hashtbl.add cache package result ;
    result 
  | Some x 
    -> 
    let result = resolve_bs_package ~cwd package in 
    if result <> x then 
      begin 
        Format.fprintf Format.err_formatter  
          "@{<warning>Duplicated package:@} %s %s (chosen) vs %s in %s @." package x result cwd;
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
(*   aux cwd  *)
