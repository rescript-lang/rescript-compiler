
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
  

(** It makes sense to have this function raise, when [bsb] could not resolve a package, it used to mean
    a failure
*)
let  resolve_bs_package_aux  ~cwd (pkg : t) =
  let sub_path = make_sub_path pkg   in
  let rec aux  cwd  =
    let abs_marker =  cwd //  sub_path in
    if Sys.file_exists abs_marker then abs_marker
    else
      let another_cwd = Filename.dirname cwd in (* TODO: may non-terminating when see symlinks *)
      if String.length another_cwd < String.length cwd then
        aux    another_cwd
      else (* To the end try other possiblilities *)
        begin match Sys.getenv "npm_config_prefix"
                    // "lib" // sub_path with
        | abs_marker when Sys.file_exists abs_marker ->
          abs_marker
        | _ ->
            Bsb_exception.package_not_found ~pkg ~json:None
        | exception Not_found ->
            Bsb_exception.package_not_found ~pkg ~json:None
        end
  in
  aux cwd

module Coll = Hashtbl_make.Make(struct
  type nonrec t = t 
  let equal = Bsb_pkg_types.equal
  let hash (x : t) = Hashtbl.hash x     
end)
let cache : string Coll.t = Coll.create 0

(** TODO: collect all warnings and print later *)
let resolve_bs_package ~cwd (package : t) =
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

(* Some package managers will implement "postinstall" caches, that do not
 * keep their build artifacts in the local node_modules. Similar to
 * npm_config_prefix, bs_custom_resolution allows these to specify the
 * exact location of build cache, but on a per-package basis. Implemented as
 * environment lookup to avoid invasive changes to bsconfig and mandates. *)
let custom_resolution =
 match Sys.getenv "bs_custom_resolution" with
  | exception Not_found  -> false
  | "true"  -> true
  | _ -> false

let regex_at = Str.regexp "@"
let regex_unders = Str.regexp "_+"
let regex_slash = Str.regexp "\\/"
let regex_dot = Str.regexp "\\."
let regex_hyphen = Str.regexp "-"
let pkg_name_as_variable pkg =
  Bsb_pkg_types.to_string pkg
  |> Str.replace_first regex_at ""
  |> Str.global_replace regex_unders "\\0_"
  |> Str.global_replace regex_slash "__slash__"
  |> Str.global_replace regex_dot "__dot__"
  |> Str.global_replace regex_hyphen "_"

let resolve_bs_package ~cwd (pkg : t) =
  if custom_resolution then
    begin
      Bsb_log.info "@{<error>Using Custom Resolution@}@.";
      let custom_pkg_loc = pkg_name_as_variable pkg ^ "__install" in
      match Sys.getenv custom_pkg_loc with
      | exception Not_found ->
          begin
            Bsb_log.error
              "@{<error>Custom resolution of package %s does not exist in var %s @}@."
              (Bsb_pkg_types.to_string pkg)
              custom_pkg_loc;
            Bsb_exception.package_not_found ~pkg ~json:None
          end
      | path when not (Sys.file_exists path) ->
         begin
           Bsb_log.error
             "@{<error>Custom resolution of package %s does not exist on disk: %s=%s @}@."
             (Bsb_pkg_types.to_string pkg)
             custom_pkg_loc
             path;
           Bsb_exception.package_not_found ~pkg ~json:None
         end
      | path ->
        begin
          Bsb_log.info
            "@{<error>Custom Resolution of package %s in var %s found at %s@}@."
            (Bsb_pkg_types.to_string pkg)
            custom_pkg_loc
            path;
          path
        end
    end
  else
    resolve_bs_package ~cwd pkg
  end
