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

let get_list_string = Bsb_build_util.get_list_string
let (//) = Ext_filename.combine





let package_name = ref None
let set_package_name s = package_name := Some s
let get_package_name () = !package_name




let bsc_flags = ref []
let get_bsc_flags () = !bsc_flags 
let set_bsc_flags s = bsc_flags := get_list_string s 




let bs_dependencies = ref []
let get_bs_dependencies () = !bs_dependencies
let set_bs_dependencies  s =
  bs_dependencies := get_list_string s 


let bs_external_includes = ref []
let set_bs_external_includes s = 
  bs_external_includes := List.map Bsb_build_util.convert_and_resolve_path (get_list_string s )
let get_bs_external_includes () = !bs_external_includes


let ocamllex =  ref  "ocamllex.opt"
let set_ocamllex s = ocamllex := Bsb_build_util.convert_and_resolve_file s 
let get_ocamllex () = !ocamllex 


let ppx_flags = ref []
let get_ppx_flags () = !ppx_flags
let set_ppx_flags ~cwd s = 
  let s = 
    s (* TODO: unix conversion *)
    |> get_list_string 
    |> List.map (fun x -> 
        if x = "" then failwith "invalid ppx, empty string found"
        else 
        if Filename.is_relative x &&  String.unsafe_get x 0 <> '.' then 
          let name = String.sub x 0 ( String.index x '/') in
          let package = (Bs_pkg.resolve_bs_package ~cwd name ) in
          match package with
          | None ->
            failwith (name ^ "not found when resolving ppx")
          | Some package
            -> Bsb_build_util.convert_and_resolve_path (Filename.dirname package // x) 
        else 
          Bsb_build_util.convert_and_resolve_path x 
      ) in 
  ppx_flags := s
