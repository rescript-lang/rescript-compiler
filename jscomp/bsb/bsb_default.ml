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
let set_ocamllex ~cwd s =
  ocamllex := Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:"ocamllex" s
let get_ocamllex () = !ocamllex


let refmt = ref "refmt"
let get_refmt () = !refmt
let set_refmt ~cwd p =
  refmt := Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:"refmt" p

let refmt_flags = ref ["--print"; "binary"]
let get_refmt_flags () = !refmt_flags
let set_refmt_flags s =
  refmt_flags := get_list_string s


let ppx_flags = ref []
let get_ppx_flags () = !ppx_flags
let set_ppx_flags ~cwd s =
  let s =
    s (* TODO: unix conversion *)
    |> get_list_string
    |> List.map (fun p ->
        if p = "" then failwith "invalid ppx, empty string found"
        else Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:"ppx" p
      ) in
  ppx_flags := s


let js_post_build_cmd = ref None
let get_js_post_build_cmd () = !js_post_build_cmd
let set_js_post_build_cmd ~cwd s =
  js_post_build_cmd := Some (Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:"js-post-build:cmd" s )

let ninja = ref "ninja"
let get_ninja () = !ninja

(* Setting ninja is a bit complex
   First if [build.ninja] does use [ninja] we need set a variable
   Second we need store it so that we can call ninja correctly
*)
let set_ninja ~cwd p  =
  ninja := Bsb_build_util.resolve_bsb_magic_file ~cwd ~desc:"ninja" p



let package_specs = ref (String_set.singleton Literals.commonjs)
(* let package_specs_overriden = ref false *)

let get_package_specs () = !package_specs



let set_package_specs_from_array arr =
    (* if not  !package_specs_overriden then *)
    let new_package_specs =
      arr
      |> get_list_string
      |> List.fold_left (fun acc x ->
          let v =
            if Bsb_config.supported_format x    then String_set.add x acc
            else
              failwith ("Unkonwn package spec" ^ x) in
          v
        ) String_set.empty in
   package_specs := new_package_specs



(*
let internal_override_package_specs str =
  package_specs_overriden := true ;
  let lst = Ext_string.split ~keep_empty:false str ',' in
  package_specs :=
    List.fold_left (fun acc x ->
          let v =
            if Bsb_config.supported_format x then String_set.add x acc
            else
              failwith ("Unkonwn package spec" ^ x) in
          v
    ) String_set.empty lst
*)

let generate_merlin = ref true

let get_generate_merlin () = !generate_merlin

let set_generate_merlin b =
  generate_merlin := b
