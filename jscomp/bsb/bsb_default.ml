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



(* 6
      Label omitted in function application.
      7
      Method overridden.
      9
      Missing fields in a record pattern. (*Not always desired, in some cases need [@@@warning "+9"] *)      
      27
      Innocuous unused variable: unused variable that is not bound with let nor as, and doesn’t start with an underscore (_) character.      
      29
      Unescaped end-of-line in a string constant (non-portable code).
      32 .. 39 Unused  blabla
      44
      Open statement shadows an already defined identifier.
      45
      Open statement shadows an already defined label or constructor.
      48
      Implicit elimination of optional arguments.
      https://caml.inria.fr/mantis/view.php?id=6352

*)  
(** *)
let bsc_flags = ref [
    "-color"; "always" ;
    "-w"; "-40+6+7+27+32..39+44+45"

  ]
let get_bsc_flags () = !bsc_flags
let set_bsc_flags s = 
  bsc_flags := Bsb_build_util.get_list_string_acc s !bsc_flags


let bs_dependencies : Bsb_config_types.bs_dependency list ref = ref []
let get_bs_dependencies () = !bs_dependencies

let resolve_package cwd  package_name = 

  match Bs_pkg.resolve_bs_package ~cwd package_name  with 
  | None -> 
    Bsb_exception.error (Package_not_found (package_name,None))
  | Some x -> 
    {
      Bsb_config_types.package_name ;
      package_install_path = x // Bsb_config.lib_ocaml
    }

(** When we plan to add more deps here,
  Make sure check it is consistent that for nested deps, we have a 
  quck check by just re-parsing deps 
  Make sure it works with [-make-world] [-clean-world]
 *)
let set_bs_dependencies ~cwd s =
  let package_names =
    Bsb_build_util.get_list_string s  
  in 
  bs_dependencies := 
    package_names 
    |> List.map (resolve_package cwd )


let bs_external_includes = ref []

(** we should not resolve it too early,
  since it is external configuration
*)
let set_bs_external_includes s =
  bs_external_includes := get_list_string s 
  (* List.map Bsb_build_util.convert_and_resolve_path *) 
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




let use_stdlib = ref true
let get_use_stdlib () = !use_stdlib

let built_in_package = ref None 
let set_use_stdlib ~cwd b = 
  use_stdlib := b ;
  if b then 
    built_in_package := Some (resolve_package cwd Bs_version.package_name )




let generate_merlin = ref true

let get_generate_merlin () = !generate_merlin

let set_generate_merlin b =
  generate_merlin := b
