(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
open My_std
open Format
open Log
open Pathname.Operators
open Tools
open Command
open Rule
open Tags.Operators
open Ocaml_utils
open Rule.Common_commands
open Outcome

let forpack_flags arg tags =
  if Tags.mem "pack" tags then
    Ocaml_arch.forpack_flags_of_pathname arg
  else N

let ocamlc_c tags arg out =
  let tags = tags++"ocaml"++"byte" in
  Cmd (S [!Options.ocamlc; A"-c"; T(tags++"compile");
          ocaml_ppflags tags; ocaml_include_flags arg; A"-o"; Px out; P arg])

let ocamlc_link flag tags deps out =
  Cmd (S [!Options.ocamlc; flag; T tags;
          atomize_paths deps; A"-o"; Px out])

let ocamlc_link_lib = ocamlc_link (A"-a")
let ocamlc_link_prog = ocamlc_link N

let ocamlmklib tags deps out =
  Cmd (S [!Options.ocamlmklib; T tags;
          atomize_paths deps; A"-o"; Px (Pathname.remove_extensions out)])

let ocamlmktop tags deps out =
  Cmd( S [!Options.ocamlmktop; T (tags++"mktop");
          atomize_paths deps; A"-o"; Px out])

let byte_lib_linker tags =
  if Tags.mem "ocamlmklib" tags then
    ocamlmklib tags
  else
    ocamlc_link_lib tags

let byte_lib_linker_tags tags = tags++"ocaml"++"link"++"byte"++"library"

let ocamlc_p tags deps out =
  Cmd (S [!Options.ocamlc; A"-pack"; T tags;
          atomize_paths deps; A"-o"; Px out])

let ocamlopt_c tags arg out =
  let tags = tags++"ocaml"++"native" in
  Cmd (S [!Options.ocamlopt; A"-c"; Ocaml_arch.forpack_flags_of_pathname arg;
          T(tags++"compile"); ocaml_ppflags tags; ocaml_include_flags arg;
          A"-o"; Px out (* FIXME ocamlopt bug -o cannot be after the input file *); P arg])

let ocamlopt_link flag tags deps out =
  Cmd (S [!Options.ocamlopt; flag; forpack_flags out tags; T tags;
          atomize_paths deps; A"-o"; Px out])

let ocamlopt_link_lib = ocamlopt_link (A"-a")
let ocamlopt_link_shared_lib = ocamlopt_link (A"-shared")
let ocamlopt_link_prog = ocamlopt_link N

let ocamlopt_p tags deps out =
  let dirnames = List.union [] (List.map Pathname.dirname deps) in
  let include_flags = List.fold_right ocaml_add_include_flag dirnames [] in
  let mli = Pathname.update_extensions "mli" out in
  let cmd =
    S [!Options.ocamlopt; A"-pack"; forpack_flags out tags; T tags;
       S include_flags; atomize_paths deps;
       A"-o"; Px out] in
  if (*FIXME true ||*) Pathname.exists mli then Cmd cmd
  else
    let rm = S[A"rm"; A"-f"; P mli] in
    Cmd(S[A"touch"; P mli; Sh" ; if "; cmd; Sh" ; then "; rm; Sh" ; else ";
          rm; Sh" ; exit 1; fi"])

let native_lib_linker tags =
  if Tags.mem "ocamlmklib" tags then
    ocamlmklib tags
  else
    ocamlopt_link_lib tags

let native_shared_lib_linker tags =
(* ocamlmklib seems to not support -shared, is this OK?
  if Tags.mem "ocamlmklib" tags then
    ocamlmklib tags
  else
*)
    ocamlopt_link_shared_lib tags

let native_lib_linker_tags tags = tags++"ocaml"++"link"++"native"++"library"


let prepare_compile build ml =
  let dir = Pathname.dirname ml in
  let include_dirs = Pathname.include_dirs_of dir in
  let modules = path_dependencies_of ml in
  let results =
    build (List.map (fun (_, x) -> expand_module include_dirs x ["cmi"]) modules) in
  List.iter2 begin fun (mandatory, name) res ->
    match mandatory, res with
    | _, Good _ -> ()
    | `mandatory, Bad exn ->
        if not !Options.ignore_auto then raise exn;
        dprintf 3
          "Warning: Failed to build the module %s requested by ocamldep."
          name;
        if not (!Options.recursive || Options.ocamlbuild_project_heuristic ())
        then Log.at_failure ~name:"a module failed to build,
           while recursive traversal was disabled by fragile heuristic;
           hint that having a _tags or myocamlbuild.ml would maybe solve
           the build error"
          (fun `Error ->
            eprintf "Hint:@ Recursive@ traversal@ of@ subdirectories@ \
              was@ not@ enabled@ for@ this@ build,@ as@ the@ working@ \
              directory does@ not@ look@ like@ an@ ocamlbuild@ project@ \
              (no@ '_tags'@ or@ 'myocamlbuild.ml'@ file).@ \
              If@ you@ have@ modules@ in@ subdirectories,@ you@ should@ add@ \
              the@ option@ \"-r\"@ or@ create@ an@ empty@ '_tags'@ file.@\n\
              @\n\
              To@ enable@ recursive@ traversal@ for@ some@ subdirectories@ \
              only,@ you@ can@ use@ the@ following@ '_tags'@ file:@\n\
              @[<v 4>@,\
                true: -traverse@,\
                <dir1> or <dir2>: traverse@,\
              @]"
          );
    | `just_try, Bad _ -> ()
  end modules results

let byte_compile_ocaml_interf mli cmi env build =
  let mli = env mli and cmi = env cmi in
  prepare_compile build mli;
  ocamlc_c (tags_of_pathname mli++"interf") mli cmi

(* given that .cmi can be built from either ocamlc and ocamlopt, this
   "agnostic" rule chooses either compilers depending on whether the
   "native" tag is present. This was requested during PR#4613 as way
   to enable using ocamlbuild in environments where only ocamlopt is
   available, not ocamlc. *)
let compile_ocaml_interf mli cmi env build =
  let mli = env mli and cmi = env cmi in
  prepare_compile build mli;
  let tags = tags_of_pathname mli++"interf" in
  let comp_c = if Tags.mem "native" tags then ocamlopt_c else ocamlc_c in
  comp_c tags mli cmi

let byte_compile_ocaml_implem ?tag ml cmo env build =
  let ml = env ml and cmo = env cmo in
  prepare_compile build ml;
  ocamlc_c (Tags.union (tags_of_pathname ml) (tags_of_pathname cmo)++"implem"+++tag) ml cmo

let cache_prepare_link = Hashtbl.create 107
let rec prepare_link tag cmx extensions build =
  let key = (tag, cmx, extensions) in
  let dir = Pathname.dirname cmx in
  let include_dirs = Pathname.include_dirs_of dir in
  let ml = Pathname.update_extensions "ml" cmx in
  let mli = Pathname.update_extensions "mli" cmx in
  let modules =
    List.union
      (if Pathname.exists (ml-.-"depends") then path_dependencies_of ml else [])
      (if Pathname.exists (mli-.-"depends") then path_dependencies_of mli else [])
  in
  let modules =
    if (modules = []) && (Pathname.exists (ml^"pack")) then
      List.map (fun s -> (`mandatory, s)) (string_list_of_file (ml^"pack"))
    else
      modules
  in
  if modules <> [] && not (Hashtbl.mem cache_prepare_link key) then
    let () = Hashtbl.add cache_prepare_link key true in
    let modules' = List.map (fun (_, x) -> expand_module include_dirs x extensions) modules in
    List.iter2 begin fun (mandatory, _) result ->
      match mandatory, result with
      | _, Good p -> prepare_link tag p extensions build
      | `mandatory, Bad exn -> if not !Options.ignore_auto then raise exn
      | `just_try, Bad _ -> ()
    end modules (build modules')

let native_compile_ocaml_implem ?tag ?(cmx_ext="cmx") ml env build =
  let ml = env ml in
  let cmi = Pathname.update_extensions "cmi" ml in
  let cmx = Pathname.update_extensions cmx_ext ml in
  prepare_link cmx cmi [cmx_ext; "cmi"] build;
  ocamlopt_c (Tags.union (tags_of_pathname ml) (tags_of_pathname cmx)++"implem"+++tag) ml cmx

let libs_of_use_lib tags =
  Tags.fold begin fun tag acc ->
    try let libpath, extern = Hashtbl.find info_libraries tag in
        if extern then acc else libpath :: acc
    with Not_found -> acc
  end tags []

let prepare_libs cma_ext a_ext out build =
  let out_no_ext = Pathname.remove_extension out in
  let libs1 = List.union (libraries_of out_no_ext) (libs_of_use_lib (tags_of_pathname out)) in
  let () = dprintf 10 "prepare_libs: %S -> %a" out pp_l libs1 in
  let libs = List.map (fun x -> x-.-cma_ext) libs1 in
  let libs2 = List.map (fun lib -> [lib-.-a_ext]) libs1 in
  List.iter ignore_good (build libs2); libs

let library_index = Hashtbl.create 32
let package_index = Hashtbl.create 32
let hidden_packages = ref []

let hide_package_contents package = hidden_packages := package :: !hidden_packages

module Ocaml_dependencies_input = struct
  let fold_dependencies = Resource.Cache.fold_dependencies
  let fold_libraries f = Hashtbl.fold f library_index
  let fold_packages f = Hashtbl.fold f package_index
end
module Ocaml_dependencies = Ocaml_dependencies.Make(Ocaml_dependencies_input)

let caml_transitive_closure = Ocaml_dependencies.caml_transitive_closure

let link_one_gen linker tagger cmX out env _build =
  let cmX = env cmX and out = env out in
  let tags = tagger (tags_of_pathname out) in
  linker tags [cmX] out

let link_gen cmX_ext cma_ext a_ext extensions linker tagger cmX out env build =
  let cmX = env cmX and out = env out in
  let tags = tagger (tags_of_pathname out) in
  let dyndeps = Rule.build_deps_of_tags build (tags++"link_with") in
  let cmi = Pathname.update_extensions "cmi" cmX in
  prepare_link cmX cmi extensions build;
  let libs = prepare_libs cma_ext a_ext out build in
  let hidden_packages = List.map (fun x -> x-.-cmX_ext) !hidden_packages in
  let deps =
    caml_transitive_closure
      ~caml_obj_ext:cmX_ext ~caml_lib_ext:cma_ext
      ~used_libraries:libs ~hidden_packages (cmX :: dyndeps) in
  let deps = (List.filter (fun l -> not (List.mem l deps)) libs) @ deps in

  (* Hack to avoid linking twice with the standard library. *)
  let stdlib = "stdlib/stdlib"-.-cma_ext in
  let is_not_stdlib x = x <> stdlib in
  let deps = List.filter is_not_stdlib deps in

  if deps = [] then failwith "Link list cannot be empty";
  let () = dprintf 6 "link: %a -o %a" print_string_list deps Pathname.print out in
  linker (tags++"dont_link_with") deps out

let byte_link_gen = link_gen "cmo" "cma" "cma" ["cmo"; "cmi"]

let byte_link = byte_link_gen ocamlc_link_prog
  (fun tags -> tags++"ocaml"++"link"++"byte"++"program")

let byte_output_obj = byte_link_gen ocamlc_link_prog
  (fun tags -> tags++"ocaml"++"link"++"byte"++"output_obj")

let byte_output_shared = byte_link_gen ocamlc_link_prog
  (fun tags -> tags++"ocaml"++"link"++"byte"++"output_obj"++"output_shared")

let byte_library_link = byte_link_gen byte_lib_linker byte_lib_linker_tags

let byte_debug_link_gen =
  link_gen "d.cmo" "d.cma" "d.cma" ["d.cmo"; "cmi"]

let byte_debug_link = byte_debug_link_gen ocamlc_link_prog
  (fun tags -> tags++"ocaml"++"link"++"byte"++"debug"++"program")

let byte_debug_library_link = byte_debug_link_gen byte_lib_linker
  (fun tags -> byte_lib_linker_tags tags++"debug")

let native_link_gen linker =
  link_gen "cmx" "cmxa" !Options.ext_lib [!Options.ext_obj; "cmi"] linker

let native_link x = native_link_gen ocamlopt_link_prog
  (fun tags -> tags++"ocaml"++"link"++"native"++"program") x

let native_output_obj x = native_link_gen ocamlopt_link_prog
  (fun tags -> tags++"ocaml"++"link"++"native"++"output_obj") x

let native_output_shared x = native_link_gen ocamlopt_link_prog
  (fun tags -> tags++"ocaml"++"link"++"native"++"output_obj"++"output_shared") x

let native_library_link x =
  native_link_gen native_lib_linker native_lib_linker_tags x

let native_profile_link_gen linker =
  link_gen "p.cmx" "p.cmxa" ("p" -.- !Options.ext_lib) ["p" -.- !Options.ext_obj; "cmi"] linker

let native_profile_link x = native_profile_link_gen ocamlopt_link_prog
  (fun tags -> tags++"ocaml"++"link"++"native"++"profile"++"program") x

let native_profile_library_link x = native_profile_link_gen native_lib_linker
  (fun tags -> native_lib_linker_tags tags++"profile") x

let link_units table extensions cmX_ext cma_ext a_ext linker tagger contents_list cmX env build =
  let cmX = env cmX in
  let tags = tagger (tags_of_pathname cmX) in
  let _ = Rule.build_deps_of_tags build tags in
  let dir =
    let dir1 = Pathname.remove_extensions cmX in
    if Resource.exists_in_source_dir dir1 then dir1
    else Pathname.dirname cmX in
  let include_dirs = Pathname.include_dirs_of dir in
  let extension_keys = List.map fst extensions in
  let libs = prepare_libs cma_ext a_ext cmX build in
  let results =
    build begin
      List.map begin fun module_name ->
        expand_module include_dirs module_name extension_keys
      end contents_list
    end in
  let module_paths =
    List.map begin function
      | Good p ->
          let extension_values = List.assoc (Pathname.get_extensions p) extensions in
          List.iter begin fun ext ->
            List.iter ignore_good (build [[Pathname.update_extensions ext p]])
          end extension_values; p
      | Bad exn -> raise exn
    end results in
  Hashtbl.replace table cmX module_paths;
  let hidden_packages = List.map (fun x -> x-.-cmX_ext) !hidden_packages in
  let deps =
    caml_transitive_closure
      ~caml_obj_ext:cmX_ext ~caml_lib_ext:cma_ext
      ~hidden_packages ~pack_mode:true module_paths in
  let full_contents = libs @ module_paths in
  let deps = List.filter (fun x -> List.mem x full_contents) deps in
  let deps = (List.filter (fun l -> not (List.mem l deps)) libs) @ deps in

  (* Hack to avoid linking twice with the standard library. *)
  let stdlib = "stdlib/stdlib"-.-cma_ext in
  let is_not_stdlib x = x <> stdlib in
  let deps = List.filter is_not_stdlib deps in

  linker tags deps cmX

let link_modules = link_units library_index
let pack_modules = link_units package_index

let link_from_file link modules_file cmX env build =
  let modules_file = env modules_file in
  let contents_list = string_list_of_file modules_file in
  link contents_list cmX env build

let byte_library_link_modules =
  link_modules [("cmo",[])] "cmo" "cma" "cma" byte_lib_linker byte_lib_linker_tags

let byte_library_link_mllib = link_from_file byte_library_link_modules

let byte_toplevel_link_modules =
  link_modules [("cmo",[])] "cmo" "cma" "cma" ocamlmktop
               (fun tags -> tags++"ocaml"++"link"++"byte"++"toplevel")

let byte_toplevel_link_mltop = link_from_file byte_toplevel_link_modules

let byte_debug_library_link_modules =
  link_modules [("d.cmo",[])] "d.cmo" "d.cma" "d.cma" byte_lib_linker
    (fun tags -> byte_lib_linker_tags tags++"debug")

let byte_debug_library_link_mllib = link_from_file byte_debug_library_link_modules

let byte_pack_modules =
  pack_modules [("cmo",["cmi"]); ("cmi",[])] "cmo" "cma" "cma" ocamlc_p
    (fun tags -> tags++"ocaml"++"pack"++"byte")

let byte_pack_mlpack = link_from_file byte_pack_modules

let byte_debug_pack_modules =
  pack_modules [("d.cmo",["cmi"]); ("cmi",[])] "d.cmo" "d.cma" "d.cma" ocamlc_p
    (fun tags -> tags++"ocaml"++"pack"++"byte"++"debug")

let byte_debug_pack_mlpack = link_from_file byte_debug_pack_modules

let native_pack_modules x =
  pack_modules [("cmx",["cmi"; !Options.ext_obj]); ("cmi",[])] "cmx" "cmxa" !Options.ext_lib ocamlopt_p
    (fun tags -> tags++"ocaml"++"pack"++"native") x

let native_pack_mlpack = link_from_file native_pack_modules

let native_profile_pack_modules x =
  pack_modules [("p.cmx",["cmi"; "p" -.- !Options.ext_obj]); ("cmi",[])] "p.cmx" "p.cmxa"
    ("p" -.- !Options.ext_lib) ocamlopt_p
    (fun tags -> tags++"ocaml"++"pack"++"native"++"profile") x

let native_profile_pack_mlpack = link_from_file native_profile_pack_modules

let native_library_link_modules x =
  link_modules [("cmx",[!Options.ext_obj])] "cmx" "cmxa"
     !Options.ext_lib native_lib_linker native_lib_linker_tags x

let native_shared_library_link_modules x =
  link_modules [("cmx",[!Options.ext_obj])] "cmx" "cmxa"
     !Options.ext_lib native_shared_lib_linker
     (fun tags -> native_lib_linker_tags tags++"shared") x

let native_library_link_mllib = link_from_file native_library_link_modules

let native_shared_library_link_mldylib = link_from_file native_shared_library_link_modules

let native_shared_library_tags tags basetags =
  List.fold_left (++) (basetags++"ocaml"++"link"++"native"++"shared"++"library") tags

let native_shared_library_link ?(tags = []) x =
  link_one_gen native_shared_lib_linker
    (native_shared_library_tags tags) x

let native_profile_library_link_modules x =
  link_modules [("p.cmx",["p" -.- !Options.ext_obj])] "p.cmx" "p.cmxa"
    ("p" -.- !Options.ext_lib) native_lib_linker
    (fun tags -> native_lib_linker_tags tags++"profile") x

let native_profile_shared_library_link_modules x =
  link_modules [("p.cmx",["p" -.- !Options.ext_obj])] "p.cmx" "p.cmxa"
    ("p" -.- !Options.ext_lib) native_shared_lib_linker
    (fun tags -> native_lib_linker_tags tags++"shared"++"profile") x

let native_profile_library_link_mllib = link_from_file native_profile_library_link_modules

let native_profile_shared_library_link_mldylib = link_from_file native_profile_shared_library_link_modules
