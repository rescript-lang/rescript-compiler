(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

open Misc
open Cmx_format

type error =
    Illegal_renaming of string * string * string
  | Forward_reference of string * string
  | Wrong_for_pack of string * string
  | Linking_error
  | Assembler_error of string
  | File_not_found of string


exception Error of error

(* Read the unit information from a .cmx file. *)

type pack_member_kind = PM_intf | PM_impl of unit_infos

type pack_member =
  { pm_file: string;
    pm_name: string;
    pm_kind: pack_member_kind }

let read_member_info pack_path file = (
  let name =
    String.capitalize(Filename.basename(chop_extensions file)) in
  let kind =
    if Filename.check_suffix file ".cmx" then begin
      let (info, crc) = Compilenv.read_unit_info file in
      if info.ui_name <> name
      then raise(Error(Illegal_renaming(name, file, info.ui_name)));
      if info.ui_symbol <>
         (Compilenv.current_unit_infos()).ui_symbol ^ "__" ^ info.ui_name
      then raise(Error(Wrong_for_pack(file, pack_path)));
      Asmlink.check_consistency file info crc;
      Compilenv.cache_unit_info info;
      PM_impl info
    end else
      PM_intf in
  { pm_file = file; pm_name = name; pm_kind = kind }
)

(* Check absence of forward references *)

let check_units members =
  let rec check forbidden = function
    [] -> ()
  | mb :: tl ->
      begin match mb.pm_kind with
      | PM_intf -> ()
      | PM_impl infos ->
          List.iter
            (fun (unit, _) ->
              if List.mem unit forbidden
              then raise(Error(Forward_reference(mb.pm_file, unit))))
            infos.ui_imports_cmx
      end;
      check (list_remove mb.pm_name forbidden) tl in
  check (List.map (fun mb -> mb.pm_name) members) members

(* Make the .o file for the package *)

let make_package_object ppf members targetobj targetname coercion =
  let objtemp =
    if !Clflags.keep_asm_file
    then chop_extension_if_any targetobj ^ ".pack" ^ Config.ext_obj
    else
      (* Put the full name of the module in the temporary file name
         to avoid collisions with MSVC's link /lib in case of successive
         packs *)
      Filename.temp_file (Compilenv.make_symbol (Some "")) Config.ext_obj in
  let components =
    List.map
      (fun m ->
        match m.pm_kind with
        | PM_intf -> None
        | PM_impl _ -> Some(Ident.create_persistent m.pm_name))
      members in
  Asmgen.compile_implementation
    (chop_extension_if_any objtemp) ppf
    (Translmod.transl_store_package
       components (Ident.create_persistent targetname) coercion);
  let objfiles =
    List.map
      (fun m -> chop_extension_if_any m.pm_file ^ Config.ext_obj)
      (List.filter (fun m -> m.pm_kind <> PM_intf) members) in
  let ok =
    Ccomp.call_linker Ccomp.Partial targetobj (objtemp :: objfiles) ""
  in
  remove_file objtemp;
  if not ok then raise(Error Linking_error)

(* Make the .cmx file for the package *)

let build_package_cmx members cmxfile =
  let unit_names =
    List.map (fun m -> m.pm_name) members in
  let filter lst =
    List.filter (fun (name, crc) -> not (List.mem name unit_names)) lst in
  let union lst =
    List.fold_left
      (List.fold_left
          (fun accu n -> if List.mem n accu then accu else n :: accu))
      [] lst in
  let units =
    List.fold_right
      (fun m accu ->
        match m.pm_kind with PM_intf -> accu | PM_impl info -> info :: accu)
      members [] in
  let ui = Compilenv.current_unit_infos() in
  let pkg_infos =
    { ui_name = ui.ui_name;
      ui_symbol = ui.ui_symbol;
      ui_defines =
          List.flatten (List.map (fun info -> info.ui_defines) units) @
          [ui.ui_symbol];
      ui_imports_cmi =
          (ui.ui_name, Some (Env.crc_of_unit ui.ui_name)) ::
          filter(Asmlink.extract_crc_interfaces());
      ui_imports_cmx =
          filter(Asmlink.extract_crc_implementations());
      ui_approx = ui.ui_approx;
      ui_curry_fun =
          union(List.map (fun info -> info.ui_curry_fun) units);
      ui_apply_fun =
          union(List.map (fun info -> info.ui_apply_fun) units);
      ui_send_fun =
          union(List.map (fun info -> info.ui_send_fun) units);
      ui_force_link =
          List.exists (fun info -> info.ui_force_link) units;
    } in
  Compilenv.write_unit_info pkg_infos cmxfile

(* Make the .cmx and the .o for the package *)

let package_object_files ppf files targetcmx
                         targetobj targetname coercion =
  let pack_path =
    match !Clflags.for_package with
    | None -> targetname
    | Some p -> p ^ "." ^ targetname in
  let members = map_left_right (read_member_info pack_path) files in
  check_units members;
  make_package_object ppf members targetobj targetname coercion;
  build_package_cmx members targetcmx

(* The entry point *)

let package_files ppf initial_env files targetcmx =
  let files =
    List.map
      (fun f ->
        try find_in_path !Config.load_path f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = chop_extensions targetcmx in
  let targetcmi = prefix ^ ".cmi" in
  let targetobj = chop_extension_if_any targetcmx ^ Config.ext_obj in
  let targetname = String.capitalize(Filename.basename prefix) in
  (* Set the name of the current "input" *)
  Location.input_name := targetcmx;
  (* Set the name of the current compunit *)
  Compilenv.reset ?packname:!Clflags.for_package targetname;
  try
    let coercion =
      Typemod.package_units initial_env files targetcmi targetname in
    package_object_files ppf files targetcmx targetobj targetname coercion
  with x ->
    remove_file targetcmx; remove_file targetobj;
    raise x

(* Error report *)

open Format

let report_error ppf = function
    Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %s when %s was expected"
        Location.print_filename file name id
  | Forward_reference(file, ident) ->
      fprintf ppf "Forward reference to %s in file %a" ident
        Location.print_filename file
  | Wrong_for_pack(file, path) ->
      fprintf ppf "File %a@ was not compiled with the `-for-pack %s' option"
              Location.print_filename file path
  | File_not_found file ->
      fprintf ppf "File %s not found" file
  | Assembler_error file ->
      fprintf ppf "Error while assembling %s" file
  | Linking_error ->
      fprintf ppf "Error during partial linking"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
