(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Link a set of .cmx/.o files and produce an executable *)

open Misc
open Config
open Cmx_format
open Compilenv

type error =
    File_not_found of string
  | Not_an_object_file of string
  | Missing_implementations of (string * string list) list
  | Inconsistent_interface of string * string * string
  | Inconsistent_implementation of string * string * string
  | Assembler_error of string
  | Linking_error
  | Multiple_definition of string * string * string
  | Missing_cmx of string * string

exception Error of error

(* Consistency check between interfaces and implementations *)

let crc_interfaces = Consistbl.create ()
let interfaces = ref ([] : string list)
let crc_implementations = Consistbl.create ()
let implementations = ref ([] : string list)
let implementations_defined = ref ([] : (string * string) list)
let cmx_required = ref ([] : string list)

let check_consistency file_name unit crc =
  begin try
    List.iter
      (fun (name, crco) ->
        interfaces := name :: !interfaces;
        match crco with
          None -> ()
        | Some crc ->
            if name = unit.ui_name
            then Consistbl.set crc_interfaces name crc file_name
            else Consistbl.check crc_interfaces name crc file_name)
      unit.ui_imports_cmi
  with Consistbl.Inconsistency(name, user, auth) ->
    raise(Error(Inconsistent_interface(name, user, auth)))
  end;
  begin try
    List.iter
      (fun (name, crco) ->
        implementations := name :: !implementations;
        match crco with
            None ->
              if List.mem name !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Consistbl.check crc_implementations name crc file_name)
      unit.ui_imports_cmx
  with Consistbl.Inconsistency(name, user, auth) ->
    raise(Error(Inconsistent_implementation(name, user, auth)))
  end;
  begin try
    let source = List.assoc unit.ui_name !implementations_defined in
    raise (Error(Multiple_definition(unit.ui_name, file_name, source)))
  with Not_found -> ()
  end;
  implementations := unit.ui_name :: !implementations;
  Consistbl.set crc_implementations unit.ui_name crc file_name;
  implementations_defined :=
    (unit.ui_name, file_name) :: !implementations_defined;
  if unit.ui_symbol <> unit.ui_name then
    cmx_required := unit.ui_name :: !cmx_required

let extract_crc_interfaces () =
  Consistbl.extract !interfaces crc_interfaces
let extract_crc_implementations () =
  Consistbl.extract !implementations crc_implementations

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    let replace_origin = Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin in
    lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts
  end

let runtime_lib () =
  let libname =
    if !Clflags.gprofile
    then "libasmrunp" ^ ext_lib
    else "libasmrun" ^ !Clflags.runtime_variant ^ ext_lib in
  try
    if !Clflags.nopervasives then []
    else [ find_in_path !load_path libname ]
  with Not_found ->
    raise(Error(File_not_found libname))

let object_file_name name =
  let file_name =
    try
      find_in_path !load_path name
    with Not_found ->
      fatal_error "Asmlink.object_file_name: not found" in
  if Filename.check_suffix file_name ".cmx" then
    Filename.chop_suffix file_name ".cmx" ^ ext_obj
  else if Filename.check_suffix file_name ".cmxa" then
    Filename.chop_suffix file_name ".cmxa" ^ ext_lib
  else
    fatal_error "Asmlink.object_file_name: bad ext"

(* First pass: determine which units are needed *)

let missing_globals = (Hashtbl.create 17 : (string, string list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by (name, crc) =
  try
    let rq = Hashtbl.find missing_globals name in
    rq := by :: !rq
  with Not_found ->
    Hashtbl.add missing_globals name (ref [by])

let remove_required name =
  Hashtbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  Hashtbl.iter (fun md rq -> mg := (md, !rq) :: !mg) missing_globals;
  !mg

type file =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let read_file obj_name =
  let file_name =
    try
      find_in_path !load_path obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = read_unit_info file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    let infos =
      try read_library_info file_name
      with Compilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let scan_file obj_name tolink = match read_file obj_name with
  | Unit (file_name,info,crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      remove_required info.ui_name;
      List.iter (add_required file_name) info.ui_imports_cmx;
      (info, file_name, crc) :: tolink
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      List.fold_right
        (fun (info, crc) reqd ->
           if info.ui_force_link
             || !Clflags.link_everything
             || is_required info.ui_name
           then begin
             remove_required info.ui_name;
             List.iter (add_required (Printf.sprintf "%s(%s)"
                                        file_name info.ui_name))
               info.ui_imports_cmx;
             (info, file_name, crc) :: reqd
           end else
             reqd)
        infos.lib_units tolink

(* Second pass: generate the startup file and link it with everything else *)

let make_startup_file ppf filename units_list =
  let compile_phrase p = Asmgen.compile_phrase ppf p in
  let oc = open_out filename in
  Emitaux.output_channel := oc;
  Location.input_name := "caml_startup"; (* set name of "current" input *)
  Compilenv.reset "_startup"; (* set the name of the "current" compunit *)
  Emit.begin_assembly();
  let name_list =
    List.flatten (List.map (fun (info,_,_) -> info.ui_defines) units_list) in
  compile_phrase (Cmmgen.entry_point name_list);
  let units = List.map (fun (info,_,_) -> info) units_list in
  List.iter compile_phrase (Cmmgen.generic_functions false units);
  Array.iteri
    (fun i name -> compile_phrase (Cmmgen.predef_exception i name))
    Runtimedef.builtin_exceptions;
  compile_phrase (Cmmgen.global_table name_list);
  compile_phrase
    (Cmmgen.globals_map
       (List.map
          (fun (unit,_,crc) ->
               let intf_crc =
                 try
                   match List.assoc unit.ui_name unit.ui_imports_cmi with
                     None -> assert false
                   | Some crc -> crc
                 with Not_found -> assert false
               in
                 (unit.ui_name, intf_crc, crc, unit.ui_defines))
          units_list));
  compile_phrase(Cmmgen.data_segment_table ("_startup" :: name_list));
  compile_phrase(Cmmgen.code_segment_table ("_startup" :: name_list));
  compile_phrase
    (Cmmgen.frame_table("_startup" :: "_system" :: name_list));

  Emit.end_assembly();
  close_out oc

let make_shared_startup_file ppf units filename =
  let compile_phrase p = Asmgen.compile_phrase ppf p in
  let oc = open_out filename in
  Emitaux.output_channel := oc;
  Location.input_name := "caml_startup";
  Compilenv.reset "_shared_startup";
  Emit.begin_assembly();
  List.iter compile_phrase
    (Cmmgen.generic_functions true (List.map fst units));
  compile_phrase (Cmmgen.plugin_header units);
  compile_phrase
    (Cmmgen.global_table
       (List.map (fun (ui,_) -> ui.ui_symbol) units));
  (* this is to force a reference to all units, otherwise the linker
     might drop some of them (in case of libraries) *)

  Emit.end_assembly();
  close_out oc


let call_linker_shared file_list output_name =
  if not (Ccomp.call_linker Ccomp.Dll output_name file_list "")
  then raise(Error Linking_error)

let link_shared ppf objfiles output_name =
  let units_tolink = List.fold_right scan_file objfiles [] in
  List.iter
    (fun (info, file_name, crc) -> check_consistency file_name info crc)
    units_tolink;
  Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
  Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
  let objfiles = List.rev (List.map object_file_name objfiles) @
    (List.rev !Clflags.ccobjs) in

  let startup =
    if !Clflags.keep_startup_file
    then output_name ^ ".startup" ^ ext_asm
    else Filename.temp_file "camlstartup" ext_asm in
  make_shared_startup_file ppf
    (List.map (fun (ui,_,crc) -> (ui,crc)) units_tolink) startup;
  let startup_obj = output_name ^ ".startup" ^ ext_obj in
  if Proc.assemble_file startup startup_obj <> 0
  then raise(Error(Assembler_error startup));
  if not !Clflags.keep_startup_file then remove_file startup;
  call_linker_shared (startup_obj :: objfiles) output_name;
  remove_file startup_obj

let call_linker file_list startup_file output_name =
  let main_dll = !Clflags.output_c_object
                 && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object
  in
  let files = startup_file :: (List.rev file_list) in
  let files, c_lib =
    if (not !Clflags.output_c_object) || main_dll || main_obj_runtime then
      files @ (List.rev !Clflags.ccobjs) @ runtime_lib (),
      (if !Clflags.nopervasives || main_obj_runtime then "" else Config.native_c_libraries)
    else
      files, ""
  in
  let mode =
    if main_dll then Ccomp.MainDll
    else if !Clflags.output_c_object then Ccomp.Partial
    else Ccomp.Exe
  in
  if not (Ccomp.call_linker mode output_name files c_lib)
  then raise(Error Linking_error)

(* Main entry point *)

let link ppf objfiles output_name =
  let stdlib =
    if !Clflags.gprofile then "stdlib.p.cmxa" else "stdlib.cmxa" in
  let stdexit =
    if !Clflags.gprofile then "std_exit.p.cmx" else "std_exit.cmx" in
  let objfiles =
    if !Clflags.nopervasives then objfiles
    else if !Clflags.output_c_object then stdlib :: objfiles
    else stdlib :: (objfiles @ [stdexit]) in
  let units_tolink = List.fold_right scan_file objfiles [] in
  Array.iter remove_required Runtimedef.builtin_exceptions;
  begin match extract_missing_globals() with
    [] -> ()
  | mg -> raise(Error(Missing_implementations mg))
  end;
  List.iter
    (fun (info, file_name, crc) -> check_consistency file_name info crc)
    units_tolink;
  Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
  Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                               (* put user's opts first *)
  let startup =
    if !Clflags.keep_startup_file then output_name ^ ".startup" ^ ext_asm
    else Filename.temp_file "camlstartup" ext_asm in
  make_startup_file ppf startup units_tolink;
  let startup_obj = Filename.temp_file "camlstartup" ext_obj in
  if Proc.assemble_file startup startup_obj <> 0 then
    raise(Error(Assembler_error startup));
  try
    call_linker (List.map object_file_name objfiles) startup_obj output_name;
    if not !Clflags.keep_startup_file then remove_file startup;
    remove_file startup_obj
  with x ->
    remove_file startup_obj;
    raise x

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a compilation unit description"
        Location.print_filename name
  | Missing_implementations l ->
     let print_references ppf = function
       | [] -> ()
       | r1 :: rl ->
           fprintf ppf "%s" r1;
           List.iter (fun r -> fprintf ppf ",@ %s" r) rl in
      let print_modules ppf =
        List.iter
         (fun (md, rq) ->
            fprintf ppf "@ @[<hov 2>%s referenced from %a@]" md
            print_references rq) in
      fprintf ppf
       "@[<v 2>No implementations provided for the following modules:%a@]"
       print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %s@]"
       Location.print_filename file1
       Location.print_filename file2
       intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %s@]"
       Location.print_filename file1
       Location.print_filename file2
       intf
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a" Location.print_filename file
  | Linking_error ->
      fprintf ppf "Error during linking"
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %s@]"
        Location.print_filename file1
        Location.print_filename file2
        modname
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the .cmx file@ for module %s,@ \
         which was produced by `ocamlopt -for-pack'.@ \
         Please recompile %a@ with the correct `-I' option@ \
         so that %s.cmx@ is found.@]"
        Location.print_filename filename name
        Location.print_filename  filename
        name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  Consistbl.clear crc_interfaces;
  Consistbl.clear crc_implementations;
  implementations_defined := [];
  cmx_required := [];
  interfaces := [];
  implementations := []
