(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Compenv
open Parsetree

let ppf = Format.err_formatter
(* Print the dependencies *)

type file_kind = ML | MLI;;

let load_path = ref ([] : (string * string array) list)
let ml_synonyms = ref [".ml"]
let mli_synonyms = ref [".mli"]
let native_only = ref false
let error_occurred = ref false
let raw_dependencies = ref false
let sort_files = ref false
let all_dependencies = ref false
let one_line = ref false
let files = ref []

let cmx_suffix = 
#if defined BS_OCAMLDEP then
    ".cmj"
#else
    ".cmx"
#end
(* Fix path to use '/' as directory separator instead of '\'.
   Only under Windows. *)

let fix_slash s =
  if Sys.os_type = "Unix" then s else begin
    String.map (function '\\' -> '/' | c -> c) s
  end

(* Since we reinitialize load_path after reading OCAMLCOMP,
  we must use a cache instead of calling Sys.readdir too often. *)
module StringMap = Map.Make(String)
let dirs = ref StringMap.empty
let readdir dir =
  try
    StringMap.find dir !dirs
  with Not_found ->
    let contents =
      try
        Sys.readdir dir
      with Sys_error msg ->
        Format.fprintf Format.err_formatter "@[Bad -I option: %s@]@." msg;
        error_occurred := true;
        [||]
    in
    dirs := StringMap.add dir contents !dirs;
    contents

let add_to_list li s =
  li := s :: !li

let add_to_load_path dir =
  try
    let dir = Misc.expand_directory Config.standard_library dir in
    let contents = readdir dir in
    add_to_list load_path (dir, contents)
  with Sys_error msg ->
    Format.fprintf Format.err_formatter "@[Bad -I option: %s@]@." msg;
    error_occurred := true

let add_to_synonym_list synonyms suffix =
  if (String.length suffix) > 1 && suffix.[0] = '.' then
    add_to_list synonyms suffix
  else begin
    Format.fprintf Format.err_formatter "@[Bad suffix: '%s'@]@." suffix;
    error_occurred := true
  end

(* Find file 'name' (capitalized) in search path *)
let find_file name =
  let uname = String.uncapitalize name in
  let rec find_in_array a pos =
    if pos >= Array.length a then None else begin
      let s = a.(pos) in
      if s = name || s = uname then Some s else find_in_array a (pos + 1)
    end in
  let rec find_in_path = function
    [] -> raise Not_found
  | (dir, contents) :: rem ->
      match find_in_array contents 0 with
        Some truename ->
          if dir = "." then truename else Filename.concat dir truename
      | None -> find_in_path rem in
  find_in_path !load_path

let rec find_file_in_list = function
  [] -> raise Not_found
| x :: rem -> try find_file x with Not_found -> find_file_in_list rem


let find_dependency target_kind modname (byt_deps, opt_deps) =
  try
    let candidates = List.map ((^) modname) !mli_synonyms in
    let filename = find_file_in_list candidates in
    let basename = Filename.chop_extension filename in
    let cmi_file = basename ^ ".cmi" in
    let ml_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !ml_synonyms in
    let new_opt_dep =
      if !all_dependencies then
        match target_kind with
        | MLI -> [ cmi_file ]
        | ML  ->
          cmi_file :: (if ml_exists then [ basename ^ cmx_suffix] else [])
      else
        (* this is a make-specific hack that makes .cmx to be a 'proxy'
           target that would force the dependency on .cmi via transitivity *)
        if ml_exists
        then [ basename ^ cmx_suffix ]
        else [ cmi_file ]
    in
    ( cmi_file :: byt_deps, new_opt_dep @ opt_deps)
  with Not_found ->
  try
    (* "just .ml" case *)
    let candidates = List.map ((^) modname) !ml_synonyms in
    let filename = find_file_in_list candidates in
    let basename = Filename.chop_extension filename in
    let bytenames =
      if !all_dependencies then
        match target_kind with
        | MLI -> [basename ^ ".cmi"]
        | ML  -> [basename ^ ".cmi";]
      else
        (* again, make-specific hack *)
        [basename ^ (if !native_only then cmx_suffix else ".cmo")] in
    let optnames =
      if !all_dependencies
      then match target_kind with
        | MLI -> [basename ^ ".cmi"]
        | ML  -> [basename ^ ".cmi"; basename ^ cmx_suffix]
      else [ basename ^ cmx_suffix ]
    in
    (bytenames @ byt_deps, optnames @  opt_deps)
  with Not_found ->
    (byt_deps, opt_deps)

let (depends_on, escaped_eol) = (":", " \\\n    ")

let print_filename s =
  let s = if !Clflags.force_slash then fix_slash s else s in
  if not (String.contains s ' ') then begin
    print_string s;
  end else begin
    let rec count n i =
      if i >= String.length s then n
      else if s.[i] = ' ' then count (n+1) (i+1)
      else count n (i+1)
    in
    let spaces = count 0 0 in
    let result = Bytes.create (String.length s + spaces) in
    let rec loop i j =
      if i >= String.length s then ()
      else if s.[i] = ' ' then begin
        Bytes.set result j '\\';
        Bytes.set result (j+1) ' ';
        loop (i+1) (j+2);
      end else begin
        Bytes.set result j s.[i];
        loop (i+1) (j+1);
      end
    in
    loop 0 0;
    print_bytes result;
  end
;;

let print_dependencies target_files deps =
  let rec print_items pos = function
    [] -> print_string "\n"
  | dep :: rem ->
    if !one_line || (pos + 1 + String.length dep <= 77) then begin
        if pos <> 0 then print_string " "; print_filename dep;
        print_items (pos + String.length dep + 1) rem
      end else begin
        print_string escaped_eol; print_filename dep;
        print_items (String.length dep + 4) rem
      end in
  print_items 0 (target_files @ [depends_on] @ deps)

let print_raw_dependencies source_file deps =
  print_filename source_file; print_string depends_on;
  Depend.StringSet.iter
    (fun dep ->
      if (String.length dep > 0)
          && (match dep.[0] with 'A'..'Z' -> true | _ -> false) then begin
            print_char ' ';
            print_string dep
          end)
    deps;
  print_char '\n'


(* Process one file *)

let report_err exn =
  error_occurred := true;
  match exn with
    | Sys_error msg ->
        Format.fprintf Format.err_formatter "@[I/O error:@ %s@]@." msg
    | x ->
        match Location.error_of_exn x with
        | Some err ->
            Format.fprintf Format.err_formatter "@[%a@]@."
              Location.report_error err
        | None -> raise x

let tool_name = "ocamldep"

let read_parse_and_extract parse_function extract_function magic source_file =
  Depend.free_structure_names := Depend.StringSet.empty;
  try
    let input_file = Pparse.preprocess source_file in
    begin try
      let ast =
        Pparse.file ~tool_name Format.err_formatter
		    input_file parse_function magic
      in
      let bound_vars = Depend.StringSet.empty in
      List.iter (fun modname ->
	Depend.open_module bound_vars (Longident.Lident modname)
      ) !Clflags.open_modules;
      extract_function bound_vars ast;
      Pparse.remove_preprocessed input_file;
      !Depend.free_structure_names
    with x ->
      Pparse.remove_preprocessed input_file;
      raise x
    end
  with x ->
    report_err x;
    Depend.StringSet.empty

let ml_file_dependencies source_file =
  let parse_use_file_as_impl lexbuf =
    let f x =
      match x with
      | Ptop_def s -> s
      | Ptop_dir _ -> []
    in
    List.flatten (List.map f (Parse.use_file lexbuf))
#if defined BS_OCAMLDEP then
    |> !Ppx_entry.rewrite_implementation
#end
  in
  let extracted_deps =
    read_parse_and_extract parse_use_file_as_impl Depend.add_implementation
                           Config.ast_impl_magic_number source_file
  in
  if !sort_files then
    files := (source_file, ML, !Depend.free_structure_names) :: !files
  else
    if !raw_dependencies then begin
      print_raw_dependencies source_file extracted_deps
    end else begin
      let basename = Filename.chop_extension source_file in
      let byte_targets = [ basename ^ ".cmo" ] in
      let native_targets =
        if !all_dependencies
        then [ basename ^ cmx_suffix; basename ^ ".o" ]
        else [ basename ^ cmx_suffix ] in
      let init_deps = if !all_dependencies then [source_file] else [] in
      let cmi_name = basename ^ ".cmi" in
      let init_deps, extra_targets =
        if List.exists (fun ext -> Sys.file_exists (basename ^ ext))
                       !mli_synonyms
        then (cmi_name :: init_deps, cmi_name :: init_deps), []
        else (init_deps, init_deps),
             (if !all_dependencies then [cmi_name] else [])
      in
      let (byt_deps, native_deps) =
        Depend.StringSet.fold (find_dependency ML)
          extracted_deps init_deps in
#if undefined BS_OCAMLDEP then
     if not !native_only then 
        print_dependencies (byte_targets @ extra_targets) byt_deps;
#end
      print_dependencies (native_targets @ extra_targets) native_deps;
    end

let mli_file_dependencies source_file =
  let extracted_deps =
    read_parse_and_extract 
#if defined BS_OCAMLDEP then
      (fun lexbuf -> !Ppx_entry.rewrite_signature (Parse.interface lexbuf) )
#else
      Parse.interface
#end
      Depend.add_signature
      Config.ast_intf_magic_number source_file
  in
  if !sort_files then
    files := (source_file, MLI, extracted_deps) :: !files
  else
    if !raw_dependencies then begin
      print_raw_dependencies source_file extracted_deps
    end else begin
      let basename = Filename.chop_extension source_file in
      let (byt_deps, _opt_deps) =
        Depend.StringSet.fold (find_dependency MLI)
          extracted_deps ([], []) in
      print_dependencies [basename ^ ".cmi"] byt_deps
    end

let file_dependencies_as kind source_file =
  Compenv.readenv ppf Before_compile;
  load_path := [];
  List.iter add_to_load_path (
      (!Compenv.last_include_dirs @
       !Clflags.include_dirs @
       !Compenv.first_include_dirs
      ));
  Location.input_name := source_file;
  try
    if Sys.file_exists source_file then begin
      match kind with
      | ML -> ml_file_dependencies source_file
      | MLI -> mli_file_dependencies source_file
    end
  with x -> report_err x

let file_dependencies source_file =
  if List.exists (Filename.check_suffix source_file) !ml_synonyms then
    file_dependencies_as ML source_file
  else if List.exists (Filename.check_suffix source_file) !mli_synonyms then
    file_dependencies_as MLI source_file
  else ()

let sort_files_by_dependencies files =
  let h = Hashtbl.create 31 in
  let worklist = ref [] in

(* Init Hashtbl with all defined modules *)
  let files = List.map (fun (file, file_kind, deps) ->
    let modname =
      String.capitalize (Filename.chop_extension (Filename.basename file))
    in
    let key = (modname, file_kind) in
    let new_deps = ref [] in
    Hashtbl.add h key (file, new_deps);
    worklist := key :: !worklist;
    (modname, file_kind, deps, new_deps)
  ) files in

(* Keep only dependencies to defined modules *)
  List.iter (fun (modname, file_kind, deps, new_deps) ->
    let add_dep modname kind =
      new_deps := (modname, kind) :: !new_deps;
    in
    Depend.StringSet.iter (fun modname ->
      match file_kind with
          ML -> (* ML depends both on ML and MLI *)
            if Hashtbl.mem h (modname, MLI) then add_dep modname MLI;
            if Hashtbl.mem h (modname, ML) then add_dep modname ML
        | MLI -> (* MLI depends on MLI if exists, or ML otherwise *)
          if Hashtbl.mem h (modname, MLI) then add_dep modname MLI
          else if Hashtbl.mem h (modname, ML) then add_dep modname ML
    ) deps;
    if file_kind = ML then (* add dep from .ml to .mli *)
      if Hashtbl.mem h (modname, MLI) then add_dep modname MLI
  ) files;

(* Print and remove all files with no remaining dependency. Iterate
   until all files have been removed (worklist is empty) or
   no file was removed during a turn (cycle). *)
  let printed = ref true in
  while !printed && !worklist <> [] do
    let files = !worklist in
    worklist := [];
    printed := false;
    List.iter (fun key ->
      let (file, deps) = Hashtbl.find h key in
      let set = !deps in
      deps := [];
      List.iter (fun key ->
        if Hashtbl.mem h key then deps := key :: !deps
      ) set;
      if !deps = [] then begin
        printed := true;
        Printf.printf "%s " file;
        Hashtbl.remove h key;
      end else
        worklist := key :: !worklist
    ) files
  done;

  if !worklist <> [] then begin
    Format.fprintf Format.err_formatter
      "@[Warning: cycle in dependencies. End of list is not sorted.@]@.";
    Hashtbl.iter (fun _ (file, deps) ->
      Format.fprintf Format.err_formatter "\t@[%s: " file;
      List.iter (fun (modname, kind) ->
        Format.fprintf Format.err_formatter "%s.%s " modname
          (if kind=ML then "ml" else "mli");
      ) !deps;
      Format.fprintf Format.err_formatter "@]@.";
      Printf.printf "%s " file) h;
  end;
  Printf.printf "\n%!";
  ()


(* Entry point *)

let usage = "Usage: ocamldep [options] <source files>\nOptions are:"

let print_version () =
  Format.printf "ocamldep, version %s@." Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  Format.printf "%s@." Sys.ocaml_version;
  exit 0;
;;


let _ =
#if defined BS_OCAMLDEP then
  native_only := true;
  Bs_conditional_initial.setup_env ();
  one_line := true;
#end
  Clflags.classic := false;
#if undefined BS_NO_COMPILER_PATCH then 
  (if not !Clflags.no_implicit_current_dir then
    add_to_list first_include_dirs Filename.current_dir_name);
#else
  add_to_list first_include_dirs Filename.current_dir_name;
#end
  Compenv.readenv ppf Before_args;
  Arg.parse [
     "-absname", Arg.Set Location.absname,
        " Show absolute filenames in error messages";
     "-all", Arg.Set all_dependencies,
        " Generate dependencies on all files";
     "-I", Arg.String (add_to_list Clflags.include_dirs),
        "<dir>  Add <dir> to the list of include directories";
     "-impl", Arg.String (file_dependencies_as ML),
        "<f>  Process <f> as a .ml file";
     "-intf", Arg.String (file_dependencies_as MLI),
        "<f>  Process <f> as a .mli file";
     "-ml-synonym", Arg.String(add_to_synonym_list ml_synonyms),
        "<e>  Consider <e> as a synonym of the .ml extension";
     "-mli-synonym", Arg.String(add_to_synonym_list mli_synonyms),
        "<e>  Consider <e> as a synonym of the .mli extension";
     "-modules", Arg.Set raw_dependencies,
        " Print module dependencies in raw form (not suitable for make)";
     "-native", Arg.Set native_only,
        " Generate dependencies for native-code only (no .cmo files)";
     "-one-line", Arg.Set one_line,
        " Output one line per file, regardless of the length";
     "-open", Arg.String (add_to_list Clflags.open_modules),
        "<module>  Opens the module <module> before typing";
     "-pp", Arg.String(fun s -> Clflags.preprocessor := Some s),
         "<cmd>  Pipe sources through preprocessor <cmd>";
     "-ppx", Arg.String (add_to_list first_ppx),
         "<cmd>  Pipe abstract syntax trees through preprocessor <cmd>";
     "-slash", Arg.Set Clflags.force_slash,
         " (Windows) Use forward slash / instead of backslash \\ in file paths";
     "-sort", Arg.Set sort_files,
        " Sort files according to their dependencies";
     "-version", Arg.Unit print_version,
         " Print version and exit";
     "-vnum", Arg.Unit print_version_num,
         " Print version number and exit";
    ] file_dependencies usage;
  Compenv.readenv ppf Before_link;
  if !sort_files then sort_files_by_dependencies !files;
  exit (if !error_occurred then 2 else 0)
