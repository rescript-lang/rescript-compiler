(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Compenv
open Parsetree
module StringMap = Depend.StringMap

let ppf = Format.err_formatter
(* Print the dependencies *)

type file_kind = ML | MLI;;

let load_path = ref ([] : (string * string array) list)
let ml_synonyms = ref [".ml"]
let mli_synonyms = ref [".mli"]
let shared = ref false
let native_only = ref false
let bytecode_only = ref false
let error_occurred = ref false
let raw_dependencies = ref false
let sort_files = ref false
let all_dependencies = ref false
let one_line = ref false
let files =
  ref ([] : (string * file_kind * Depend.StringSet.t * string list) list)
let allow_approximation = ref false
let map_files = ref []
let module_map = ref StringMap.empty
let debug = ref false

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
  let uname = String.uncapitalize_ascii name in
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
    let cmx_file = basename ^ cmx_suffix in
    let ml_exists =
      List.exists (fun ext -> Sys.file_exists (basename ^ ext)) !ml_synonyms in
    let new_opt_dep =
      if !all_dependencies then
        match target_kind with
        | MLI -> [ cmi_file ]
        | ML  ->
          cmi_file :: (if ml_exists then [ cmx_file ] else [])
      else
        (* this is a make-specific hack that makes .cmx to be a 'proxy'
           target that would force the dependency on .cmi via transitivity *)
        if ml_exists
        then [ cmx_file ]
        else [ cmi_file ]
    in
    ( cmi_file :: byt_deps, new_opt_dep @ opt_deps)
  with Not_found ->
  try
    (* "just .ml" case *)
    let candidates = List.map ((^) modname) !ml_synonyms in
    let filename = find_file_in_list candidates in
    let basename = Filename.chop_extension filename in
    let cmi_file = basename ^ ".cmi" in
    let cmx_file = basename ^ cmx_suffix in
    let bytenames =
      if !all_dependencies then
        match target_kind with
        | MLI -> [ cmi_file ]
        | ML  -> [ cmi_file ]
      else
        (* again, make-specific hack *)
        [basename ^ (if !native_only then cmx_suffix else ".cmo")] in
    let optnames =
      if !all_dependencies
      then match target_kind with
        | MLI -> [ cmi_file ]
        | ML  -> [ cmi_file; cmx_file ]
      else [ cmx_file ]
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
       (* filter out "*predef*" *)
      if (String.length dep > 0)
          && (match dep.[0] with
              | 'A'..'Z' | '\128'..'\255' -> true
              | _ -> false) then
        begin
          print_char ' ';
          print_string dep
        end)
    deps;
  print_char '\n'


(* Process one file *)

let report_err exn =
  error_occurred := true;
  Location.report_exception Format.err_formatter exn

let tool_name = "ocamldep"

let rec lexical_approximation lexbuf =
  (* Approximation when a file can't be parsed.
     Heuristic:
     - first component of any path starting with an uppercase character is a
       dependency.
     - always skip the token after a dot, unless dot is preceded by a
       lower-case identifier
     - always skip the token after a backquote
  *)
  try
    let rec process after_lident lexbuf =
      match Lexer.token lexbuf with
      | Parser.UIDENT name ->
          Depend.free_structure_names :=
            Depend.StringSet.add name !Depend.free_structure_names;
          process false lexbuf
      | Parser.LIDENT _ -> process true lexbuf
      | Parser.DOT when after_lident -> process false lexbuf
      | Parser.DOT | Parser.BACKQUOTE -> skip_one lexbuf
      | Parser.EOF -> ()
      | _ -> process false lexbuf
    and skip_one lexbuf =
      match Lexer.token lexbuf with
      | Parser.DOT | Parser.BACKQUOTE -> skip_one lexbuf
      | Parser.EOF -> ()
      | _ -> process false lexbuf

    in
    process false lexbuf
  with Lexer.Error _ -> lexical_approximation lexbuf

let read_and_approximate inputfile =
  error_occurred := false;
  Depend.free_structure_names := Depend.StringSet.empty;
  let ic = open_in_bin inputfile in
  try
    seek_in ic 0;
    Location.input_name := inputfile;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf inputfile;
    lexical_approximation lexbuf;
    close_in ic;
    !Depend.free_structure_names
  with exn ->
    close_in ic;
    report_err exn;
    !Depend.free_structure_names

let read_parse_and_extract parse_function extract_function def ast_kind
    source_file =
  Depend.pp_deps := [];
  Depend.free_structure_names := Depend.StringSet.empty;
  try
    let input_file = Pparse.preprocess source_file in
    begin try
      let ast =
        Pparse.file ~tool_name Format.err_formatter
                    input_file parse_function ast_kind
      in
      let bound_vars =
        List.fold_left
          (fun bv modname ->
            Depend.open_module bv (Longident.parse modname))
          !module_map ((* PR#7248 *) List.rev !Clflags.open_modules)
      in
      let r = extract_function bound_vars ast in
      Pparse.remove_preprocessed input_file;
      (!Depend.free_structure_names, r)
    with x ->
      Pparse.remove_preprocessed input_file;
      raise x
    end
  with x -> begin
    report_err x;
    if not !allow_approximation
    then (Depend.StringSet.empty, def)
    else (read_and_approximate source_file, def)
  end

let print_ml_dependencies source_file extracted_deps pp_deps =
  let basename = Filename.chop_extension source_file in
  let byte_targets = [ basename ^ ".cmo" ] in
  let native_targets =
    if !all_dependencies
    then [ basename ^ cmx_suffix; basename ^ ".o" ]
    else [ basename ^ cmx_suffix ] in
  let shared_targets = [ basename ^ ".cmxs" ] in
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
    print_dependencies (byte_targets @ extra_targets) (byt_deps @ pp_deps);
#end
  if not !bytecode_only then
    begin
      print_dependencies (native_targets @ extra_targets)
        (native_deps @ pp_deps);
      if !shared then
        print_dependencies (shared_targets @ extra_targets)
          (native_deps @ pp_deps)
    end

let print_mli_dependencies source_file extracted_deps pp_deps =
  let basename = Filename.chop_extension source_file in
  let (byt_deps, _opt_deps) =
    Depend.StringSet.fold (find_dependency MLI)
      extracted_deps ([], []) in
  print_dependencies [basename ^ ".cmi"] (byt_deps @ pp_deps)

let print_file_dependencies (source_file, kind, extracted_deps, pp_deps) =
  if !raw_dependencies then begin
    print_raw_dependencies source_file extracted_deps
  end else
    match kind with
    | ML -> print_ml_dependencies source_file extracted_deps pp_deps
    | MLI -> print_mli_dependencies source_file extracted_deps pp_deps


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
  let (extracted_deps, ()) =
    read_parse_and_extract parse_use_file_as_impl Depend.add_implementation ()
                           Pparse.Structure source_file
  in
  files := (source_file, ML, extracted_deps, !Depend.pp_deps) :: !files

let mli_file_dependencies source_file =
  let (extracted_deps, ()) =
    read_parse_and_extract
#if defined BS_OCAMLDEP then
      (fun lexbuf -> !Ppx_entry.rewrite_signature (Parse.interface lexbuf) )
#else
      Parse.interface
#end
      Depend.add_signature
      ()
      Pparse.Signature source_file
  in
  files := (source_file, MLI, extracted_deps, !Depend.pp_deps) :: !files

let process_file_as process_fun def source_file =
  Compenv.readenv ppf (Before_compile source_file);
  load_path := [];
  List.iter add_to_load_path (
      (!Compenv.last_include_dirs @
       !Clflags.include_dirs @
       !Compenv.first_include_dirs
      ));
  Location.input_name := source_file;
  try
    if Sys.file_exists source_file then process_fun source_file else def
  with x -> report_err x; def

let process_file source_file ~ml_file ~mli_file ~def =
  if List.exists (Filename.check_suffix source_file) !ml_synonyms then
    process_file_as ml_file def source_file
  else if List.exists (Filename.check_suffix source_file) !mli_synonyms then
    process_file_as mli_file def source_file
  else def

let file_dependencies source_file =
  process_file source_file ~def:()
    ~ml_file:ml_file_dependencies
    ~mli_file:mli_file_dependencies

let file_dependencies_as kind =
  match kind with
  | ML -> process_file_as ml_file_dependencies ()
  | MLI -> process_file_as mli_file_dependencies ()

let sort_files_by_dependencies files =
  let h = Hashtbl.create 31 in
  let worklist = ref [] in

(* Init Hashtbl with all defined modules *)
  let files = List.map (fun (file, file_kind, deps, pp_deps) ->
    let modname =
      String.capitalize_ascii (Filename.chop_extension (Filename.basename file))
    in
    let key = (modname, file_kind) in
    let new_deps = ref [] in
    Hashtbl.add h key (file, new_deps);
    worklist := key :: !worklist;
    (modname, file_kind, deps, new_deps, pp_deps)
  ) files in

(* Keep only dependencies to defined modules *)
  List.iter (fun (modname, file_kind, deps, new_deps, _pp_deps) ->
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
    let sorted_deps =
      let li = ref [] in
      Hashtbl.iter (fun _ file_deps -> li := file_deps :: !li) h;
      List.sort (fun (file1, _) (file2, _) -> String.compare file1 file2) !li
    in
    List.iter (fun (file, deps) ->
      Format.fprintf Format.err_formatter "\t@[%s: " file;
      List.iter (fun (modname, kind) ->
        Format.fprintf Format.err_formatter "%s.%s " modname
          (if kind=ML then "ml" else "mli");
      ) !deps;
      Format.fprintf Format.err_formatter "@]@.";
      Printf.printf "%s " file) sorted_deps;
  end;
  Printf.printf "\n%!";
  ()

(* Map *)

let rec dump_map s0 ppf m =
  let open Depend in
  StringMap.iter
    (fun key (Node(s1,m')) ->
      let s = StringSet.diff s1 s0 in
      if StringSet.is_empty s then
        Format.fprintf ppf "@ @[<hv2>module %s : sig%a@;<1 -2>end@]"
          key (dump_map (StringSet.union s1 s0)) m'
      else
        Format.fprintf ppf "@ module %s = %s" key (StringSet.choose s))
    m

let process_ml_map =
  read_parse_and_extract Parse.implementation Depend.add_implementation_binding
                         StringMap.empty Pparse.Structure

let process_mli_map =
  read_parse_and_extract Parse.interface Depend.add_signature_binding
                         StringMap.empty Pparse.Signature

let parse_map fname =
  map_files := fname :: !map_files ;
  let old_transp = !Clflags.transparent_modules in
  Clflags.transparent_modules := true;
  let (deps, m) =
    process_file fname ~def:(Depend.StringSet.empty, StringMap.empty)
      ~ml_file:process_ml_map
      ~mli_file:process_mli_map
  in
  Clflags.transparent_modules := old_transp;
  let modname =
    String.capitalize_ascii
      (Filename.basename (Filename.chop_extension fname)) in
  if StringMap.is_empty m then
    report_err (Failure (fname ^ " : empty map file or parse error"));
  let mm = Depend.make_node m in
  if !debug then begin
    Format.printf "@[<v>%s:%t%a@]@." fname
      (fun ppf -> Depend.StringSet.iter (Format.fprintf ppf " %s") deps)
      (dump_map deps) (StringMap.add modname mm StringMap.empty)
  end;
  let mm = Depend.(weaken_map (StringSet.singleton modname) mm) in
  module_map := StringMap.add modname mm !module_map
;;


(* Entry point *)

let print_version () =
  Format.printf "ocamldep, version %s@." Sys.ocaml_version;
  exit 0;
;;

let print_version_num () =
  Format.printf "%s@." Sys.ocaml_version;
  exit 0;
;;

let main () =
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
  add_to_list first_include_dirs Filename.current_dir_name;
  Compenv.readenv ppf Before_args;
  Clflags.reset_arguments (); (* reset arguments from ocamlc/ocamlopt *)
  Clflags.add_arguments __LOC__ [
     "-absname", Arg.Set Location.absname,
        " Show absolute filenames in error messages";
     "-all", Arg.Set all_dependencies,
        " Generate dependencies on all files";
     "-allow-approx", Arg.Set allow_approximation,
        " Fallback to a lexer-based approximation on unparseable files";
     "-as-map", Arg.Set Clflags.transparent_modules,
      " Omit delayed dependencies for module aliases (-no-alias-deps -w -49)";
      (* "compiler uses -no-alias-deps, and no module is coerced"; *)
     "-debug-map", Arg.Set debug,
        " Dump the delayed dependency map for each map file";
     "-I", Arg.String (add_to_list Clflags.include_dirs),
        "<dir>  Add <dir> to the list of include directories";
     "-impl", Arg.String (file_dependencies_as ML),
        "<f>  Process <f> as a .ml file";
     "-intf", Arg.String (file_dependencies_as MLI),
        "<f>  Process <f> as a .mli file";
     "-map", Arg.String parse_map,
        "<f>  Read <f> and propagate delayed dependencies to following files";
     "-ml-synonym", Arg.String(add_to_synonym_list ml_synonyms),
        "<e>  Consider <e> as a synonym of the .ml extension";
     "-mli-synonym", Arg.String(add_to_synonym_list mli_synonyms),
        "<e>  Consider <e> as a synonym of the .mli extension";
     "-modules", Arg.Set raw_dependencies,
        " Print module dependencies in raw form (not suitable for make)";
     "-native", Arg.Set native_only,
        " Generate dependencies for native-code only (no .cmo files)";
     "-bytecode", Arg.Set bytecode_only,
        " Generate dependencies for bytecode-code only (no .cmx files)";
     "-one-line", Arg.Set one_line,
        " Output one line per file, regardless of the length";
     "-open", Arg.String (add_to_list Clflags.open_modules),
        "<module>  Opens the module <module> before typing";
     "-plugin", Arg.String Compplugin.load,
         "<plugin>  Load dynamic plugin <plugin>";
     "-pp", Arg.String(fun s -> Clflags.preprocessor := Some s),
         "<cmd>  Pipe sources through preprocessor <cmd>";
     "-ppx", Arg.String (add_to_list first_ppx),
         "<cmd>  Pipe abstract syntax trees through preprocessor <cmd>";
     "-shared", Arg.Set shared,
         " Generate dependencies for native plugin files (.cmxs targets)";
     "-slash", Arg.Set Clflags.force_slash,
         " (Windows) Use forward slash / instead of backslash \\ in file paths";
     "-sort", Arg.Set sort_files,
        " Sort files according to their dependencies";
     "-version", Arg.Unit print_version,
         " Print version and exit";
     "-vnum", Arg.Unit print_version_num,
         " Print version number and exit";
     "-args", Arg.Expand Arg.read_arg,
         "<file> Read additional newline separated command line arguments \n\
         \      from <file>";
     "-args0", Arg.Expand Arg.read_arg0,
         "<file> Read additional NUL separated command line arguments from \n\
         \      <file>"
  ];
  let usage =
    Printf.sprintf "Usage: %s [options] <source files>\nOptions are:"
                   (Filename.basename Sys.argv.(0))
  in
  Clflags.parse_arguments file_dependencies usage;
  Compenv.readenv ppf Before_link;
  if !sort_files then sort_files_by_dependencies !files
  else List.iter print_file_dependencies (List.sort compare !files);
  exit (if !error_occurred then 2 else 0)

let main_from_option () =
  if Sys.argv.(1) <> "-depend" then begin
    Printf.eprintf
      "Fatal error: argument -depend must be used as first argument.\n%!";
    exit 2;
  end;
  incr Arg.current;
  Sys.argv.(0) <- Sys.argv.(0) ^ " -depend";
  Sys.argv.(!Arg.current) <- Sys.argv.(0);
  main ()
