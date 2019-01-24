(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of a few built-in actions *)

open Actions

(* Miscellaneous functions *)

let env_id env = env

let run_command
    ?(stdin_variable=Builtin_variables.stdin)
    ?(stdout_variable=Builtin_variables.stdout)
    ?(stderr_variable=Builtin_variables.stderr)
    ?(append=false)
    ?(timeout=0)
    log env cmd
  =
  let log_redirection std filename =
    if filename<>"" then
    begin
      Printf.fprintf log "  Redirecting %s to %s \n%!" std filename
    end in
  let lst = List.concat (List.map Testlib.words cmd) in
  let quoted_lst =
    if Sys.os_type="Win32"
    then List.map Testlib.maybe_quote lst
    else lst in
  let cmd' = String.concat " " quoted_lst in
  Printf.fprintf log "Commandline: %s\n" cmd';
  let progname = List.hd quoted_lst in
  let arguments = Array.of_list quoted_lst in
  (*
  let environment =
    try [|Sys.getenv "PATH" |]
    with Not_found -> [| |] in
  *)
  let stdin_filename = Environments.safe_lookup stdin_variable env in
  let stdout_filename = Environments.safe_lookup stdout_variable env in
  let stderr_filename = Environments.safe_lookup stderr_variable env in
  log_redirection "stdin" stdin_filename;
  log_redirection "stdout" stdout_filename;
  log_redirection "stderr" stderr_filename;
  Run_command.run {
    Run_command.progname = progname;
    Run_command.argv = arguments;
    (* Run_command.envp = environment; *)
    Run_command.stdin_filename = stdin_filename;
    Run_command.stdout_filename = stdout_filename;
    Run_command.stderr_filename = stderr_filename;
    Run_command.append = append;
    Run_command.timeout = timeout;
    Run_command.log = log
  }

let mkreason what commandline exitcode =
  Printf.sprintf "%s: command\n%s\nfailed with exit code %d"
    what commandline exitcode

let make_file_name name ext = String.concat "." [name; ext]

let make_path components = List.fold_left Filename.concat "" components

(*
let rec map_reduce_result f g init = function
  | [] -> Ok init
  | x::xs ->
    (match f x with
      | Ok fx ->
        (match map_reduce_result f g init xs with
          | Ok fxs -> Ok (g fx fxs)
          | Error _ as e -> e
        )
      | Error _ as e -> e
    )
*)

let setup_symlinks test_source_directory build_directory files =
  let symlink filename =
    let src = Filename.concat test_source_directory filename in
    let cmd = "ln -sf " ^ src ^" " ^ build_directory in
    Testlib.run_system_command cmd in
  let copy filename =
    let src = Filename.concat test_source_directory filename in
    let dst = Filename.concat build_directory filename in
    Testlib.copy_file src dst in
  let f = if Sys.os_type="Win32" then copy else symlink in
  List.iter f files

let mkexe =
  if Sys.os_type="Win32"
  then fun name -> make_file_name name "exe"
  else fun name -> name

(* Compilers and flags *)

let ocamlsrcdir () =
  try Sys.getenv "OCAMLSRCDIR"
  with Not_found -> Ocamltest_config.ocamlsrcdir

let ocamlrun ocamlsrcdir =
  let ocamlrunfile = mkexe "ocamlrun" in
  make_path [ocamlsrcdir; "byterun"; ocamlrunfile]

let ocamlc ocamlsrcdir =
  make_path [ocamlsrcdir; "ocamlc"]

let ocaml ocamlsrcdir =
  make_path [ocamlsrcdir; "ocaml"]

let ocamlc_dot_byte ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let ocamlc = ocamlc ocamlsrcdir in
  ocamlrun ^ " " ^ ocamlc

let ocamlc_dot_opt ocamlsrcdir =
  make_path [ocamlsrcdir; "ocamlc.opt"]

let ocamlopt ocamlsrcdir =
  make_path [ocamlsrcdir; "ocamlopt"]

let ocamlopt_dot_byte ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let ocamlopt = ocamlopt ocamlsrcdir in
  ocamlrun ^ " " ^ ocamlopt

let ocamlopt_dot_opt ocamlsrcdir =
  make_path [ocamlsrcdir; "ocamlopt.opt"]

let ocaml_dot_byte ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let ocaml = ocaml ocamlsrcdir in
  ocamlrun ^ " " ^ ocaml

let ocaml_dot_opt ocamlsrcdir =
  make_path [ocamlsrcdir; mkexe "ocamlnat"]

let cmpbyt ocamlsrcdir =
  make_path [ocamlsrcdir; "tools"; "cmpbyt"]

let stdlib ocamlsrcdir =
  make_path [ocamlsrcdir; "stdlib"]

let stdlib_flags ocamlsrcdir =
  let stdlib_path = stdlib ocamlsrcdir in
  "-nostdlib -I " ^ stdlib_path

let c_includes ocamlsrcdir =
  make_path [ocamlsrcdir; "byterun"]

let c_includes_flags ocamlsrcdir =
  let dir = c_includes ocamlsrcdir in
  "-ccopt -I" ^ dir

let use_runtime backend ocamlsrcdir = match backend with
  | Sys.Bytecode ->
    let ocamlrun = ocamlrun ocamlsrcdir in
    "-use-runtime " ^ ocamlrun
  | _ -> ""

(* Compiler descriptions *)

type compiler_info = {
  compiler_name : string -> string;
  compiler_flags : string;
  compiler_directory : string;
  compiler_backend : Sys.backend_type;
  compiler_exit_status_variabe : Variables.t;
  compiler_reference_variable : Variables.t;
  compiler_output_variable : Variables.t
}

(* Compilers compiling byte-code programs *)

let bytecode_bytecode_compiler =
{
  compiler_name = ocamlc_dot_byte;
  compiler_flags = "";
  compiler_directory = "ocamlc.byte";
  compiler_backend = Sys.Bytecode;
  compiler_exit_status_variabe = Builtin_variables.ocamlc_byte_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference;
  compiler_output_variable = Builtin_variables.compiler_output;
}

let bytecode_native_compiler =
{
  compiler_name = ocamlc_dot_opt;
  compiler_flags = "";
  compiler_directory = "ocamlc.opt";
  compiler_backend = Sys.Bytecode;
  compiler_exit_status_variabe = Builtin_variables.ocamlc_opt_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference2;
  compiler_output_variable = Builtin_variables.compiler_output2;
}

(* Compilers compiling native-code programs *)

let native_bytecode_compiler =
{
  compiler_name = ocamlopt_dot_byte;
  compiler_flags = "";
  compiler_directory = "ocamlopt.byte";
  compiler_backend = Sys.Native;
  compiler_exit_status_variabe = Builtin_variables.ocamlopt_byte_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference;
  compiler_output_variable = Builtin_variables.compiler_output;
}

let native_native_compiler =
{
  compiler_name = ocamlopt_dot_opt;
  compiler_flags = "";
  compiler_directory = "ocamlopt.opt";
  compiler_backend = Sys.Native;
  compiler_exit_status_variabe = Builtin_variables.ocamlopt_opt_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference2;
  compiler_output_variable = Builtin_variables.compiler_output2;
}

(* Top-levels *)

let ocaml = {
  compiler_name = ocaml_dot_byte;
  compiler_flags = "";
  compiler_directory = "ocaml";
  compiler_backend = Sys.Bytecode;
  compiler_exit_status_variabe = Builtin_variables.ocaml_byte_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference;
  compiler_output_variable = Builtin_variables.compiler_output;
}

let ocamlnat = {
  compiler_name = ocaml_dot_opt;
  compiler_flags = "-S"; (* Keep intermediate assembly files *)
  compiler_directory = "ocamlnat";
  compiler_backend = Sys.Native;
  compiler_exit_status_variabe = Builtin_variables.ocaml_opt_exit_status;
  compiler_reference_variable = Builtin_variables.compiler_reference2;
  compiler_output_variable = Builtin_variables.compiler_output2;
}

let expected_compiler_exit_status env compiler =
  try int_of_string
    (Environments.safe_lookup compiler.compiler_exit_status_variabe env)
  with _ -> 0

let compiler_reference_filename env prefix compiler =
  let compiler_reference_suffix =
    Environments.safe_lookup Builtin_variables.compiler_reference_suffix env in
  let suffix =
    if compiler_reference_suffix<>""
    then compiler_reference_suffix ^ ".reference"
    else ".reference" in
  let mk s = (make_file_name prefix s) ^suffix in
  let filename = mk compiler.compiler_directory in
  if Sys.file_exists filename then filename else
  let filename = mk (Backends.string_of_backend compiler.compiler_backend) in
  if Sys.file_exists filename then filename else
  mk "compilers"

(* Extracting information from environment *)

let get_backend_value_from_env env bytecode_var native_var =
  Backends.make_backend_function
    (Environments.safe_lookup bytecode_var env)
    (Environments.safe_lookup native_var env)

let testfile env =
  match Environments.lookup Builtin_variables.test_file env with
  | None -> assert false
  | Some t -> t

let words_of_variable variable env =
  Testlib.words (Environments.safe_lookup variable env)

let modules env = words_of_variable Builtin_variables.modules env

let files env = words_of_variable Builtin_variables.files env

let flags env = Environments.safe_lookup Builtin_variables.flags env

let libraries backend env =
  let value = Environments.safe_lookup Builtin_variables.libraries env in
  let libs = Testlib.words value in
  let extension = Backends.library_extension backend in
  let add_extension lib = make_file_name lib extension in
  String.concat " " (List.map add_extension libs)

let backend_default_flags env =
  get_backend_value_from_env env
    Builtin_variables.ocamlc_default_flags
    Builtin_variables.ocamlopt_default_flags

let backend_flags env =
  get_backend_value_from_env env
    Builtin_variables.ocamlc_flags
    Builtin_variables.ocamlopt_flags

let test_source_directory env =
  Environments.safe_lookup Builtin_variables.test_source_directory env

let test_build_directory env =
  Environments.safe_lookup Builtin_variables.test_build_directory env

(*
let action_of_filetype = function
  | Filetype.Implementation -> "Compiling implementation"
  | Filetype.Interface -> "Compiling interface"
  | Filetype.C -> "Compiling C source file"
  | Filetype.C_minus_minus -> "Processing C minus minus file"
  | Filetype.Lexer -> "Generating lexer"
  | Filetype.Grammar -> "Generating parser"
*)

let link_modules
    ocamlsrcdir compiler compilername compileroutput program_variable
    custom c_headers_flags log env modules
  =
  let backend = compiler.compiler_backend in
  let expected_exit_status = expected_compiler_exit_status env compiler in
  let executable_name = match Environments.lookup program_variable env with
    | None -> assert false
    | Some program -> program in
  let module_names =
    String.concat " " (List.map Filetype.make_filename modules) in
  let what = Printf.sprintf "Linking modules %s into %s"
    module_names executable_name in
  Printf.fprintf log "%s\n%!" what;
  let output = "-o " ^ executable_name in
  let customstr = if custom then "-custom" else "" in
  let commandline =
  [
    compilername;
    customstr;
    c_headers_flags;
    use_runtime backend ocamlsrcdir;
    stdlib_flags ocamlsrcdir;
    "-linkall";
    flags env;
    libraries backend env;
    backend_default_flags env backend;
    backend_flags env backend;
    output;
    module_names
  ] in
  let exit_status =
    run_command
      ~stdout_variable:compileroutput
      ~stderr_variable:compileroutput
      ~append:true
      log env commandline in
  if exit_status=expected_exit_status
  then Pass env
  else Fail (mkreason what (String.concat " " commandline) exit_status)

let compile_program
    ocamlsrcdir compiler compilername compileroutput program_variable
    log env modules
  =
  let is_c_file (_filename, filetype) = filetype=Filetype.C in
  let has_c_file = List.exists is_c_file modules in
  let backend = compiler.compiler_backend in
  let custom = (backend = Sys.Bytecode) && has_c_file in
  let c_headers_flags =
    if has_c_file then c_includes_flags ocamlsrcdir else "" in
  link_modules
    ocamlsrcdir compiler compilername compileroutput
    program_variable custom c_headers_flags log env modules

let module_has_interface directory module_name =
  let interface_name =
    Filetype.make_filename (module_name, Filetype.Interface) in
  let interface_fullpath = make_path [directory;interface_name] in
  Sys.file_exists interface_fullpath

let add_module_interface directory module_description =
  match module_description with
    | (filename, Filetype.Implementation) when
      module_has_interface directory filename ->
        [(filename, Filetype.Interface); module_description]
  | _ -> [module_description]

let print_module_names log description modules =
  Printf.fprintf log "%s modules: %s\n%!"
    description
    (String.concat " " (List.map Filetype.make_filename modules))

let setup_build_environment
    testfile source_directory build_directory log env
  =
  let specified_modules =
    List.map Filetype.filetype ((modules env) @ [testfile]) in
  print_module_names log "Specified" specified_modules;
  let source_modules =
    Testlib.concatmap
      (add_module_interface source_directory)
      specified_modules in
  print_module_names log "Source" source_modules;
  Testlib.make_directory build_directory;
  setup_symlinks
    source_directory
    build_directory
    (List.map Filetype.make_filename source_modules);
  setup_symlinks source_directory build_directory (files env);
  Sys.chdir build_directory;
  source_modules

let prepare_module (module_name, module_type) =
  match module_type with
    | Filetype.Implementation | Filetype.Interface | Filetype.C ->
      [(module_name, module_type)]
    | Filetype.C_minus_minus -> assert false
    | Filetype.Lexer -> assert false
    | Filetype.Grammar -> assert false

let compile_test_program program_variable compiler log env =
  let backend = compiler.compiler_backend in
  let testfile = testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let source_directory = test_source_directory env in
  let compiler_directory_suffix =
    Environments.safe_lookup Builtin_variables.compiler_directory_suffix env in
  let compiler_directory_name =
    compiler.compiler_directory ^ compiler_directory_suffix in
  let build_directory =
    make_path [test_build_directory env; compiler_directory_name] in
  let compilerreference_prefix =
    make_path [source_directory; testfile_basename] in
  let compilerreference_filename =
    compiler_reference_filename env compilerreference_prefix compiler in
  let compiler_reference_variable = compiler.compiler_reference_variable in
  let executable_filename =
    mkexe
      (make_file_name
        testfile_basename (Backends.executable_extension backend)) in
  let executable_path = make_path [build_directory; executable_filename] in
  let compiler_output_filename =
    make_file_name compiler.compiler_directory "output" in
  let compiler_output =
    make_path [build_directory; compiler_output_filename] in
  let compiler_output_variable = compiler.compiler_output_variable in
  let newenv = Environments.add_bindings
    [
      (program_variable, executable_path);
      (compiler_reference_variable, compilerreference_filename);
      (compiler_output_variable, compiler_output);
    ] env in
  if Sys.file_exists compiler_output_filename then
    Sys.remove compiler_output_filename;
  let ocamlsrcdir = ocamlsrcdir () in
  let compilername = compiler.compiler_name ocamlsrcdir in
  let source_modules =
    setup_build_environment
      testfile source_directory build_directory log env in
  let prepared_modules =
    Testlib.concatmap prepare_module source_modules in
  compile_program
    ocamlsrcdir
    compiler
    compilername
    compiler_output_variable
    program_variable log newenv prepared_modules

(* Compile actions *)

let compile_bytecode_with_bytecode_compiler = {
  action_name = "compile-bytecode-with-bytecode-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program
      Builtin_variables.program bytecode_bytecode_compiler
}

let compile_bytecode_with_native_compiler = {
  action_name = "compile-bytecode-with-native-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program
      Builtin_variables.program2 bytecode_native_compiler
}

let compile_native_with_bytecode_compiler = {
  action_name = "compile-native-with-bytecode-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program
      Builtin_variables.program native_bytecode_compiler
}

let compile_native_with_native_compiler = {
  action_name = "compile-native-with-native-compiler";
  action_environment = env_id;
  action_body =
    compile_test_program
      Builtin_variables.program2 native_native_compiler
}

let exec log_message redirect_output prog_variable args_variable log env =
  match Environments.lookup prog_variable env with
  | None ->
    let msg = Printf.sprintf "%s: variable %s is undefined"
      log_message (Variables.name_of_variable prog_variable) in
    Fail msg
  | Some program ->
    let arguments = Environments.safe_lookup args_variable env in
    let commandline = [program; arguments] in
    let what = log_message ^ " " ^ program ^ " " ^
    begin if arguments="" then "without any argument"
    else "with arguments " ^ arguments
    end in
    let output = program ^ ".output" in
    let bindings =
    [
      Builtin_variables.stdout, output;
      Builtin_variables.stderr, output
    ] in
    let execution_env =
      if redirect_output then Environments.add_bindings bindings env
      else env in
    match run_command log execution_env commandline with
      | 0 ->
        let newenv =
          if redirect_output
          then Environments.add Builtin_variables.output output env
          else env in
        Pass newenv
      | _ as exitcode ->
        if exitcode = 125
        then Skip (mkreason what (String.concat " " commandline) exitcode)
        else Fail (mkreason what (String.concat " " commandline) exitcode)

let execute_program =
  exec
    "Executing program"
    true
    Builtin_variables.program
    Builtin_variables.arguments

let execute = {
  action_name = "execute-program";
  action_environment = env_id;
  action_body = execute_program
}

let run_script log env =
  let testfile = testfile env in
  (* let testfile_basename = Filename.chop_extension testfile in *)
  let source_directory = test_source_directory env in
  let build_directory = test_build_directory env in
  let _modules =
    setup_build_environment
      testfile source_directory build_directory log env in
  exec
    "Running script"
    false
    Builtin_variables.script
    Builtin_variables.test_file
    log env

let script = {
  action_name = "run-script";
  action_environment = env_id;
  action_body = run_script
}

let run_expect log env =
  let newenv = Environments.apply_modifiers env Builtin_modifiers.expect in
  run_script log newenv

let expect = {
  action_name = "run-expect";
  action_environment = env_id;
  action_body = run_expect
}

let check_output kind_of_output output_variable reference_variable log env =
  let reference_filename = Environments.safe_lookup reference_variable env in
  let output_filename = Environments.safe_lookup output_variable env in
  Printf.fprintf log "Comparing %s output %s to reference %s\n%!"
    kind_of_output output_filename reference_filename;
  let files =
  {
    Filecompare.filetype = Filecompare.Text;
    Filecompare.reference_filename = reference_filename;
    Filecompare.output_filename = output_filename
  } in
  match Filecompare.check_file files with
    | Filecompare.Same -> Pass env
    | Filecompare.Different ->
      let diff = Filecompare.diff files in
      let diffstr = match diff with
        | Ok difference -> difference
        | Error diff_file -> ("See " ^ diff_file) in
      let reason =
        Printf.sprintf "%s output %s differs from reference %s: \n%s\n"
        kind_of_output output_filename reference_filename diffstr in
      (Actions.Fail reason)
    | Filecompare.Unexpected_output ->
      let banner = String.make 40 '=' in
      let unexpected_output = Testlib.string_of_file output_filename in
      let unexpected_output_with_banners = Printf.sprintf
        "%s\n%s%s\n" banner unexpected_output banner in
      let reason = Printf.sprintf
        "The file %s was expected to be empty because there is no \
          reference file %s but it is not:\n%s\n"
        output_filename reference_filename unexpected_output_with_banners in
      (Actions.Fail reason)
    | Filecompare.Error (commandline, exitcode) ->
      let reason = Printf.sprintf "The command %s failed with status %d"
        commandline exitcode in
      (Actions.Fail reason)

let make_check_compiler_output name compiler = {
  action_name = name;
  action_environment = env_id;
  action_body =
    check_output
      "compiler"
      compiler.compiler_output_variable
      compiler.compiler_reference_variable
}

let check_ocamlc_dot_byte_output = make_check_compiler_output
  "check-ocamlc-byte-output" bytecode_bytecode_compiler

let check_ocamlc_dot_opt_output = make_check_compiler_output
  "check-ocamlc-opt-output" bytecode_native_compiler

let check_ocamlopt_dot_byte_output = make_check_compiler_output
  "check-ocamlopt-byte-output" native_bytecode_compiler

let check_ocamlopt_dot_opt_output = make_check_compiler_output
  "check-ocamlopt-opt-output" native_native_compiler

let check_program_output = {
  action_name = "check-program-output";
  action_environment = env_id;
  action_body = check_output "program"
    Builtin_variables.output
    Builtin_variables.reference
}

(*
let comparison_start_address portable_executable_filename =
  let portable_executalbe_signature = "PE\000\000" in
  let signature_length = String.length portable_executalbe_signature in
  let address_length = 4 in
  let start_address = 0x3c in
  let ic = open_in portable_executable_filename in
  seek_in ic start_address;
  let portable_executable_signature_address_str =
    really_input_string ic address_length in
  let b0 = int_of_char portable_executable_signature_address_str.[0] in
  let b1 = int_of_char portable_executable_signature_address_str.[1] in
  let b2 = int_of_char portable_executable_signature_address_str.[2] in
  let b3 = int_of_char portable_executable_signature_address_str.[3] in
  let signature_address =
    b0 +
    b1 * 256 +
    b2 * 256 * 256 +
    b3 * 256 * 256 * 256 in
  seek_in ic signature_address;
  let signature =
    really_input_string ic signature_length in
  if signature<>portable_executalbe_signature
  then failwith
    (portable_executable_filename ^ " does not contain the PE signature");
  let result = signature_address + 12 in
  (* 12 is 4-bytes signature, 2-bytes machine type, *)
  (* 2-bytes number of sections, 4-bytes timestamp *)
  close_in ic;
  result
*)

let compare_programs backend comparison_tool log env =
  let program = Environments.safe_lookup Builtin_variables.program env in
  let program2 = Environments.safe_lookup Builtin_variables.program2 env in
  let what = Printf.sprintf "Comparing %s programs %s and %s"
    (Backends.string_of_backend backend) program program2 in
  Printf.fprintf log "%s\n%!" what;
  let files = {
    Filecompare.filetype = Filecompare.Binary;
    Filecompare.reference_filename = program;
    Filecompare.output_filename = program2
  } in
  if Ocamltest_config.flambda && backend = Sys.Native
  then begin
    Printf.fprintf log
      "flambda temporarily disables comparison of native programs";
    Pass env
  end else if backend = Sys.Native && (Sys.os_type="Win32" || Sys.os_type="Cygwin")
  then begin
    Printf.fprintf log
      "comparison of native programs temporarily disabled under Windows";
    Pass env
  end else begin
    let comparison_tool =
      if backend=Sys.Native && (Sys.os_type="Win32" || Sys.os_type="Cygwin")
        then
          let bytes_to_ignore = 512 (* comparison_start_address program *) in
          Filecompare.make_cmp_tool bytes_to_ignore
        else comparison_tool in
    match Filecompare.compare_files ~tool:comparison_tool files with
      | Filecompare.Same -> Pass env
      | Filecompare.Different ->
        let reason = Printf.sprintf "Files %s and %s are different"
          program program2 in
        Fail reason
      | Filecompare.Unexpected_output -> assert false
      | Filecompare.Error (commandline, exitcode) ->
        let reason = mkreason what commandline exitcode in
        Fail reason
  end

let make_bytecode_programs_comparison_tool ocamlsrcdir =
  let ocamlrun = ocamlrun ocamlsrcdir in
  let cmpbyt = cmpbyt ocamlsrcdir in
  let tool_name = ocamlrun ^ " " ^ cmpbyt in
  Filecompare.make_comparison_tool tool_name ""

let native_programs_comparison_tool = Filecompare.default_comparison_tool

let compare_bytecode_programs_body log env =
  let ocamlsrcdir = ocamlsrcdir () in
  let bytecode_programs_comparison_tool =
    make_bytecode_programs_comparison_tool ocamlsrcdir in
  compare_programs Sys.Bytecode bytecode_programs_comparison_tool log env

let compare_bytecode_programs = {
  action_name = "compare-bytecode-programs";
  action_environment = env_id;
  action_body = compare_bytecode_programs_body
}

let compare_native_programs = {
  action_name = "compare-native-programs";
  action_environment = env_id;
  action_body = compare_programs Sys.Native native_programs_comparison_tool
}

let run_test_program_in_toplevel toplevel log env =
  let testfile = testfile env in
  let testfile_basename = Filename.chop_extension testfile in
  let expected_exit_status = expected_compiler_exit_status env toplevel in
  let what =
    Printf.sprintf "Running %s in %s toplevel (expected exit status: %d)"
      testfile
      (Backends.string_of_backend toplevel.compiler_backend)
      expected_exit_status in
  Printf.fprintf log "%s\n%!" what;
  let source_directory = test_source_directory env in
  let compiler_directory_suffix =
    Environments.safe_lookup Builtin_variables.compiler_directory_suffix env in
  let compiler_directory_name =
    toplevel.compiler_directory ^ compiler_directory_suffix in
  let build_directory =
    make_path [test_build_directory env; compiler_directory_name] in
  let _modules =
    setup_build_environment
      testfile source_directory build_directory log env in
  let compilerreference_prefix =
    make_path [source_directory; testfile_basename] in
  let compilerreference_filename =
    compiler_reference_filename env compilerreference_prefix toplevel in
  let compiler_reference_variable = toplevel.compiler_reference_variable in
  let compiler_output_filename =
    make_file_name toplevel.compiler_directory "output" in
  let compiler_output =
    make_path [build_directory; compiler_output_filename] in
  let compiler_output_variable = toplevel.compiler_output_variable in
  let newenv = Environments.add_bindings
    [
      (compiler_reference_variable, compilerreference_filename);
      (compiler_output_variable, compiler_output);
    ] env in
  if Sys.file_exists compiler_output_filename then
    Sys.remove compiler_output_filename;
  let ocamlsrcdir = ocamlsrcdir () in
  let toplevel_name = toplevel.compiler_name ocamlsrcdir in
  let toplevel_default_flags = "-noinit -no-version -noprompt" in
  let commandline =
  [
    toplevel_name;
    toplevel_default_flags;
    toplevel.compiler_flags;
    stdlib_flags ocamlsrcdir;
    flags env;
  ] in
  let exit_status =
    run_command
      ~stdin_variable:Builtin_variables.test_file
      ~stdout_variable:compiler_output_variable
      ~stderr_variable:compiler_output_variable
      log newenv commandline in
  if exit_status=expected_exit_status
  then Pass newenv
  else Fail (mkreason what (String.concat " " commandline) exit_status)

let run_in_ocaml =
{
  action_name = "run-in-bytecode-toplevel";
  action_environment = env_id;
  action_body = run_test_program_in_toplevel ocaml;
}

let run_in_ocamlnat =
{
  action_name = "run-in-native-toplevel";
  action_environment = env_id;
  action_body = run_test_program_in_toplevel ocamlnat;
}

let check_ocaml_output = make_check_compiler_output
  "check-bytecode-toplevel-output" ocaml

let check_ocamlnat_output = make_check_compiler_output
  "check-native-toplevel-output" ocamlnat

let if_not_safe_string = {
  action_name = "if_not_safe_string";
  action_environment = env_id;
  action_body = fun _log env ->
    if Ocamltest_config.safe_string
    then Skip "safe strings enabled"
    else Pass env
}

let _ =
  List.iter register
  [
    compile_bytecode_with_bytecode_compiler;
    compile_bytecode_with_native_compiler;
    compile_native_with_bytecode_compiler;
    compile_native_with_native_compiler;
    execute;
    script;
    check_program_output;
    compare_bytecode_programs;
    compare_native_programs;
    check_ocamlc_dot_byte_output;
    check_ocamlc_dot_opt_output;
    check_ocamlopt_dot_byte_output;
    check_ocamlopt_dot_opt_output;
    run_in_ocaml;
    run_in_ocamlnat;
    check_ocaml_output;
    check_ocamlnat_output;
    if_not_safe_string;
  ]
