(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Clflags

let output_prefix name =
  let oname =
    match !output_name with
    | None -> name
    | Some n -> if !compile_only then (output_name := None; n) else name in
  Filename.remove_extension oname

let print_version_and_library compiler =
  Printf.printf "The OCaml %s, version " compiler;
  print_string Config.version; print_newline();
  print_string "Standard library directory: ";
  print_string Config.standard_library; print_newline();
  exit 0

let print_version_string () =
  print_string Config.version; print_newline(); exit 0

let print_standard_library () =
  print_string Config.standard_library; print_newline(); exit 0

let fatal err =
  prerr_endline err;
  exit 2

let extract_output = function
  | Some s -> s
  | None ->
      fatal "Please specify the name of the output file, using option -o"

let default_output = function
  | Some s -> s
  | None -> Config.default_executable_name

let implicit_modules = ref []
let first_include_dirs = ref []
let last_include_dirs = ref []
let first_ccopts = ref []
let last_ccopts = ref []
let first_ppx = ref []
let last_ppx = ref []
let first_objfiles = ref []
let last_objfiles = ref []

(* Check validity of module name *)
let is_unit_name name =
  try
    if name = "" then raise Exit;
    begin match name.[0] with
    | 'A'..'Z' -> ()
    | _ ->
       raise Exit;
    end;
    for i = 1 to String.length name - 1 do
      match name.[i] with
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> ()
      | _ ->
         raise Exit;
    done;
    true
  with Exit -> false
;;

let check_unit_name ppf filename name =
  if not (is_unit_name name) then
    Location.print_warning (Location.in_file filename) ppf
      (Warnings.Bad_module_name name);;

(* Compute name of module from output file name *)
let module_of_filename ppf inputfile outputprefix =
  let basename = Filename.basename outputprefix in
  let name =
    try
      let pos = String.index basename '.' in
      String.sub basename 0 pos
    with Not_found -> basename
  in
  let name = String.capitalize_ascii name in
  check_unit_name ppf inputfile name;
  name
;;

type filename = string

type readenv_position =
  Before_args | Before_compile of filename | Before_link

(* Syntax of OCAMLPARAM: (name=VALUE,)* _ (,name=VALUE)*
   where VALUE should not contain ',' *)
exception SyntaxError of string

let parse_args s =
  let args = String.split_on_char ',' s in
  let rec iter is_after args before after =
    match args with
      [] ->
      if not is_after then
        raise (SyntaxError "no '_' separator found")
      else
      (List.rev before, List.rev after)
    | "_" :: _ when is_after -> raise (SyntaxError "too many '_' separators")
    | "_" :: tail -> iter true tail before after
    | arg :: tail ->
      let binding = try
        Misc.cut_at arg '='
      with Not_found ->
        raise (SyntaxError ("missing '=' in " ^ arg))
      in
      if is_after then
        iter is_after tail before (binding :: after)
      else
        iter is_after tail (binding :: before) after
  in
  iter false args [] []

let setter ppf f name options s =
  try
    let bool = match s with
      | "0" -> false
      | "1" -> true
      | _ -> raise Not_found
    in
    List.iter (fun b -> b := f bool) options
  with Not_found ->
    Location.print_warning Location.none ppf
      (Warnings.Bad_env_variable ("OCAMLPARAM",
                                  Printf.sprintf "bad value for %s" name))

let int_setter ppf name option s =
  try
    option := int_of_string s
  with _ ->
    Location.print_warning Location.none ppf
      (Warnings.Bad_env_variable
         ("OCAMLPARAM", Printf.sprintf "non-integer parameter for \"%s\"" name))

let int_option_setter ppf name option s =
  try
    option := Some (int_of_string s)
  with _ ->
    Location.print_warning Location.none ppf
      (Warnings.Bad_env_variable
         ("OCAMLPARAM", Printf.sprintf "non-integer parameter for \"%s\"" name))

(*
let float_setter ppf name option s =
  try
    option := float_of_string s
  with _ ->
    Location.print_warning Location.none ppf
      (Warnings.Bad_env_variable
         ("OCAMLPARAM", Printf.sprintf "non-float parameter for \"%s\"" name))
*)

let load_plugin = ref (fun _ -> ())

let check_bool ppf name s =
  match s with
  | "0" -> false
  | "1" -> true
  | _ ->
    Location.print_warning Location.none ppf
      (Warnings.Bad_env_variable ("OCAMLPARAM",
                                  Printf.sprintf "bad value for %s" name));
    false

(* 'can-discard=' specifies which arguments can be discarded without warning
   because they are not understood by some versions of OCaml. *)
let can_discard = ref []

let read_one_param ppf position name v =
  let set name options s =  setter ppf (fun b -> b) name options s in
  let clear name options s = setter ppf (fun b -> not b) name options s in
  match name with
  | "g" -> set "g" [ Clflags.debug ] v
  | "p" -> set "p" [ Clflags.gprofile ] v
  | "bin-annot" -> set "bin-annot" [ Clflags.binary_annotations ] v
  | "afl-instrument" -> set "afl-instrument" [ Clflags.afl_instrument ] v
  | "afl-inst-ratio" ->
      int_setter ppf "afl-inst-ratio" afl_inst_ratio v
  | "annot" -> set "annot" [ Clflags.annotations ] v
  | "absname" -> set "absname" [ Location.absname ] v
  | "compat-32" -> set "compat-32" [ bytecode_compatible_32 ] v
  | "noassert" -> set "noassert" [ noassert ] v
  | "noautolink" -> set "noautolink" [ no_auto_link ] v
  | "nostdlib" -> set "nostdlib" [ no_std_include ] v
  | "linkall" -> set "linkall" [ link_everything ] v
  | "nolabels" -> set "nolabels" [ classic ] v
  | "principal" -> set "principal"  [ principal ] v
  | "rectypes" -> set "rectypes" [ recursive_types ] v
  | "safe-string" -> clear "safe-string" [ unsafe_string ] v
  | "strict-sequence" -> set "strict-sequence" [ strict_sequence ] v
  | "strict-formats" -> set "strict-formats" [ strict_formats ] v
  | "thread" -> set "thread" [ use_threads ] v
  | "unboxed-types" -> set "unboxed-types" [ unboxed_types ] v
  | "unsafe" -> set "unsafe" [ fast ] v
  | "verbose" -> set "verbose" [ verbose ] v
  | "nopervasives" -> set "nopervasives" [ nopervasives ] v
  | "slash" -> set "slash" [ force_slash ] v (* for ocamldep *)
  | "keep-docs" -> set "keep-docs" [ Clflags.keep_docs ] v
  | "keep-locs" -> set "keep-locs" [ Clflags.keep_locs ] v

  | "compact" -> clear "compact" [ optimize_for_speed ] v
  | "no-app-funct" -> clear "no-app-funct" [ applicative_functors ] v
  | "nodynlink" -> clear "nodynlink" [ dlcode ] v
  | "short-paths" -> clear "short-paths" [ real_paths ] v
  | "trans-mod" -> set "trans-mod" [ transparent_modules ] v
  | "opaque" -> set "opaque" [ opaque ] v

  | "pp" -> preprocessor := Some v
  | "runtime-variant" -> runtime_variant := v
  | "cc" -> c_compiler := Some v

  | "clambda-checks" -> set "clambda-checks" [ clambda_checks ] v

  (* assembly sources *)
  |  "s" ->
    set "s" [ Clflags.keep_asm_file ; Clflags.keep_startup_file ] v
  |  "S" -> set "S" [ Clflags.keep_asm_file ] v
  |  "dstartup" -> set "dstartup" [ Clflags.keep_startup_file ] v

  (* warn-errors *)
  | "we" | "warn-error" -> Warnings.parse_options true v
  (* warnings *)
  |  "w"  ->               Warnings.parse_options false v
  (* warn-errors *)
  | "wwe" ->               Warnings.parse_options false v

  (* inlining *)
  | "inline" ->
      let module F = Float_arg_helper in
      begin match F.parse_no_error v inline_threshold with
      | F.Ok -> ()
      | F.Parse_failed exn ->
          let error =
            Printf.sprintf "bad syntax for \"inline\": %s"
              (Printexc.to_string exn)
          in
          Location.print_warning Location.none ppf
            (Warnings.Bad_env_variable ("OCAMLPARAM", error))
      end

  | "inline-toplevel" ->
    Int_arg_helper.parse v
      "Bad syntax in OCAMLPARAM for 'inline-toplevel'"
      inline_toplevel_threshold

  | "rounds" -> int_option_setter ppf "rounds" simplify_rounds v
  | "inline-max-unroll" ->
    Int_arg_helper.parse v "Bad syntax in OCAMLPARAM for 'inline-max-unroll'"
      inline_max_unroll
  | "inline-call-cost" ->
    Int_arg_helper.parse v
      "Bad syntax in OCAMLPARAM for 'inline-call-cost'"
      inline_call_cost
  | "inline-alloc-cost" ->
    Int_arg_helper.parse v
      "Bad syntax in OCAMLPARAM for 'inline-alloc-cost'"
      inline_alloc_cost
  | "inline-prim-cost" ->
    Int_arg_helper.parse v
      "Bad syntax in OCAMLPARAM for 'inline-prim-cost'"
      inline_prim_cost
  | "inline-branch-cost" ->
    Int_arg_helper.parse v
      "Bad syntax in OCAMLPARAM for 'inline-branch-cost'"
      inline_branch_cost
  | "inline-indirect-cost" ->
    Int_arg_helper.parse v
      "Bad syntax in OCAMLPARAM for 'inline-indirect-cost'"
      inline_indirect_cost
  | "inline-lifting-benefit" ->
    Int_arg_helper.parse v
      "Bad syntax in OCAMLPARAM for 'inline-lifting-benefit'"
      inline_lifting_benefit
  | "inline-branch-factor" ->
    Float_arg_helper.parse v
      "Bad syntax in OCAMLPARAM for 'inline-branch-factor'"
      inline_branch_factor
  | "inline-max-depth" ->
    Int_arg_helper.parse v
      "Bad syntax in OCAMLPARAM for 'inline-max-depth'"
      inline_max_depth

  | "Oclassic" ->
      set "Oclassic" [ classic_inlining ] v
  | "O2" ->
    if check_bool ppf "O2" v then begin
      default_simplify_rounds := 2;
      use_inlining_arguments_set o2_arguments;
      use_inlining_arguments_set ~round:0 o1_arguments
    end

  | "O3" ->
    if check_bool ppf "O3" v then begin
      default_simplify_rounds := 3;
      use_inlining_arguments_set o3_arguments;
      use_inlining_arguments_set ~round:1 o2_arguments;
      use_inlining_arguments_set ~round:0 o1_arguments
    end
  | "unbox-closures" ->
      set "unbox-closures" [ unbox_closures ] v
  | "unbox-closures-factor" ->
      int_setter ppf "unbox-closures-factor" unbox_closures_factor v
  | "remove-unused-arguments" ->
      set "remove-unused-arguments" [ remove_unused_arguments ] v

  | "inlining-report" ->
      if !native_code then
        set "inlining-report" [ inlining_report ] v

  | "flambda-verbose" ->
      set "flambda-verbose" [ dump_flambda_verbose ] v
  | "flambda-invariants" ->
      set "flambda-invariants" [ flambda_invariant_checks ] v

  (* color output *)
  | "color" ->
      begin match parse_color_setting v with
      | None ->
        Location.print_warning Location.none ppf
          (Warnings.Bad_env_variable ("OCAMLPARAM",
           "bad value for \"color\", \
            (expected \"auto\", \"always\" or \"never\")"))
      | Some setting -> color := Some setting
      end

  | "intf-suffix" -> Config.interface_suffix := v

  | "I" -> begin
      match position with
      | Before_args -> first_include_dirs := v :: !first_include_dirs
      | Before_link | Before_compile _ ->
        last_include_dirs := v :: !last_include_dirs
    end

  | "cclib" ->
    begin
      match position with
      | Before_compile _ -> ()
      | Before_link | Before_args ->
        ccobjs := Misc.rev_split_words v @ !ccobjs
    end

  | "ccopts" ->
    begin
      match position with
      | Before_link | Before_compile _ ->
        last_ccopts := v :: !last_ccopts
      | Before_args ->
        first_ccopts := v :: !first_ccopts
    end

  | "ppx" ->
    begin
      match position with
      | Before_link | Before_compile _ ->
        last_ppx := v :: !last_ppx
      | Before_args ->
        first_ppx := v :: !first_ppx
    end


  | "cmo" | "cma" ->
    if not !native_code then
    begin
      match position with
      | Before_link | Before_compile _ ->
        last_objfiles := v ::! last_objfiles
      | Before_args ->
        first_objfiles := v :: !first_objfiles
    end

  | "cmx" | "cmxa" ->
    if !native_code then
    begin
      match position with
      | Before_link | Before_compile _ ->
        last_objfiles := v ::! last_objfiles
      | Before_args ->
        first_objfiles := v :: !first_objfiles
    end

  | "pic" ->
    if !native_code then
      set "pic" [ pic_code ] v

  | "can-discard" ->
    can_discard := v ::!can_discard

  | "timings" | "profile" ->
     let if_on = if name = "timings" then [ `Time ] else Profile.all_columns in
     profile_columns := if check_bool ppf name v then if_on else []

  | "plugin" -> !load_plugin v

  | _ ->
    if not (List.mem name !can_discard) then begin
      can_discard := name :: !can_discard;
      Printf.eprintf
        "Warning: discarding value of variable %S in OCAMLPARAM\n%!"
        name
    end

let read_OCAMLPARAM ppf position =
  try
    let s = Sys.getenv "OCAMLPARAM" in
    let (before, after) =
      try
        parse_args s
      with SyntaxError s ->
         Location.print_warning Location.none ppf
           (Warnings.Bad_env_variable ("OCAMLPARAM", s));
         [],[]
    in
    List.iter (fun (name, v) -> read_one_param ppf position name v)
      (match position with
         Before_args -> before
       | Before_compile _ | Before_link -> after)
  with Not_found -> ()

(* OCAMLPARAM passed as file *)

type pattern =
  | Filename of string
  | Any

type file_option = {
  pattern : pattern;
  name : string;
  value : string;
}

let scan_line ic =
  Scanf.bscanf ic "%[0-9a-zA-Z_.*] : %[a-zA-Z_-] = %s "
    (fun pattern name value ->
       let pattern =
         match pattern with
         | "*" -> Any
         | _ -> Filename pattern
       in
       { pattern; name; value })

let load_config ppf filename =
  match open_in_bin filename with
  | exception e ->
      Location.print_error ppf (Location.in_file filename);
      Format.fprintf ppf "Cannot open file %s@." (Printexc.to_string e);
      raise Exit
  | ic ->
      let sic = Scanf.Scanning.from_channel ic in
      let rec read line_number line_start acc =
        match scan_line sic with
        | exception End_of_file ->
            close_in ic;
            acc
        | exception Scanf.Scan_failure error ->
            let position = Lexing.{
                pos_fname = filename;
                pos_lnum = line_number;
                pos_bol = line_start;
                pos_cnum = pos_in ic;
              }
            in
            let loc = Location.{
                loc_start = position;
                loc_end = position;
                loc_ghost = false;
              }
            in
            Location.print_error ppf loc;
            Format.fprintf ppf "Configuration file error %s@." error;
            close_in ic;
            raise Exit
        | line ->
            read (line_number + 1) (pos_in ic) (line :: acc)
      in
      let lines = read 0 0 [] in
      lines

let matching_filename filename { pattern } =
  match pattern with
  | Any -> true
  | Filename pattern ->
    let filename = String.lowercase_ascii filename in
    let pattern = String.lowercase_ascii pattern in
    filename = pattern

let apply_config_file ppf position =
  let config_file =
    Filename.concat Config.standard_library "ocaml_compiler_internal_params"
  in
  let config =
    if Sys.file_exists config_file then
      load_config ppf config_file
    else
      []
  in
  let config =
    match position with
    | Before_compile filename ->
      List.filter (matching_filename filename) config
    | Before_args | Before_link ->
      List.filter (fun { pattern } -> pattern = Any) config
  in
  List.iter (fun { name; value } -> read_one_param ppf position name value)
    config

let readenv ppf position =
  last_include_dirs := [];
  last_ccopts := [];
  last_ppx := [];
  last_objfiles := [];
  apply_config_file ppf position;
  read_OCAMLPARAM ppf position;
  all_ccopts := !last_ccopts @ !first_ccopts;
  all_ppx := !last_ppx @ !first_ppx

let get_objfiles ~with_ocamlparam =
  if with_ocamlparam then
    List.rev (!last_objfiles @ !objfiles @ !first_objfiles)
  else
    List.rev !objfiles






type deferred_action =
  | ProcessImplementation of string
  | ProcessInterface of string
  | ProcessCFile of string
  | ProcessOtherFile of string
  | ProcessObjects of string list
  | ProcessDLLs of string list

let c_object_of_filename name =
  Filename.chop_suffix (Filename.basename name) ".c" ^ Config.ext_obj

let process_action
    (ppf, implementation, interface, ocaml_mod_ext, ocaml_lib_ext) action =
  match action with
  | ProcessImplementation name ->
      readenv ppf (Before_compile name);
      let opref = output_prefix name in
      implementation ppf name opref;
      objfiles := (opref ^ ocaml_mod_ext) :: !objfiles
  | ProcessInterface name ->
      readenv ppf (Before_compile name);
      let opref = output_prefix name in
      interface ppf name opref;
      if !make_package then objfiles := (opref ^ ".cmi") :: !objfiles
  | ProcessCFile name ->
      readenv ppf (Before_compile name);
      Location.input_name := name;
      if Ccomp.compile_file name <> 0 then exit 2;
      ccobjs := c_object_of_filename name :: !ccobjs
  | ProcessObjects names ->
      ccobjs := names @ !ccobjs
  | ProcessDLLs names ->
      dllibs := names @ !dllibs
  | ProcessOtherFile name ->
      if Filename.check_suffix name ocaml_mod_ext
      || Filename.check_suffix name ocaml_lib_ext then
        objfiles := name :: !objfiles
      else if Filename.check_suffix name ".cmi" && !make_package then
        objfiles := name :: !objfiles
      else if Filename.check_suffix name Config.ext_obj
           || Filename.check_suffix name Config.ext_lib then
        ccobjs := name :: !ccobjs
      else if not !native_code && Filename.check_suffix name Config.ext_dll then
        dllibs := name :: !dllibs
      else
        raise(Arg.Bad("don't know what to do with " ^ name))


let action_of_file name =
  if Filename.check_suffix name ".ml"
  || Filename.check_suffix name ".mlt" then
    ProcessImplementation name
  else if Filename.check_suffix name !Config.interface_suffix then
    ProcessInterface name
  else if Filename.check_suffix name ".c" then
    ProcessCFile name
  else
    ProcessOtherFile name

let deferred_actions = ref []
let defer action =
  deferred_actions := action :: !deferred_actions

let anonymous filename = defer (action_of_file filename)
let impl filename = defer (ProcessImplementation filename)
let intf filename = defer (ProcessInterface filename)

let process_deferred_actions env =
  let final_output_name = !output_name in
  (* Make sure the intermediate products don't clash with the final one
     when we're invoked like: ocamlopt -o foo bar.c baz.ml. *)
  if not !compile_only then output_name := None;
  begin
    match final_output_name with
    | None -> ()
    | Some output_name ->
        if !compile_only then begin
          if List.filter (function
              | ProcessCFile name -> c_object_of_filename name <> output_name
              | _ -> false) !deferred_actions <> [] then
            fatal "Options -c and -o are incompatible when compiling C files";

          if List.length (List.filter (function
              | ProcessImplementation _
              | ProcessInterface _
              | _ -> false) !deferred_actions) > 1 then
            fatal "Options -c -o are incompatible with compiling multiple files"
        end;
  end;
  if !make_archive && List.exists (function
      | ProcessOtherFile name -> Filename.check_suffix name ".cmxa"
      | _ -> false) !deferred_actions then
    fatal "Option -a cannot be used with .cmxa input files.";
  List.iter (process_action env) (List.rev !deferred_actions);
  output_name := final_output_name;
