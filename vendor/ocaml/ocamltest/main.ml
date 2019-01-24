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

(* Main program of the ocamltest test driver *)

open Tsl_semantics

(*
let first_token filename =
  let input_channel = open_in filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf filename;
  let token =
    try Tsl_lexer.token lexbuf with e -> close_in input_channel; raise e
  in close_in input_channel; token

let is_test filename =
  match first_token filename with
    | exception _ -> false
    | Tsl_parser.TSL_BEGIN -> true
    | _ -> false
*)

let tsl_block_of_file test_filename =
  let input_channel = open_in test_filename in
  let lexbuf = Lexing.from_channel input_channel in
  Location.init lexbuf test_filename;
  match Tsl_parser.tsl_block Tsl_lexer.token lexbuf with
    | exception e -> close_in input_channel; raise e
    | _ as tsl_block -> close_in input_channel; tsl_block

let tsl_block_of_file_safe test_filename =
  try tsl_block_of_file test_filename with
  | Sys_error message ->
    Printf.eprintf "%s\n" message;
    exit 1
  | Parsing.Parse_error ->
    Printf.eprintf "Could not read test block in %s\n" test_filename;
    exit 1

let print_usage () =
  Printf.printf "%s\n%!" Options.usage

let rec run_test log common_prefix path ancestor_result = function
  Node (testenvspec, test, env_modifiers, subtrees) ->
  Printf.printf "%s %s (%s) => %!" common_prefix path test.Tests.test_name;
  let print_test_result str = Printf.printf "%s\n%!" str in
  let test_result = match ancestor_result with
    | Actions.Pass env -> (* Ancestor succeded, really run the test *)
      let testenv0 = interprete_environment_statements env testenvspec in
      let testenv = List.fold_left apply_modifiers testenv0 env_modifiers in
      Tests.run log testenv test
    | Actions.Skip _ -> (Actions.Skip "ancestor test skipped")
    | Actions.Fail _ -> (Actions.Skip "ancestor test failed") in
  let result_to_pass = match test_result with
    | Actions.Pass _ ->
      print_test_result "passed";
      test_result
    | Actions.Fail _ ->
      print_test_result "failed";
      ancestor_result
    | Actions.Skip _ ->
      print_test_result "skipped";
      ancestor_result in
  List.iteri (run_test_i log common_prefix path result_to_pass) subtrees
and run_test_i log common_prefix path ancestor_result i test_tree =
  let path_prefix = if path="" then "" else path ^ "." in
  let new_path = Printf.sprintf "%s%d" path_prefix (i+1) in
  run_test log common_prefix new_path ancestor_result test_tree

let get_test_source_directory test_dirname =
  if not (Filename.is_relative test_dirname) then test_dirname
  else let pwd = Sys.getcwd() in
  Filename.concat pwd test_dirname

let get_test_build_directory test_dirname =
  let ocamltestdir_variable = "OCAMLTESTDIR" in
  let root = try Sys.getenv ocamltestdir_variable with
    | Not_found -> (Filename.concat (Sys.getcwd ()) "_ocamltest") in
  if test_dirname = "." then root
  else Filename.concat root test_dirname

let main () =
  if !Options.testfile = "" then begin
    print_usage();
    exit 1
  end;
  let test_filename = !Options.testfile in
  (* Printf.printf "# reading test file %s\n%!" test_filename; *)
  let tsl_block = tsl_block_of_file_safe test_filename in
  let (rootenv_statements, test_trees) = test_trees_of_tsl_block tsl_block in
  let test_trees = match test_trees with
    | [] ->
      let default_tests = Tests.default_tests() in
      let make_tree test = Node ([], test, [], []) in
      List.map make_tree default_tests
    | _ -> test_trees in
  let actions = actions_in_tests (tests_in_trees test_trees) in
  let test_dirname = Filename.dirname test_filename in
  let test_basename = Filename.basename test_filename in
  let test_prefix = Filename.chop_extension test_basename in
  let test_directory =
    if test_dirname="." then test_prefix
    else Filename.concat test_dirname test_prefix in
  let test_source_directory = get_test_source_directory test_dirname in
  let test_build_directory = get_test_build_directory test_directory in
  let reference_filename = Filename.concat
    test_source_directory (test_prefix ^ ".reference") in
  let initial_environment = Environments.from_bindings
  [
    Builtin_variables.c_preprocessor, Ocamltest_config.c_preprocessor;
    Builtin_variables.ocamlc_default_flags,
      Ocamltest_config.ocamlc_default_flags;
    Builtin_variables.ocamlopt_default_flags,
      Ocamltest_config.ocamlopt_default_flags;
    Builtin_variables.test_file, test_basename;
    Builtin_variables.reference, reference_filename;
    Builtin_variables.test_source_directory, test_source_directory;
    Builtin_variables.test_build_directory, test_build_directory;
  ] in
  let root_environment =
    interprete_environment_statements initial_environment rootenv_statements in
  let rootenv = Actions.update_environment root_environment actions in
  Testlib.make_directory test_build_directory;
  Sys.chdir test_build_directory;
  let log_filename = test_prefix ^ ".log" in
  let log = open_out log_filename in
  let common_prefix = " ... testing '" ^ test_basename ^ "' with" in
  List.iteri
    (run_test_i log common_prefix "" (Actions.Pass rootenv))
    test_trees;
  close_out log

let _ = main()
