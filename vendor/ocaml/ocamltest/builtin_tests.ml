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

(* Definitions of built-in tests *)

open Tests
open Builtin_actions

let bytecode =
  let opt_actions =
  [
    compile_bytecode_with_native_compiler;
    check_ocamlc_dot_opt_output;
    compare_bytecode_programs
  ] in
{
  test_name = "bytecode";
  test_run_by_default = true;
  test_actions =
  [
    compile_bytecode_with_bytecode_compiler;
    check_ocamlc_dot_byte_output;
    execute;
    check_program_output
  ] @ (if Ocamltest_config.arch<>"none" then opt_actions else [])
}

let expect = {
  test_name = "expect";
  test_run_by_default = false;
  test_actions = [expect];
}

let native = {
  test_name = "native";
  test_run_by_default = true;
  test_actions =
  [
    compile_native_with_bytecode_compiler;
    check_ocamlopt_dot_byte_output;
    execute;
    check_program_output;
    compile_native_with_native_compiler;
    check_ocamlopt_dot_opt_output;
    compare_native_programs;
  ]
}

let script = {
  test_name = "script";
  test_run_by_default = false;
  test_actions = [script];
}

let toplevel = {
  test_name = "toplevel";
  test_run_by_default = false;
  test_actions =
  [
    run_in_ocaml;
    check_ocaml_output;
(*
    run_in_ocamlnat;
    check_ocamlnat_output;
*)
  ]
}

let _ =
  List.iter register
  [
    bytecode;
    expect;
    script;
    toplevel;
  ];
  if (Ocamltest_config.arch <> "none") then register native
