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

(* Definition of variables used by built-in actions *)

(* The variables are listed in alphabetical order *)

(*
  The name of the identifier representing a variable and its string name
  should be similar. Is there a way to enforce this?
*)

open Variables (* Should not be necessary with a ppx *)

let arguments = make ("arguments",
  "Arguments passed to executed programs and scripts")

let c_preprocessor = make ("c_preprocessor",
  "Command to use to invoke the C preprocessor")

let compiler_directory_suffix = make ("compiler_directory_suffix",
  "Suffix to add to the directory where the test will be compiled")

let compiler_reference = make ("compiler_reference",
  "Reference file for compiler output for ocamlc.byte and ocamlopt.byte")

let compiler_reference2 = make ("compiler_reference2",
  "Reference file for compiler output for ocamlc.opt and ocamlopt.opt")

let compiler_reference_suffix = make ("compiler_reference_suffix",
  "Suffix to add to the file name containing the reference for compiler output")

let compiler_output = make ("compiler_output",
  "Where to log output of bytecode compilers")

let compiler_output2 = make ("compiler_output2",
  "Where to log output of native compilers")

let ocamlc_flags = make ("ocamlc_flags",
  "Flags passed to ocamlc.byte and ocamlc.opt")

let ocamlc_default_flags = make ("ocamlc_default_flags",
  "Flags passed by default to ocamlc.byte and ocamlc.opt")

let files = make ("files",
  "Files used by the tests")

let flags = make ("flags",
  "Flags passed to all the compilers")

let libraries = make ("libraries",
  "Libraries the program should be linked with")

let modules = make ("modules",
  "Other modules of the test")

let ocamlopt_flags = make ("ocamlopt_flags",
  "Flags passed to ocamlopt.byte and ocamlopt.opt")

let ocamlopt_default_flags = make ("ocamlopt_default_flags",
  "Flags passed by default to ocamlopt.byte and ocamlopt.opt")

let ocaml_byte_exit_status = make ("ocaml_byte_exit_status",
  "Expected exit status of ocaml.byte")

let ocamlc_byte_exit_status = make ("ocamlc_byte_exit_status",
  "Expected exit status of ocac.byte")

let ocamlopt_byte_exit_status = make ("ocamlopt_byte_exit_status",
  "Expected exit status of ocamlopt.byte")

let ocaml_opt_exit_status = make ("ocaml_opt_exit_status",
  "Expected exit status of ocaml.opt")

let ocamlc_opt_exit_status = make ("ocamlc_opt_exit_status",
  "Expected exit status of ocac.opt")

let ocamlopt_opt_exit_status = make ("ocamlopt_opt_exit_status",
  "Expected exit status of ocamlopt.opt")

let output = make ("output",
  "Where the output of executing the program is saved")

let program = make ("program",
  "Name of program produced by ocamlc.byte and ocamlopt.byte")
let program2 = make ("program2",
  "Name of program produced by ocamlc.opt and ocamlopt.opt")

let reference = make ("reference",
  "Path of file to which program output should be compared")

let script = make ("script",
  "External script to run")

let stdin = make ("stdin", "Default standard input")
let stdout = make ("stdout", "Default standard output")
let stderr = make ("stderr", "Default standard error")

let test_build_directory = make ("test_build_directory",
  "Directory for files produced during a test")

let test_file = make ("test_file",
  "Name of file containing the specification of which tests to run")

let test_source_directory = make ("test_source_directory",
  "Directory containing the test source files")

let _ = List.iter register_variable
  [
    c_preprocessor;
    ocamlc_default_flags;
    ocamlopt_default_flags
  ]
