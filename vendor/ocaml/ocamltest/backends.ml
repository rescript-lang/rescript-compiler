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

(* Backends of the OCaml compiler and their properties *)

type t = Sys.backend_type

let string_of_backend = function
  | Sys.Bytecode -> "bytecode"
  | Sys.Native -> "native"
  | Sys.Other backend_name -> backend_name

(* Creates a function that returns its first argument for Bytecode,          *)
(* its second argument for Native code and fails for other backends          *)
let make_backend_function bytecode_value native_value = function
  | Sys.Bytecode -> bytecode_value
  | Sys.Native -> native_value
  | Sys.Other backend_name ->
    let error_message =
      ("Other backend " ^ backend_name ^ " not supported") in
    raise (Invalid_argument error_message)

let module_extension = make_backend_function "cmo" "cmx"

let library_extension = make_backend_function "cma" "cmxa"

let executable_extension = make_backend_function "byte" "opt"
