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

(* Types of input files involved in an OCaml project and related functions *)

type t =
  | Implementation
  | Interface
  | C
  | C_minus_minus
  | Lexer
  | Grammar

let string_of_filetype = function
  | Implementation -> "implementation"
  | Interface -> "interface"
  | C -> "C source file"
  | C_minus_minus -> "C minus minus source file"
  | Lexer -> "lexer"
  | Grammar -> "grammar"

let extension_of_filetype = function
  | Implementation -> "ml"
  | Interface -> "mli"
  | C -> "c"
  | C_minus_minus -> "cmm"
  | Lexer -> "mll"
  | Grammar -> "mly"

let filetype_of_extension = function
  | "ml" -> Implementation
  | "mli" -> Interface
  | "c" -> C
  | "cmm" -> C_minus_minus
  | "mll" -> Lexer
  | "mly" -> Grammar
  | _ -> raise Not_found

let split_filename name =
  let l = String.length name in
  let is_dir_sep name i = name.[i] = Filename.dir_sep.[0] in
  let rec search_dot i =
    if i < 0 || is_dir_sep name i then (name, "")
    else if name.[i] = '.' then
      let basename = String.sub name 0 i in
      let extension = String.sub name (i+1) (l-i-1) in
      (basename, extension)
    else search_dot (i - 1) in
  search_dot (l - 1)

let filetype filename =
  let (basename, extension) = split_filename filename in
  (basename, filetype_of_extension extension)

let make_filename (basename, filetype) =
  let extension = extension_of_filetype filetype in
  basename ^ "." ^ extension
