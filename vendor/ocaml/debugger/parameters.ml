(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Jerome Vouillon, projet Cristal, INRIA Rocquencourt          *)
(*           OCaml port by John Malecki and Xavier Leroy                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Miscellaneous parameters *)

open Primitives
open Config
open Debugger_config

let program_name = ref ""
let socket_name = ref ""
let arguments = ref ""

let default_load_path =
  ref [ Filename.current_dir_name; Config.standard_library ]

let add_path dir =
  load_path := dir :: except dir !load_path;
  Envaux.reset_cache()

let add_path_for mdl dir =
  let old = try Hashtbl.find load_path_for mdl with Not_found -> [] in
  Hashtbl.replace load_path_for mdl (dir :: old)

(* Used by emacs ? *)
let emacs = ref false

let machine_readable = ref false
