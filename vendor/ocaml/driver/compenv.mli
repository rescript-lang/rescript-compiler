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

val module_of_filename : Format.formatter -> string -> string -> string

val output_prefix : string -> string
val extract_output : string option -> string
val default_output : string option -> string

val print_version_and_library : string -> 'a
val print_version_string : unit -> 'a
val print_standard_library : unit -> 'a
val fatal : string -> 'a

val first_ccopts : string list ref
val first_ppx : string list ref
val first_include_dirs : string list ref
val last_include_dirs : string list ref
val implicit_modules : string list ref

(* function to call on plugin=XXX *)
val load_plugin : (string -> unit) ref

(* return the list of objfiles, after OCAMLPARAM and List.rev *)
val get_objfiles : with_ocamlparam:bool -> string list
val last_objfiles : string list ref
val first_objfiles : string list ref

type filename = string

type readenv_position =
  Before_args | Before_compile of filename | Before_link

val readenv : Format.formatter -> readenv_position -> unit

(* [is_unit_name name] returns true only if [name] can be used as a
   correct module name *)
val is_unit_name : string -> bool
(* [check_unit_name ppf filename name] prints a warning in [filename]
   on [ppf] if [name] should not be used as a module name. *)
val check_unit_name : Format.formatter -> string -> string -> unit

(* Deferred actions of the compiler, while parsing arguments *)

type deferred_action =
  | ProcessImplementation of string
  | ProcessInterface of string
  | ProcessCFile of string
  | ProcessOtherFile of string
  | ProcessObjects of string list
  | ProcessDLLs of string list

val c_object_of_filename : string -> string

val defer : deferred_action -> unit
val anonymous : string -> unit
val impl : string -> unit
val intf : string -> unit

val process_deferred_actions :
  Format.formatter *
  (Format.formatter -> string -> string -> unit) * (* compile implementation *)
  (Format.formatter -> string -> string -> unit) * (* compile interface *)
  string * (* ocaml module extension *)
  string -> (* ocaml library extension *)
  unit
