(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
val stdlib_dir : Pathname.t Lazy.t
val module_name_of_filename : Pathname.t -> string
val module_name_of_pathname : Pathname.t -> string
val ignore_stdlib : string -> bool
val non_dependency : string -> string -> unit
val expand_module :
  Pathname.t list -> Pathname.t -> string list -> Pathname.t list
val string_list_of_file : string -> string list
val ocaml_ppflags : Tags.t -> Command.spec
val ocaml_include_flags : Pathname.t -> Command.spec
val libraries_of : Pathname.t -> Pathname.t list
val use_lib : Pathname.t -> Pathname.t -> unit
val cmi_of : Pathname.t -> Pathname.t
val ocaml_add_include_flag : string -> Command.spec list -> Command.spec list
val flag_and_dep : Tags.elt list -> Command.spec -> unit
val pflag_and_dep : Tags.elt list -> Tags.elt -> (string -> Command.spec) ->
  unit

exception Ocamldep_error of string

(* Takes a path and returns a list of modules *)
val path_dependencies_of : Pathname.t -> ([ `mandatory | `just_try ] * string) list

val info_libraries : (string, string * bool) Hashtbl.t

val ocaml_lib :
  ?extern:bool ->
  ?byte:bool ->
  ?native:bool ->
  ?dir:Pathname.t ->
  ?tag_name:string ->
  Pathname.t -> unit
