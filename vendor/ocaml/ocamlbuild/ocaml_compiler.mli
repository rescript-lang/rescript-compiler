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

val forpack_flags : string -> Tags.t -> Command.spec
val ocamlc_c : Tags.t -> Pathname.t -> Pathname.t -> Command.t
val ocamlc_link_lib  : Tags.t -> Pathname.t list -> Pathname.t -> Command.t
val ocamlc_link_prog : Tags.t -> Pathname.t list -> Pathname.t -> Command.t
val ocamlc_p : Tags.t -> Pathname.t list -> Pathname.t -> Command.t
val ocamlopt_c : Tags.t -> Pathname.t -> Pathname.t -> Command.t
val ocamlopt_link_lib : Tags.t -> Pathname.t list -> Pathname.t -> Command.t
val ocamlopt_link_shared_lib : Tags.t -> Pathname.t list -> Pathname.t -> Command.t
val ocamlopt_link_prog : Tags.t -> Pathname.t list -> Pathname.t -> Command.t
val ocamlopt_p : Tags.t -> Pathname.t list -> Pathname.t -> Command.t
val ocamlmklib : Tags.t -> Pathname.t list -> Pathname.t -> Command.t
val ocamlmktop : Tags.t -> Pathname.t list -> Pathname.t -> Command.t
val prepare_compile : Rule.builder -> Pathname.t -> unit
val compile_ocaml_interf : string -> string -> Rule.action
val byte_compile_ocaml_interf : string -> string -> Rule.action
val byte_compile_ocaml_implem : ?tag:string -> string -> string -> Rule.action
val prepare_link :
  Pathname.t -> Pathname.t ->
  string list -> Rule.builder -> unit
val native_compile_ocaml_implem : ?tag:string -> ?cmx_ext:string -> string -> Rule.action
val prepare_libs :
  string -> string -> Pathname.t ->
  Rule.builder -> Pathname.t list
val link_gen :
  string -> string -> string -> string list ->
  (Tags.t -> Pathname.t list -> Pathname.t -> Command.t) ->
  (Tags.t -> Tags.t) ->
  string -> string -> Rule.action
val byte_link : string -> string -> Rule.action
val byte_output_obj : string -> string -> Rule.action
val byte_output_shared : string -> string -> Rule.action
val byte_library_link : string -> string -> Rule.action
val byte_debug_link : string -> string -> Rule.action
val byte_debug_library_link : string -> string -> Rule.action
val native_link : string -> string -> Rule.action
val native_output_obj : string -> string -> Rule.action
val native_output_shared : string -> string -> Rule.action
val native_library_link : string -> string -> Rule.action
val native_shared_library_link : ?tags:(string list) -> string -> string -> Rule.action
val native_profile_link : string -> string -> Rule.action
val native_profile_library_link : string -> string -> Rule.action
val link_modules :
  (Pathname.t * string list) list ->
  string -> string ->
  string -> (Tags.t -> Pathname.t list -> Pathname.t -> Command.t) ->
  (Tags.t -> Tags.t) ->
  string list -> string -> Rule.action
val pack_modules :
  (Pathname.t * string list) list ->
  string -> string ->
  string -> (Tags.t -> Pathname.t list -> Pathname.t -> Command.t) ->
  (Tags.t -> Tags.t) ->
  string list -> string -> Rule.action
val byte_library_link_modules : string list -> string -> Rule.action
val byte_library_link_mllib : string -> string -> Rule.action
val byte_debug_library_link_modules : string list -> string -> Rule.action
val byte_debug_library_link_mllib : string -> string -> Rule.action
val byte_pack_modules : string list -> string -> Rule.action
val byte_pack_mlpack : string -> string -> Rule.action
val byte_debug_pack_modules : string list -> string -> Rule.action
val byte_debug_pack_mlpack : string -> string -> Rule.action
val byte_toplevel_link_modules : string list -> string -> Rule.action
val byte_toplevel_link_mltop : string -> string -> Rule.action
val native_pack_modules : string list -> string -> Rule.action
val native_pack_mlpack : string -> string -> Rule.action
val native_library_link_modules : string list -> string -> Rule.action
val native_library_link_mllib : string -> string -> Rule.action
val native_shared_library_link_modules : string list -> string -> Rule.action
val native_shared_library_link_mldylib : string -> string -> Rule.action
val native_profile_pack_modules : string list -> string -> Rule.action
val native_profile_pack_mlpack : string -> string -> Rule.action
val native_profile_library_link_modules : string list -> string -> Rule.action
val native_profile_library_link_mllib : string -> string -> Rule.action
val native_profile_shared_library_link_modules : string list -> string -> Rule.action
val native_profile_shared_library_link_mldylib : string -> string -> Rule.action

(** [hide_package_contents pack_name]
    Don't treat the given package as an open package.
    So a module will not be replaced during linking by
    this package even if it contains that module. *)
val hide_package_contents : string -> unit
