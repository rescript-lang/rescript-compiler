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
open My_std

open Pathname
type resource_pattern
type env

type t = Pathname.t
module Resources : Set.S with type elt = t

module Cache :
  sig
    type cache_entry
    type suspension

    type build_status =
      | Bbuilt
      | Bcannot_be_built
      | Bnot_built_yet
      | Bsuspension of suspension

    val clean : unit -> unit
    val resource_state : t -> build_status
    val resource_changed : t -> unit
    val resource_has_changed : t -> bool
    val resource_built : t -> unit
    val resource_failed : t -> unit
    val import_in_build_dir : t -> unit
    val suspend_resource : t -> Command.t -> (unit -> unit) -> t list -> unit
    val resume_resource : t -> unit
    val resume_suspension : suspension -> unit
    val get_optional_resource_suspension : t -> (Command.t * (unit -> unit)) option
    val clear_resource_failed : t -> unit
    val add_dependency : t -> t -> unit
    val fold_dependencies : (string -> string -> 'a -> 'a) -> 'a -> 'a
    val external_is_up_to_date : t -> bool

    (* These are not currently used by others modules. *)
    val dependencies : t -> Resources.t
    val print_cache : Format.formatter -> unit -> unit
    val print_dependencies : Format.formatter -> unit -> unit
  end

val digest : t -> string
val exists_in_source_dir : t -> bool
val exists_in_build_dir : t -> bool
val in_build_dir : t -> t
val in_source_dir : t -> t

val clean_up_links : bool Slurp.entry -> bool Slurp.entry

val compare : t -> t -> int
val print : Format.formatter -> t -> unit
val print_pattern : Format.formatter -> resource_pattern -> unit
val clean : t -> unit
val import : string -> t
val import_pattern : string -> resource_pattern

val matchit : resource_pattern -> t -> env option
val subst : env -> t -> t
val subst_any : env -> t -> t
val subst_pattern : env -> resource_pattern -> t
(* val is_up_to_date : t -> bool *)
val print_env : Format.formatter -> env -> unit
