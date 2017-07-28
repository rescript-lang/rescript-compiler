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
(* Command *)

(** Provides an abstract type for easily building complex shell commands without making
    quotation mistakes.  *)
include Signatures.COMMAND with type tags = Tags.t and type pathname = string

(** {6 For system use only, not for the casual user} *)

val string_target_and_tags_of_command_spec : spec -> string * string * Tags.t

val iter_tags : (Tags.t -> unit) -> t -> unit

val fold_pathnames : (pathname -> 'a -> 'a) -> t -> 'a -> 'a

(** Digest the given command. *)
val digest : t -> Digest.t

(** Maximum number of parallel jobs. *)
val jobs : int ref

(** Hook here the function that maps a set of tags to appropriate command
    options. It also build the dependencies that matches the tags. *)
val tag_handler : (Tags.t -> spec) ref

(** For system use only *)
val dump_parallel_stats : unit -> unit

val deps_of_tags : Tags.t -> pathname list

(** [dep tags deps] Will build [deps] when [tags] will be activated. *)
val dep : Tags.elt list -> pathname list -> unit

val pdep : Tags.elt list -> Tags.elt -> (string -> pathname list) -> unit

val list_all_deps : unit -> (Tags.t * pathname list) list

val file_or_exe_exists: string -> bool
