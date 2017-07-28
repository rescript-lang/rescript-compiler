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
(* Configuration *)

(** Handles the "_tags" file mechanism. *)

(** Incorporate a newline-separated configuration string into the current configuration.
    Will usually raising an [Invalid_arg] with an appropriately explicit message in case of error. *)
val parse_string : ?source:Loc.source -> string -> unit

(** [parse_file ?dir fn] incorporates the configuration file named [fn], prefixing its glob patterns
    with [dir] if given. *)
val parse_file : ?dir:string -> string -> unit

(** Return the set of tags that apply to a given filename under the current configuration. *)
val tags_of_filename : string -> Tags.t

val has_tag : string -> bool

(** [tag_file filename tag_list] Tag the given filename with all given tags. *)
val tag_file : Pathname.t -> Tags.elt list -> unit

(** [tag_any tag_list] Tag anything with all given tags. *)
val tag_any : Tags.elt list -> unit

(** the tags that apply to any file *)
val global_tags : unit -> Tags.t

(** Given the list of all tags that are really used by an existing
    flagset, traverse existing configuration files and warns on tags
    that will never get used. *)
val check_tags_usage : Tags.t -> unit
