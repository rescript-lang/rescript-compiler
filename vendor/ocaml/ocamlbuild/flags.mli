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
val of_tags : Tags.t -> Command.spec
val of_tag_list : Tags.elt list -> Command.spec

(* The ?deprecated parameter marks the flag declaration as deprecated,
   because it is superseded by a different, better way to express the
   same thing (eg. a parametrized tag). So far, it is only used when
   showing documentation.

   This flag is not exported in OCamlbuild_plugin interface for now. It
   would make sense to let plugin authors deprecate their own flags,
   but it has to be balanced again the simplicity of the plugin
   interface exposed. If you're reading this as a plugin author that
   has a real need for deprecation, drop us a note on the bugtracker. *)
val flag : ?deprecated:bool -> Tags.elt list -> Command.spec -> unit

val pflag : Tags.elt list -> string -> (string -> Command.spec) -> unit
val add : 'a -> 'a list -> 'a list
val remove : 'a -> 'a list -> 'a list

val show_documentation : unit -> unit

(** "useful" tags: they are used by a tag declaration, or have been
    explicitly added with [mark_as_used] *)
val get_used_tags : unit -> Tags.t

val mark_tag_used : Tags.elt -> unit
