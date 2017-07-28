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
(** OCaml dependencies *)

exception Circular_dependencies of string list * string

(** Give to this module a way to access libraries, packages,
    and dependencies between files. *)
module type INPUT = sig
  val fold_dependencies : (string -> string -> 'a -> 'a) -> 'a -> 'a
  val fold_libraries : (string -> string list -> 'a -> 'a) -> 'a -> 'a
  val fold_packages : (string -> string list -> 'a -> 'a) -> 'a -> 'a
end

(** Wait an [INPUT] module and gives a function to compute the
    transitive closure of caml file takeing in account libraries and packages. *)
module Make (I : INPUT) : sig

  (** [caml_transitive_closure] takes a list of root ocaml compiled files and returns
      the list of files that must be given to a linker. Optionally you can change the
      extension of caml object/library files (cmo/cma by default); use the  pack mode
      (false by default) to include only root files (just sort them); and gives  the
      list of used libraries (empty by default). *)
  val caml_transitive_closure :
    ?caml_obj_ext:string ->
    ?caml_lib_ext:string ->
    ?pack_mode:bool ->
    ?used_libraries:string list ->
    ?hidden_packages:string list ->
    Pathname.t list -> Pathname.t list

end
