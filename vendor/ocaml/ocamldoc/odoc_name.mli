(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Maxence Guesdon, projet Cristal, INRIA Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation of element names. *)

type t = string

(** Add parenthesis to the given simple name if needed. *)
val parens_if_infix : t -> t

(** Return a simple name from a name.*)
val simple : t -> t

(** Return the name of the 'father' (like dirname for a file name).*)
val father : t -> t

(** Concatenates two names. *)
val concat : t -> t -> t

(** Normalize the given name by removing the beginning and ending spaces
     of the simple name and adding parenthesis if needed. *)
val normalize_name : t -> t

(** Returns the head of a name. *)
val head : t -> t

(** Returns the depth of the name, i.e. the number of levels to the root.
   Example : [Toto.Tutu.name] has depth 3. *)
val depth : t -> int

(** Returns true if the first name is a prefix of the second name.
   If the two names are equal, then it is false (strict prefix).*)
val prefix : t -> t -> bool

(** Take two names n1 and n2 = n3.n4 and return n4 if n3=n1 or else n2. *)
val get_relative : t -> t -> t

(** Take two names n1=n3.n4 and n2 = n5.n6 and return n6 if n3=n5 or else n2. *)
val get_relative_raw : t -> t -> t

(** Take a list of module names to hide and a name,
   and return the name when the module name (or part of it)
   was removed, according to the list of module names to hide.*)
val hide_given_modules : t list -> t -> t

(** Indicate if a name if qualified or not. *)
val qualified : t -> bool

(** Get a name from an [Ident.t]. *)
val from_ident : Ident.t -> t

(** Get a name from a [Path.t]. *)
val from_path : Path.t -> t

(** Get a [Path.t] from a name.*)
val to_path : t -> Path.t

(** Get a name from a [Longident.t].*)
val from_longident : Longident.t -> t

module Map : Map.S with type key = t
