(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Character operations. *)

external code : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

val chr : int -> char
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "Char.chr"] if the argument is
   outside the range 0--255. *)

val escaped : char -> string
(** Return a string representing the given character,
    with special characters escaped following the lexical conventions
    of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash, double-quote, and single-quote. *)

val lowercase : char -> char
  [@@ocaml.deprecated "Use Char.lowercase_ascii instead."]
(** Convert the given character to its equivalent lowercase character,
   using the ISO Latin-1 (8859-1) character set.
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val uppercase : char -> char
  [@@ocaml.deprecated "Use Char.uppercase_ascii instead."]
(** Convert the given character to its equivalent uppercase character,
   using the ISO Latin-1 (8859-1) character set.
   @deprecated Functions operating on Latin-1 character set are deprecated. *)

val lowercase_ascii : char -> char
(** Convert the given character to its equivalent lowercase character,
   using the US-ASCII character set.
   @since 4.03.0 *)

val uppercase_ascii : char -> char
(** Convert the given character to its equivalent uppercase character,
   using the US-ASCII character set.
   @since 4.03.0 *)

type t = char
(** An alias for the type of characters. *)

val compare: t -> t -> int
(** The comparison function for characters, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val equal: t -> t -> bool
(** The equal function for chars.
    @since 4.03.0 *)

(**/**)

(* The following is for system use only. Do not call directly. *)

external unsafe_chr : int -> char = "%identity"
