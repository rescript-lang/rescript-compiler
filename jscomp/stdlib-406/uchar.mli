(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                           Daniel C. Buenzli                            *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Unicode characters.

    @since 4.03 *)

type t
(** The type for Unicode characters.

    A value of this type represents an Unicode
    {{:http://unicode.org/glossary/#unicode_scalar_value}scalar
    value} which is an integer in the ranges [0x0000]...[0xD7FF] or
    [0xE000]...[0x10FFFF]. *)

val min : t (* [@@dead "min"] *)
(** [min] is U+0000. *)

val max : t (* [@@dead "max"] *)
(** [max] is U+10FFFF. *)

val bom : t (* [@@dead "bom"] *)
(** [bom] is U+FEFF, the
    {{:http://unicode.org/glossary/#byte_order_mark}byte order mark} (BOM)
    character.

    @since 4.06.0 *)

val rep : t (* [@@dead "rep"] *)
(** [rep] is U+FFFD, the
    {{:http://unicode.org/glossary/#replacement_character}replacement}
    character.

    @since 4.06.0 *)

val succ : t -> t (* [@@dead "succ"] *)
(** [succ u] is the scalar value after [u] in the set of Unicode scalar
    values.

    @raise Invalid_argument if [u] is {!max}. *)

val pred : t -> t (* [@@dead "pred"] *)
(** [pred u] is the scalar value before [u] in the set of Unicode scalar
    values.

    @raise Invalid_argument if [u] is {!min}. *)

val is_valid : int -> bool (* [@@dead "is_valid"] *)
(** [is_valid n] is [true] iff [n] is an Unicode scalar value
    (i.e. in the ranges [0x0000]...[0xD7FF] or [0xE000]...[0x10FFFF]).*)

val of_int : int -> t (* [@@dead "of_int"] *)
(** [of_int i] is [i] as an Unicode character.

    @raise Invalid_argument if [i] does not satisfy {!is_valid}. *)

(**/**)
val unsafe_of_int : int -> t (* [@@dead "unsafe_of_int"] *)
(**/**)

val to_int : t -> int
(** [to_int u] is [u] as an integer. *)

val is_char : t -> bool (* [@@dead "is_char"] *)
(** [is_char u] is [true] iff [u] is a latin1 OCaml character. *)

val of_char : char -> t (* [@@dead "of_char"] *)
(** [of_char c] is [c] as an Unicode character. *)

val to_char : t -> char (* [@@dead "to_char"] *)
(** [to_char u] is [u] as an OCaml latin1 character.

    @raise Invalid_argument if [u] does not satisfy {!is_char}. *)

(**/**)
val unsafe_to_char : t -> char (* [@@dead "unsafe_to_char"] *)
(**/**)

val equal : t -> t -> bool (* [@@dead "equal"] *)
(** [equal u u'] is [u = u']. *)

val compare : t -> t -> int (* [@@dead "compare"] *)
(** [compare u u'] is [Pervasives.compare u u']. *)

val hash : t -> int (* [@@dead "hash"] *)
(** [hash u] associates a non-negative integer to [u]. *)