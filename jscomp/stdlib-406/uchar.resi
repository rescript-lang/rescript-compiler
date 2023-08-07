@@ocaml.text(/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Daniel C. Buenzli */
/*  */
/* Copyright 2014 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

" Unicode characters.

    @since 4.03 ")

@ocaml.doc(" The type for Unicode characters.

    A value of this type represents an Unicode
    {{:http://unicode.org/glossary/#unicode_scalar_value}scalar
    value} which is an integer in the ranges [0x0000]...[0xD7FF] or
    [0xE000]...[0x10FFFF]. ")
type t

@ocaml.doc(" [min] is U+0000. ")
let min: t

@ocaml.doc(" [max] is U+10FFFF. ")
let max: t

@ocaml.doc(" [bom] is U+FEFF, the
    {{:http://unicode.org/glossary/#byte_order_mark}byte order mark} (BOM)
    character.

    @since 4.06.0 ")
let bom: t

@ocaml.doc(" [rep] is U+FFFD, the
    {{:http://unicode.org/glossary/#replacement_character}replacement}
    character.

    @since 4.06.0 ")
let rep: t

@ocaml.doc(" [succ u] is the scalar value after [u] in the set of Unicode scalar
    values.

    @raise Invalid_argument if [u] is {!max}. ")
let succ: t => t

@ocaml.doc(" [pred u] is the scalar value before [u] in the set of Unicode scalar
    values.

    @raise Invalid_argument if [u] is {!min}. ")
let pred: t => t

@ocaml.doc(" [is_valid n] is [true] iff [n] is an Unicode scalar value
    (i.e. in the ranges [0x0000]...[0xD7FF] or [0xE000]...[0x10FFFF]).")
let is_valid: int => bool

@ocaml.doc(" [of_int i] is [i] as an Unicode character.

    @raise Invalid_argument if [i] does not satisfy {!is_valid}. ")
let of_int: int => t

@@ocaml.text("/*")
let unsafe_of_int: int => t
@@ocaml.text("/*")

@ocaml.doc(" [to_int u] is [u] as an integer. ")
let to_int: t => int

@ocaml.doc(" [is_char u] is [true] iff [u] is a latin1 OCaml character. ")
let is_char: t => bool

@ocaml.doc(" [of_char c] is [c] as an Unicode character. ")
let of_char: char => t

@ocaml.doc(" [to_char u] is [u] as an OCaml latin1 character.

    @raise Invalid_argument if [u] does not satisfy {!is_char}. ")
let to_char: t => char

@@ocaml.text("/*")
let unsafe_to_char: t => char
@@ocaml.text("/*")

@ocaml.doc(" [equal u u'] is [u = u']. ")
let equal: (t, t) => bool

@ocaml.doc(" [compare u u'] is [Pervasives.compare u u']. ")
let compare: (t, t) => int

@ocaml.doc(" [hash u] associates a non-negative integer to [u]. ")
let hash: t => int
