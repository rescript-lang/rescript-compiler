/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/*** Byte sequence operations.
    @since 4.02.0
*/

/** Return the length (number of bytes) of the argument. */
external length: bytes => int = "%bytes_length"

/** [get s n] returns the byte at index [n] in argument [s].

    Raise [Invalid_argument] if [n] is not a valid index in [s]. */
external get: (bytes, int) => char = "%bytes_safe_get"

/** [set s n c] modifies [s] in place, replacing the byte at index [n]
    with [c].

    Raise [Invalid_argument] if [n] is not a valid index in [s]. */
external set: (bytes, int, char) => unit = "%bytes_safe_set"

/** [create n] returns a new byte sequence of length [n]. The
    sequence is uninitialized and contains arbitrary bytes.

    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. */
external create: int => bytes = "?create_bytes"

/** [make n c] returns a new byte sequence of length [n], filled with
    the byte [c].

    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. */
let make: (int, char) => bytes

/** [init n f] returns a fresh byte sequence of length [n],
    with character [i] initialized to the result of [f i].

    Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. */
let init: (int, ~f: int => char) => bytes

/** A byte sequence of size 0. */
let empty: bytes

/** Return a new byte sequence that contains the same bytes as the
    argument. */
let copy: bytes => bytes

/** Return a new byte sequence that contains the same bytes as the
    given string. */
let of_string: string => bytes

/** Return a new string that contains the same bytes as the given byte
    sequence. */
let to_string: bytes => string

/** [sub s start len] returns a new byte sequence of length [len],
    containing the subsequence of [s] that starts at position [start]
    and has length [len].

    Raise [Invalid_argument] if [start] and [len] do not designate a
    valid range of [s]. */
let sub: (bytes, ~pos: int, ~len: int) => bytes

/** Same as [sub] but return a string instead of a byte sequence. */
let sub_string: (bytes, ~pos: int, ~len: int) => string

/** [extend s left right] returns a new byte sequence that contains
    the bytes of [s], with [left] uninitialized bytes prepended and
    [right] uninitialized bytes appended to it. If [left] or [right]
    is negative, then bytes are removed (instead of appended) from
    the corresponding side of [s].

    Raise [Invalid_argument] if the result length is negative or
    longer than {!Sys.max_string_length} bytes.
    @since 4.05.0 */
let extend: (bytes, ~left: int, ~right: int) => bytes

/** [fill s start len c] modifies [s] in place, replacing [len]
    characters with [c], starting at [start].

    Raise [Invalid_argument] if [start] and [len] do not designate a
    valid range of [s]. */
let fill: (bytes, ~pos: int, ~len: int, char) => unit

/** [blit src srcoff dst dstoff len] copies [len] bytes from sequence
    [src], starting at index [srcoff], to sequence [dst], starting at
    index [dstoff]. It works correctly even if [src] and [dst] are the
    same byte sequence, and the source and destination intervals
    overlap.

    Raise [Invalid_argument] if [srcoff] and [len] do not
    designate a valid range of [src], or if [dstoff] and [len]
    do not designate a valid range of [dst]. */
let blit: (~src: bytes, ~src_pos: int, ~dst: bytes, ~dst_pos: int, ~len: int) => unit

/** [blit src srcoff dst dstoff len] copies [len] bytes from string
    [src], starting at index [srcoff], to byte sequence [dst],
    starting at index [dstoff].

    Raise [Invalid_argument] if [srcoff] and [len] do not
    designate a valid range of [src], or if [dstoff] and [len]
    do not designate a valid range of [dst].
    @since 4.05.0 */
let blit_string: (~src: string, ~src_pos: int, ~dst: bytes, ~dst_pos: int, ~len: int) => unit

/** [concat sep sl] concatenates the list of byte sequences [sl],
    inserting the separator byte sequence [sep] between each, and
    returns the result as a new byte sequence. */
let concat: (~sep: bytes, list<bytes>) => bytes

/** [cat s1 s2] concatenates [s1] and [s2] and returns the result
     as new byte sequence.

    Raise [Invalid_argument] if the result is longer than
    {!Sys.max_string_length} bytes.
    @since 4.05.0 */
let cat: (bytes, bytes) => bytes

/** [iter f s] applies function [f] in turn to all the bytes of [s].
    It is equivalent to [f (get s 0); f (get s 1); ...; f (get s
    (length s - 1)); ()]. */
let iter: (~f: char => unit, bytes) => unit

/** Same as {!Bytes.iter}, but the function is applied to the index of
    the byte as first argument and the byte itself as second
    argument. */
let iteri: (~f: (int, char) => unit, bytes) => unit

/** [map f s] applies function [f] in turn to all the bytes of [s] and
    stores the resulting bytes in a new sequence that is returned as
    the result. */
let map: (~f: char => char, bytes) => bytes

/** [mapi f s] calls [f] with each character of [s] and its
    index (in increasing index order) and stores the resulting bytes
    in a new sequence that is returned as the result. */
let mapi: (~f: (int, char) => char, bytes) => bytes

/** Return a copy of the argument, without leading and trailing
    whitespace. The bytes regarded as whitespace are the ASCII
    characters [' '], ['\x0c'], ['\n'], ['\r'], and ['\t']. */
let trim: bytes => bytes

/** Return a copy of the argument, with special characters represented
    by escape sequences, following the lexical conventions of OCaml. */
let escaped: bytes => bytes

/** [index s c] returns the index of the first occurrence of byte [c]
    in [s].

    Raise [Not_found] if [c] does not occur in [s]. */
let index: (bytes, char) => int

/** [index_opt s c] returns the index of the first occurrence of byte [c]
    in [s] or [None] if [c] does not occur in [s].
    @since 4.05 */
let index_opt: (bytes, char) => option<int>

/** [rindex s c] returns the index of the last occurrence of byte [c]
    in [s].

    Raise [Not_found] if [c] does not occur in [s]. */
let rindex: (bytes, char) => int

/** [rindex_opt s c] returns the index of the last occurrence of byte [c]
    in [s] or [None] if [c] does not occur in [s].
    @since 4.05 */
let rindex_opt: (bytes, char) => option<int>

/** [index_from s i c] returns the index of the first occurrence of
    byte [c] in [s] after position [i].  [Bytes.index s c] is
    equivalent to [Bytes.index_from s 0 c].

    Raise [Invalid_argument] if [i] is not a valid position in [s].
    Raise [Not_found] if [c] does not occur in [s] after position [i]. */
let index_from: (bytes, int, char) => int

/** [index_from _opts i c] returns the index of the first occurrence of
    byte [c] in [s] after position [i] or [None] if [c] does not occur in [s] after position [i].
    [Bytes.index_opt s c] is equivalent to [Bytes.index_from_opt s 0 c].

    Raise [Invalid_argument] if [i] is not a valid position in [s].
    @since 4.05 */
let index_from_opt: (bytes, int, char) => option<int>

/** [rindex_from s i c] returns the index of the last occurrence of
    byte [c] in [s] before position [i+1].  [rindex s c] is equivalent
    to [rindex_from s (Bytes.length s - 1) c].

    Raise [Invalid_argument] if [i+1] is not a valid position in [s].
    Raise [Not_found] if [c] does not occur in [s] before position [i+1]. */
let rindex_from: (bytes, int, char) => int

/** [rindex_from_opt s i c] returns the index of the last occurrence
    of byte [c] in [s] before position [i+1] or [None] if [c] does not
    occur in [s] before position [i+1].  [rindex_opt s c] is equivalent to
    [rindex_from s (Bytes.length s - 1) c].

    Raise [Invalid_argument] if [i+1] is not a valid position in [s].
    @since 4.05 */
let rindex_from_opt: (bytes, int, char) => option<int>

/** [contains s c] tests if byte [c] appears in [s]. */
let contains: (bytes, char) => bool

/** [contains_from s start c] tests if byte [c] appears in [s] after
    position [start].  [contains s c] is equivalent to [contains_from
    s 0 c].

    Raise [Invalid_argument] if [start] is not a valid position in [s]. */
let contains_from: (bytes, int, char) => bool

/** [rcontains_from s stop c] tests if byte [c] appears in [s] before
    position [stop+1].

    Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
    position in [s]. */
let rcontains_from: (bytes, int, char) => bool

/** Return a copy of the argument, with all lowercase letters
   translated to uppercase, using the US-ASCII character set.
   @since 4.05.0 */
let uppercase_ascii: bytes => bytes

/** Return a copy of the argument, with all uppercase letters
   translated to lowercase, using the US-ASCII character set.
   @since 4.05.0 */
let lowercase_ascii: bytes => bytes

/** Return a copy of the argument, with the first character set to uppercase,
   using the US-ASCII character set.
   @since 4.05.0 */
let capitalize_ascii: bytes => bytes

/** Return a copy of the argument, with the first character set to lowercase,
   using the US-ASCII character set.
   @since 4.05.0 */
let uncapitalize_ascii: bytes => bytes

/** An alias for the type of byte sequences. */
type t = bytes

/** The comparison function for byte sequences, with the same
    specification as {!Pervasives.compare}.  Along with the type [t],
    this function [compare] allows the module [Bytes] to be passed as
    argument to the functors {!Set.Make} and {!Map.Make}. */
let compare: (t, t) => int

/** The equality function for byte sequences.
    @since 4.05.0 */
let equal: (t, t) => bool

/* The following is for system use only. Do not call directly. */

external unsafe_get: (bytes, int) => char = "%bytes_unsafe_get"
external unsafe_set: (bytes, int, char) => unit = "%bytes_unsafe_set"
let unsafe_to_string: bytes => string
let unsafe_of_string: string => bytes
