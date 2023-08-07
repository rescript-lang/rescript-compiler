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

/*** MD5 message digest.

   This module provides functions to compute 128-bit 'digests' of
   arbitrary-length strings or files. The digests are of cryptographic
   quality: it is very hard, given a digest, to forge a string having
   that digest. The algorithm used is MD5. This module should not be
   used for secure and sensitive cryptographic applications. For these
   kind of applications more recent and stronger cryptographic
   primitives should be used instead.
*/

/** The type of digests: 16-character strings. */
type t = string

/** The comparison function for 16-character digest, with the same
    specification as {!Pervasives.compare} and the implementation
    shared with {!String.compare}. Along with the type [t], this
    function [compare] allows the module [Digest] to be passed as
    argument to the functors {!Set.Make} and {!Map.Make}.
    @since 4.00.0 */
let compare: (t, t) => int

/** The equal function for 16-character digest.
    @since 4.03.0 */
let equal: (t, t) => bool

/** Return the digest of the given string. */
let string: string => t

/** Return the digest of the given byte sequence.
    @since 4.02.0 */
let bytes: bytes => t

/** [Digest.substring s ofs len] returns the digest of the substring
   of [s] starting at index [ofs] and containing [len] characters. */
let substring: (string, int, int) => t

/** [Digest.subbytes s ofs len] returns the digest of the subsequence
    of [s] starting at index [ofs] and containing [len] bytes.
    @since 4.02.0 */
let subbytes: (bytes, int, int) => t

/** Return the printable hexadecimal representation of the given digest.
    Raise [Invalid_argument] if the argument is not exactly 16 bytes.
 */
let to_hex: t => string

/** Convert a hexadecimal representation back into the corresponding digest.
   Raise [Invalid_argument] if the argument is not exactly 32 hexadecimal
   characters.
   @since 4.00.0 */
let from_hex: string => t
