/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1997 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/*** Streams and parsers. */

/** The type of streams holding values of type ['a]. */
type t<'a>

/** Raised by parsers when none of the first components of the stream
   patterns is accepted. */
exception Failure

/** Raised by parsers when the first component of a stream pattern is
   accepted, but one of the following components is rejected. */
exception Error(string)

/*** {1 Stream builders} */

/** [Stream.from f] returns a stream built from the function [f].
   To create a new stream element, the function [f] is called with
   the current stream count. The user function [f] must return either
   [Some <value>] for a value or [None] to specify the end of the
   stream.

   Do note that the indices passed to [f] may not start at [0] in the
   general case. For example, [[< '0; '1; Stream.from f >]] would call
   [f] the first time with count [2].
*/
let from: (int => option<'a>) => t<'a>

/** Return the stream holding the elements of the list in the same
   order. */
let of_list: list<'a> => t<'a>

/** Return the stream of the characters of the string parameter. */
let of_string: string => t<char>

/** Return the stream of the characters of the bytes parameter.
    @since 4.02.0 */
let of_bytes: bytes => t<char>

/* {1 Stream iterator} */

/** [Stream.iter f s] scans the whole stream s, applying function [f]
   in turn to each stream element encountered. */
let iter: ('a => unit, t<'a>) => unit

/* {1 Predefined parsers} */

/** Return the first element of the stream and remove it from the
   stream. Raise {!Stream.Failure} if the stream is empty. */
let next: t<'a> => 'a

/** Return [()] if the stream is empty, else raise {!Stream.Failure}. */
let empty: t<'a> => unit

/* {1 Useful functions} */

/** Return [Some] of "the first element" of the stream, or [None] if
   the stream is empty. */
let peek: t<'a> => option<'a>

/** Remove the first element of the stream, possibly unfreezing
   it before. */
let junk: t<'a> => unit

/** Return the current count of the stream elements, i.e. the number
   of the stream elements discarded. */
let count: t<'a> => int

/** [npeek n] returns the list of the [n] first elements of
   the stream, or all its remaining elements if less than [n]
   elements are available. */
let npeek: (int, t<'a>) => list<'a>

/* The following is for system use only. Do not call directly. */

let iapp: (t<'a>, t<'a>) => t<'a>
let icons: ('a, t<'a>) => t<'a>
let ising: 'a => t<'a>

let lapp: (unit => t<'a>, t<'a>) => t<'a>
let lcons: (unit => 'a, t<'a>) => t<'a>
let lsing: (unit => 'a) => t<'a>

let sempty: t<'a>
let slazy: (unit => t<'a>) => t<'a>

let dump: ('a => unit, t<'a>) => unit
