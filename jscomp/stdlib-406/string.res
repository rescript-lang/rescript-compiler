/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Damien Doligez, projet Gallium, INRIA Rocquencourt */
/*  */
/* Copyright 2014 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* String operations, based on byte sequence operations */

/* WARNING: Some functions in this file are duplicated in bytes.ml for
   efficiency reasons. When you modify the one in this file you need to
   modify its duplicate in bytes.ml.
   These functions have a "duplicated" comment above their definition.
*/

external length: string => int = "%string_length"
external get: (string, int) => char = "%string_safe_get"
external unsafe_get: (string, int) => char = "%string_unsafe_get"

type t = string

let compare = (x: t, y: t) => Pervasives.compare(x, y)
let equal: (string, string) => bool = (a, b) => a == b
