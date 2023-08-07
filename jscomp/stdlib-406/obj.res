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

/* Operations on internal representations of values */

type t

external repr: 'a => t = "%identity"
external obj: t => 'a = "%identity"
external magic: 'a => 'b = "%identity"
external is_int: t => bool = "%obj_is_int"
@inline(always) let is_block = a => !is_int(a)
external tag: t => int = "?obj_tag"
external size: t => int = "#obj_length"
external field: (t, int) => t = "%obj_field"
external set_field: (t, int, t) => unit = "%obj_set_field"
external dup: t => t = "?obj_dup"
