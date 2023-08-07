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

/*** Operations on internal representations of values.

   Not for the casual user.
*/

type t

external repr: 'a => t = "%identity"
external obj: t => 'a = "%identity"
external magic: 'a => 'b = "%identity"
@inline(always) let is_block: t => bool

external tag: t => int = "?obj_tag"
/**
     Computes the total size (in words, including the headers) of all
     heap blocks accessible from the argument.  Statically
     allocated blocks are excluded.

     @Since 4.04
*/
external size: t => int = "#obj_length"

external field: (t, int) => t = "%obj_field"

/** When using flambda:

    [set_field] MUST NOT be called on immutable blocks.  (Blocks allocated
    in C stubs, or with [new_block] below, are always considered mutable.)
    
    For experts only:
    [set_field] et al can be made safe by first wrapping the block in
    {!Sys.opaque_identity}, so any information about its contents will not
    be propagated.
*/
external set_field: (t, int, t) => unit = "%obj_set_field"
external dup: t => t = "?obj_dup"
