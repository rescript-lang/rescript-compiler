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

/** Return the length (number of characters) of the given string. */
external length: string => int = "%string_length"

/** [String.get s n] returns the character at index [n] in string [s].
   You can also write [s.[n]] instead of [String.get s n].

   Raise [Invalid_argument] if [n] not a valid index in [s]. */
external get: (string, int) => char = "%string_safe_get"

/** An alias for the type of strings. */
type t = string

/** The comparison function for strings, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [String] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. */
let compare: (t, t) => int

/** The equal function for strings.
    @since 4.03.0 */
let equal: (t, t) => bool

/* The following is for system use only. Do not call directly. */

external unsafe_get: (string, int) => char = "%string_unsafe_get"
