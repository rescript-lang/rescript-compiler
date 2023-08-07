@@ocaml.text(
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

  " String operations. "
)

@ocaml.doc(" Return the length (number of characters) of the given string. ")
external length: string => int = "%string_length"

@ocaml.doc(" [String.get s n] returns the character at index [n] in string [s].
   You can also write [s.[n]] instead of [String.get s n].

   Raise [Invalid_argument] if [n] not a valid index in [s]. ")
external get: (string, int) => char = "%string_safe_get"

@ocaml.doc(" [String.make n c] returns a fresh string of length [n],
   filled with the character [c].

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. ")
let make: (int, char) => string

@ocaml.doc(" [init n f] returns a string of length [n],
    with character [i] initialized to the result of [f i].

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}.
   @since 4.02.0 ")
let init: (int, ~f: int => char) => string

@ocaml.doc(" [String.sub s start len] returns a fresh string of length [len],
   containing the substring of [s] that starts at position [start] and
   has length [len].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid substring of [s]. ")
let sub: (string, ~pos: int, ~len: int) => string

@ocaml.doc(" [String.blit src srcoff dst dstoff len] copies [len] bytes
   from the string [src], starting at index [srcoff],
   to byte sequence [dst], starting at character number [dstoff].

   Raise [Invalid_argument] if [srcoff] and [len] do not
   designate a valid range of [src], or if [dstoff] and [len]
   do not designate a valid range of [dst]. ")
let blit: (~src: string, ~src_pos: int, ~dst: bytes, ~dst_pos: int, ~len: int) => unit

@ocaml.doc(" [String.concat sep sl] concatenates the list of strings [sl],
   inserting the separator string [sep] between each. ")
let concat: (~sep: string, list<string>) => string

@ocaml.doc(" [String.iter f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[String.length s - 1]; ()]. ")
let iter: (~f: char => unit, string) => unit

@ocaml.doc(" Same as {!String.iter}, but the
   function is applied to the index of the element as first argument
   (counting from 0), and the character itself as second argument.
   @since 4.00.0 ")
let iteri: (~f: (int, char) => unit, string) => unit

@ocaml.doc(" [String.map f s] applies function [f] in turn to all
   the characters of [s] and stores the results in a new string that
   is returned.
   @since 4.00.0 ")
let map: (~f: char => char, string) => string

@ocaml.doc(" [String.mapi f s] calls [f] with each character of [s] and its
    index (in increasing index order) and stores the results in a new
    string that is returned.
    @since 4.02.0 ")
let mapi: (~f: (int, char) => char, string) => string

@ocaml.doc(" Return a copy of the argument, without leading and trailing
   whitespace.  The characters regarded as whitespace are: [' '],
   ['\012'], ['\n'], ['\r'], and ['\t'].  If there is no leading nor
   trailing whitespace character in the argument, return the original
   string itself, not a copy.
   @since 4.00.0 ")
let trim: string => string

@ocaml.doc(" Return a copy of the argument, with special characters
   represented by escape sequences, following the lexical
   conventions of OCaml.  If there is no special
   character in the argument, return the original string itself,
   not a copy. Its inverse function is Scanf.unescaped. ")
let escaped: string => string

@ocaml.doc(" [String.index s c] returns the index of the first
   occurrence of character [c] in string [s].

   Raise [Not_found] if [c] does not occur in [s]. ")
let index: (string, char) => int

@ocaml.doc(" [String.index_opt s c] returns the index of the first
    occurrence of character [c] in string [s], or
    [None] if [c] does not occur in [s].
    @since 4.05 ")
let index_opt: (string, char) => option<int>

@ocaml.doc(" [String.rindex s c] returns the index of the last
   occurrence of character [c] in string [s].

   Raise [Not_found] if [c] does not occur in [s]. ")
let rindex: (string, char) => int

@ocaml.doc(" [String.rindex_opt s c] returns the index of the last occurrence
    of character [c] in string [s], or [None] if [c] does not occur in
    [s].
    @since 4.05 ")
let rindex_opt: (string, char) => option<int>

@ocaml.doc(" [String.index_from s i c] returns the index of the
   first occurrence of character [c] in string [s] after position [i].
   [String.index s c] is equivalent to [String.index_from s 0 c].

   Raise [Invalid_argument] if [i] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] after position [i]. ")
let index_from: (string, int, char) => int

@ocaml.doc(" [String.index_from_opt s i c] returns the index of the
    first occurrence of character [c] in string [s] after position [i]
    or [None] if [c] does not occur in [s] after position [i].

    [String.index_opt s c] is equivalent to [String.index_from_opt s 0 c].
    Raise [Invalid_argument] if [i] is not a valid position in [s].

    @since 4.05
")
let index_from_opt: (string, int, char) => option<int>

@ocaml.doc(" [String.rindex_from s i c] returns the index of the
   last occurrence of character [c] in string [s] before position [i+1].
   [String.rindex s c] is equivalent to
   [String.rindex_from s (String.length s - 1) c].

   Raise [Invalid_argument] if [i+1] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] before position [i+1]. ")
let rindex_from: (string, int, char) => int

@ocaml.doc(" [String.rindex_from_opt s i c] returns the index of the
   last occurrence of character [c] in string [s] before position [i+1]
   or [None] if [c] does not occur in [s] before position [i+1].

   [String.rindex_opt s c] is equivalent to
   [String.rindex_from_opt s (String.length s - 1) c].

   Raise [Invalid_argument] if [i+1] is not a valid position in [s].

    @since 4.05
")
let rindex_from_opt: (string, int, char) => option<int>

@ocaml.doc(" [String.contains s c] tests if character [c]
   appears in the string [s]. ")
let contains: (string, char) => bool

@ocaml.doc(" [String.contains_from s start c] tests if character [c]
   appears in [s] after position [start].
   [String.contains s c] is equivalent to
   [String.contains_from s 0 c].

   Raise [Invalid_argument] if [start] is not a valid position in [s]. ")
let contains_from: (string, int, char) => bool

@ocaml.doc(" [String.rcontains_from s stop c] tests if character [c]
   appears in [s] before position [stop+1].

   Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
   position in [s]. ")
let rcontains_from: (string, int, char) => bool

@ocaml.doc(" Return a copy of the argument, with all lowercase letters
   translated to uppercase, using the US-ASCII character set.
   @since 4.05.0 ")
let uppercase_ascii: string => string

@ocaml.doc(" Return a copy of the argument, with all uppercase letters
   translated to lowercase, using the US-ASCII character set.
   @since 4.05.0 ")
let lowercase_ascii: string => string

@ocaml.doc(" Return a copy of the argument, with the first character set to uppercase,
   using the US-ASCII character set.
   @since 4.05.0 ")
let capitalize_ascii: string => string

@ocaml.doc(" Return a copy of the argument, with the first character set to lowercase,
   using the US-ASCII character set.
   @since 4.05.0 ")
let uncapitalize_ascii: string => string

@ocaml.doc(" An alias for the type of strings. ")
type t = string

@ocaml.doc(" The comparison function for strings, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [String] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. ")
let compare: (t, t) => int

@ocaml.doc(" The equal function for strings.
    @since 4.05.0 ")
let equal: (t, t) => bool

@ocaml.doc(" [String.split_on_char sep s] returns the list of all (possibly empty)
    substrings of [s] that are delimited by the [sep] character.

    The function's output is specified by the following invariants:

    - The list is not empty.
    - Concatenating its elements using [sep] as a separator returns a
      string equal to the input ([String.concat (String.make 1 sep)
      (String.split_on_char sep s) = s]).
    - No string in the result contains the [sep] character.

    @since 4.05.0
")
let split_on_char: (~sep: char, string) => list<string>

@@ocaml.text("/*")

/* The following is for system use only. Do not call directly. */

external unsafe_get: (string, int) => char = "%string_unsafe_get"
