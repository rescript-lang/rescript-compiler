(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../../LICENSE.  *)
(*                                                                     *)
(***********************************************************************)

(** Regular expressions and high-level string processing *)


(** {6 Regular expressions} *)


type regexp
(** The type of compiled regular expressions. *)


val regexp : string -> regexp
(** Compile a regular expression. The following constructs are
    recognized:
   - [.     ] Matches any character except newline.
   - [*     ] (postfix) Matches the preceding expression zero, one or
              several times
   - [+     ] (postfix) Matches the preceding expression one or
              several times
   - [?     ] (postfix) Matches the preceding expression once or
              not at all
   - [[..]  ] Character set. Ranges are denoted with [-], as in [[a-z]].
              An initial [^], as in [[^0-9]], complements the set.
              To include a [\]] character in a set, make it the first
              character of the set. To include a [-] character in a set,
              make it the first or the last character of the set.
   - [^     ] Matches at beginning of line (either at the beginning of
              the matched string, or just after a newline character).
   - [$     ] Matches at end of line (either at the end of the matched
              string, or just before a newline character).
   - [\|    ] (infix) Alternative between two expressions.
   - [\(..\)] Grouping and naming of the enclosed expression.
   - [\1    ] The text matched by the first [\(...\)] expression
     ([\2] for the second expression, and so on up to [\9]).
   - [\b    ] Matches word boundaries.
   - [\     ] Quotes special characters.  The special characters
              are [$^\.*+?[]].

   Note: the argument to [regexp] is usually a string literal. In this
   case, any backslash character in the regular expression must be
   doubled to make it past the OCaml string parser. For example, the
   following expression:
   {[ let r = Str.regexp "hello \\([A-Za-z]+\\)" in
      Str.replace_first r "\\1" "hello world" ]}
   returns the string ["world"].

   In particular, if you want a regular expression that matches a single
   backslash character, you need to quote it in the argument to [regexp]
   (according to the last item of the list above) by adding a second
   backslash. Then you need to quote both backslashes (according to the
   syntax of string constants in OCaml) by doubling them again, so you
   need to write four backslash characters: [Str.regexp "\\\\"].
*)

val regexp_case_fold : string -> regexp
(** Same as [regexp], but the compiled expression will match text
    in a case-insensitive way: uppercase and lowercase letters will
    be considered equivalent. *)

val quote : string -> string
(** [Str.quote s] returns a regexp string that matches exactly
   [s] and nothing else. *)

val regexp_string : string -> regexp
(** [Str.regexp_string s] returns a regular expression
   that matches exactly [s] and nothing else.*)

val regexp_string_case_fold : string -> regexp
(** [Str.regexp_string_case_fold] is similar to {!Str.regexp_string},
   but the regexp matches in a case-insensitive way. *)


(** {6 String matching and searching} *)


val string_match : regexp -> string -> int -> bool
(** [string_match r s start] tests whether a substring of [s] that
   starts at position [start] matches the regular expression [r].
   The first character of a string has position [0], as usual. *)

val search_forward : regexp -> string -> int -> int
(** [search_forward r s start] searches the string [s] for a substring
   matching the regular expression [r]. The search starts at position
   [start] and proceeds towards the end of the string.
   Return the position of the first character of the matched
   substring.
   @raise Not_found if no substring matches. *)

val search_backward : regexp -> string -> int -> int
(** [search_backward r s last] searches the string [s] for a
  substring matching the regular expression [r]. The search first
  considers substrings that start at position [last] and proceeds
  towards the beginning of string. Return the position of the first
  character of the matched substring.
  @raise Not_found if no substring matches. *)

val string_partial_match : regexp -> string -> int -> bool
(** Similar to {!Str.string_match}, but also returns true if
   the argument string is a prefix of a string that matches.
   This includes the case of a true complete match. *)

val matched_string : string -> string
(** [matched_string s] returns the substring of [s] that was matched
   by the last call to one of the following matching or searching
   functions:
   - {!Str.string_match}
   - {!Str.search_forward}
   - {!Str.search_backward}
   - {!Str.string_partial_match}
   - {!Str.global_substitute}
   - {!Str.substitute_first}

   provided that none of the following functions was called inbetween:
   - {!Str.global_replace}
   - {!Str.replace_first}
   - {!Str.split}
   - {!Str.bounded_split}
   - {!Str.split_delim}
   - {!Str.bounded_split_delim}
   - {!Str.full_split}
   - {!Str.bounded_full_split}

   Note: in the case of [global_substitute] and [substitute_first],
   a call to [matched_string] is only valid within the [subst] argument,
   not after [global_substitute] or [substitute_first] returns.

   The user must make sure that the parameter [s] is the same string
   that was passed to the matching or searching function. *)

val match_beginning : unit -> int
(** [match_beginning()] returns the position of the first character
   of the substring that was matched by the last call to a matching
   or searching function (see {!Str.matched_string} for details). *)

val match_end : unit -> int
(** [match_end()] returns the position of the character following the
   last character of the substring that was matched by the last call
   to a matching or searching function (see {!Str.matched_string} for
   details). *)

val matched_group : int -> string -> string
(** [matched_group n s] returns the substring of [s] that was matched
   by the [n]th group [\(...\)] of the regular expression that was
   matched by the last call to a matching or searching function (see
   {!Str.matched_string} for details).
   The user must make sure that the parameter [s] is the same string
   that was passed to the matching or searching function.
   @raise Not_found if the [n]th group
   of the regular expression was not matched.  This can happen
   with groups inside alternatives [\|], options [?]
   or repetitions [*].  For instance, the empty string will match
   [\(a\)*], but [matched_group 1 ""] will raise [Not_found]
   because the first group itself was not matched. *)

val group_beginning : int -> int
(** [group_beginning n] returns the position of the first character
   of the substring that was matched by the [n]th group of
   the regular expression that was matched by the last call to a
   matching or searching function (see {!Str.matched_string} for details).
   @raise Not_found if the [n]th group of the regular expression
   was not matched.
   @raise Invalid_argument if there are fewer than [n] groups in
   the regular expression. *)

val group_end : int -> int
(** [group_end n] returns
   the position of the character following the last character of
   substring that was matched by the [n]th group of the regular
   expression that was matched by the last call to a matching or
   searching function (see {!Str.matched_string} for details).
   @raise Not_found if the [n]th group of the regular expression
   was not matched.
   @raise Invalid_argument if there are fewer than [n] groups in
   the regular expression. *)


(** {6 Replacement} *)


val global_replace : regexp -> string -> string -> string
(** [global_replace regexp templ s] returns a string identical to [s],
   except that all substrings of [s] that match [regexp] have been
   replaced by [templ]. The replacement template [templ] can contain
   [\1], [\2], etc; these sequences will be replaced by the text
   matched by the corresponding group in the regular expression.
   [\0] stands for the text matched by the whole regular expression. *)

val replace_first : regexp -> string -> string -> string
(** Same as {!Str.global_replace}, except that only the first substring
   matching the regular expression is replaced. *)

val global_substitute : regexp -> (string -> string) -> string -> string
(** [global_substitute regexp subst s] returns a string identical
   to [s], except that all substrings of [s] that match [regexp]
   have been replaced by the result of function [subst]. The
   function [subst] is called once for each matching substring,
   and receives [s] (the whole text) as argument. *)

val substitute_first : regexp -> (string -> string) -> string -> string
(** Same as {!Str.global_substitute}, except that only the first substring
   matching the regular expression is replaced. *)

val replace_matched : string -> string -> string
(** [replace_matched repl s] returns the replacement text [repl]
   in which [\1], [\2], etc. have been replaced by the text
   matched by the corresponding groups in the regular expression
   that was matched by the last call to a matching or searching
   function (see {!Str.matched_string} for details).
   [s] must be the same string that was passed to the matching or
   searching function. *)


(** {6 Splitting} *)


val split : regexp -> string -> string list
(** [split r s] splits [s] into substrings, taking as delimiters
   the substrings that match [r], and returns the list of substrings.
   For instance, [split (regexp "[ \t]+") s] splits [s] into
   blank-separated words.  An occurrence of the delimiter at the
   beginning or at the end of the string is ignored. *)

val bounded_split : regexp -> string -> int -> string list
(** Same as {!Str.split}, but splits into at most [n] substrings,
   where [n] is the extra integer parameter. *)

val split_delim : regexp -> string -> string list
(** Same as {!Str.split} but occurrences of the
   delimiter at the beginning and at the end of the string are
   recognized and returned as empty strings in the result.
   For instance, [split_delim (regexp " ") " abc "]
   returns [[""; "abc"; ""]], while [split] with the same
   arguments returns [["abc"]]. *)

val bounded_split_delim : regexp -> string -> int -> string list
(** Same as {!Str.bounded_split}, but occurrences of the
   delimiter at the beginning and at the end of the string are
   recognized and returned as empty strings in the result. *)

type split_result =
    Text of string
  | Delim of string

val full_split : regexp -> string -> split_result list
(** Same as {!Str.split_delim}, but returns
   the delimiters as well as the substrings contained between
   delimiters.  The former are tagged [Delim] in the result list;
   the latter are tagged [Text].  For instance,
   [full_split (regexp "[{}]") "{ab}"] returns
   [[Delim "{"; Text "ab"; Delim "}"]]. *)

val bounded_full_split : regexp -> string -> int -> split_result list
(** Same as {!Str.bounded_split_delim}, but returns
   the delimiters as well as the substrings contained between
   delimiters.  The former are tagged [Delim] in the result list;
   the latter are tagged [Text]. *)


(** {6 Extracting substrings} *)


val string_before : string -> int -> string
(** [string_before s n] returns the substring of all characters of [s]
   that precede position [n] (excluding the character at
   position [n]). *)

val string_after : string -> int -> string
(** [string_after s n] returns the substring of all characters of [s]
   that follow position [n] (including the character at
   position [n]). *)

val first_chars : string -> int -> string
(** [first_chars s n] returns the first [n] characters of [s].
   This is the same function as {!Str.string_before}. *)

val last_chars : string -> int -> string
(** [last_chars s n] returns the last [n] characters of [s]. *)
