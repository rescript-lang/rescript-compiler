(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Weis, projet Cristal, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
[@@@ocaml.deprecated "This module is deprecated"]
(** Formatted input functions. *)

(** {1 Introduction} *)

(** {2 Functional input with format strings} *)

(** The module {!Scanf} provides formatted input functions or {e scanners}.

    The formatted input functions can read from any kind of input, including
    strings, files, or anything that can return characters. The more general
    source of characters is named a {e formatted input channel} (or {e
    scanning buffer}) and has type {!Scanning.in_channel}. The more general
    formatted input function reads from any scanning buffer and is named
    [bscanf].

    Generally speaking, the formatted input functions have 3 arguments:
    - the first argument is a source of characters for the input,
    - the second argument is a format string that specifies the values to
      read,
    - the third argument is a {e receiver function} that is applied to the
      values read.

    Hence, a typical call to the formatted input function {!Scanf.bscanf} is
    [bscanf ic fmt f], where:

    - [ic] is a source of characters (typically a {e
    formatted input channel} with type {!Scanning.in_channel}),

    - [fmt] is a format string (the same format strings as those used to print
    material with module {!Printf} or {!Format}),

    - [f] is a function that has as many arguments as the number of values to
    read in the input according to [fmt].
*)

(** {2 A simple example} *)

(** As suggested above, the expression [bscanf ic "%d" f] reads a decimal
    integer [n] from the source of characters [ic] and returns [f n].

    For instance,

    - if we use [stdin] as the source of characters ({!Scanning.stdin} is
    the predefined formatted input channel that reads from standard input),

    - if we define the receiver [f] as [let f x = x + 1],

    then [bscanf Scanning.stdin "%d" f] reads an integer [n] from the
    standard input and returns [f n] (that is [n + 1]). Thus, if we
    evaluate [bscanf stdin "%d" f], and then enter [41] at the
    keyboard, the result we get is [42].
*)

(** {2 Formatted input as a functional feature} *)

(** The OCaml scanning facility is reminiscent of the corresponding C feature.
    However, it is also largely different, simpler, and yet more powerful:
    the formatted input functions are higher-order functionals and the
    parameter passing mechanism is just the regular function application not
    the variable assignment based mechanism which is typical for formatted
    input in imperative languages; the OCaml format strings also feature
    useful additions to easily define complex tokens; as expected within a
    functional programming language, the formatted input functions also
    support polymorphism, in particular arbitrary interaction with
    polymorphic user-defined scanners. Furthermore, the OCaml formatted input
    facility is fully type-checked at compile time.
*)

(** {1 Formatted input channel} *)

module Scanning : sig

type in_channel
(** The notion of input channel for the {!Scanf} module:
   those channels provide all the machinery necessary to read from any source
   of characters, including a {!Pervasives.in_channel} value.
   A Scanf.Scanning.in_channel value is also called a {i formatted input
   channel} or equivalently a {i scanning buffer}.
   The type {!Scanning.scanbuf} below is an alias for [Scanning.in_channel].
   @since 3.12.0
*)

type scanbuf = in_channel
(** The type of scanning buffers. A scanning buffer is the source from which a
    formatted input function gets characters. The scanning buffer holds the
    current state of the scan, plus a function to get the next char from the
    input, and a token buffer to store the string matched so far.

    Note: a scanning action may often require to examine one character in
    advance; when this 'lookahead' character does not belong to the token
    read, it is stored back in the scanning buffer and becomes the next
    character yet to be read.
*)

val stdin : in_channel
(** The standard input notion for the {!Scanf} module.
    [Scanning.stdin] is the {!Scanning.in_channel} formatted input channel
    attached to {!Pervasives.stdin}.

    Note: in the interactive system, when input is read from
    {!Pervasives.stdin}, the newline character that triggers evaluation is
    part of the input; thus, the scanning specifications must properly skip
    this additional newline character (for instance, simply add a ['\n'] as
    the last character of the format string).
    @since 3.12.0
*)

type file_name = string
(** A convenient alias to designate a file name.
    @since 4.00.0
*)

val open_in : file_name -> in_channel
(** [Scanning.open_in fname] returns a {!Scanning.in_channel} formatted input
    channel for bufferized reading in text mode from file [fname].

    Note:
    [open_in] returns a formatted input channel that efficiently reads
    characters in large chunks; in contrast, [from_channel] below returns
    formatted input channels that must read one character at a time, leading
    to a much slower scanning rate.
    @since 3.12.0
*)

val open_in_bin : file_name -> in_channel
(** [Scanning.open_in_bin fname] returns a {!Scanning.in_channel} formatted
    input channel for bufferized reading in binary mode from file [fname].
    @since 3.12.0
*)

val close_in : in_channel -> unit
(** Closes the {!Pervasives.in_channel} associated with the given
  {!Scanning.in_channel} formatted input channel.
  @since 3.12.0
*)

val from_file : file_name -> in_channel
(** An alias for {!Scanning.open_in} above. *)

val from_file_bin : string -> in_channel
(** An alias for {!Scanning.open_in_bin} above. *)

val from_string : string -> in_channel
(** [Scanning.from_string s] returns a {!Scanning.in_channel} formatted
    input channel which reads from the given string.
    Reading starts from the first character in the string.
    The end-of-input condition is set when the end of the string is reached.
*)

val from_function : (unit -> char) -> in_channel
(** [Scanning.from_function f] returns a {!Scanning.in_channel} formatted
    input channel with the given function as its reading method.

    When scanning needs one more character, the given function is called.

    When the function has no more character to provide, it {e must} signal an
    end-of-input condition by raising the exception [End_of_file].
*)

val from_channel : Pervasives.in_channel -> in_channel
(** [Scanning.from_channel ic] returns a {!Scanning.in_channel} formatted
    input channel which reads from the regular {!Pervasives.in_channel} input
    channel [ic] argument.
    Reading starts at current reading position of [ic].
*)

val end_of_input : in_channel -> bool
(** [Scanning.end_of_input ic] tests the end-of-input condition of the given
    {!Scanning.in_channel} formatted input channel.
*)

val beginning_of_input : in_channel -> bool
(** [Scanning.beginning_of_input ic] tests the beginning of input condition
    of the given {!Scanning.in_channel} formatted input channel.
*)

val name_of_input : in_channel -> string
(** [Scanning.name_of_input ic] returns the name of the character source
    for the given {!Scanning.in_channel} formatted input channel.
    @since 3.09.0
*)

val stdib : in_channel
  [@@ocaml.deprecated "Use Scanf.Scanning.stdin instead."]
(** A deprecated alias for {!Scanning.stdin}, the scanning buffer reading from
    {!Pervasives.stdin}.
*)

end

(** {1 Type of formatted input functions} *)

type ('a, 'b, 'c, 'd) scanner =
     ('a, Scanning.in_channel, 'b, 'c, 'a -> 'd, 'd) format6 -> 'c
(** The type of formatted input scanners: [('a, 'b, 'c, 'd) scanner]
    is the type of a formatted input function that reads from some
    formatted input channel according to some format string; more
    precisely, if [scan] is some formatted input function, then [scan
    ic fmt f] applies [f] to all the arguments specified by format
    string [fmt], when [scan] has read those arguments from the
    {!Scanning.in_channel} formatted input channel [ic].

    For instance, the {!Scanf.scanf} function below has type
    [('a, 'b, 'c, 'd) scanner], since it is a formatted input function that
    reads from {!Scanning.stdin}: [scanf fmt f] applies [f] to the arguments
    specified by [fmt], reading those arguments from {!Pervasives.stdin} as
    expected.

    If the format [fmt] has some [%r] indications, the corresponding
    formatted input functions must be provided {e before} receiver function
    [f]. For instance, if [read_elem] is an input function for values of type
    [t], then [bscanf ic "%r;" read_elem f] reads a value [v] of type [t]
    followed by a [';'] character, and returns [f v].
    @since 3.10.0
*)

exception Scan_failure of string
(** When the input can not be read according to the format string
    specification, formatted input functions typically raise exception
    [Scan_failure].
*)

(** {1 The general formatted input function} *)

val bscanf : Scanning.in_channel -> ('a, 'b, 'c, 'd) scanner

(** [bscanf ic fmt r1 ... rN f] reads characters from the
    {!Scanning.in_channel} formatted input channel [ic] and converts them to
    values according to format string [fmt].
    As a final step, receiver function [f] is applied to the values read and
    gives the result of the [bscanf] call.

    For instance, if [f] is the function [fun s i -> i + 1], then
    [Scanf.sscanf "x= 1" "%s = %i" f] returns [2].

    Arguments [r1] to [rN] are user-defined input functions that read the
    argument corresponding to the [%r] conversions specified in the format
    string.
*)

(** {1 Format string description} *)

(** The format string is a character string which contains three types of
    objects:
    - plain characters, which are simply matched with the characters of the
      input (with a special case for space and line feed, see {!Scanf.space}),
    - conversion specifications, each of which causes reading and conversion of
      one argument for the function [f] (see {!Scanf.conversion}),
    - scanning indications to specify boundaries of tokens
      (see scanning {!Scanf.indication}).
*)

(** {2:space The space character in format strings} *)

(** As mentioned above, a plain character in the format string is just
    matched with the next character of the input; however, two characters are
    special exceptions to this rule: the space character ([' '] or ASCII code
    32) and the line feed character (['\n'] or ASCII code 10).
    A space does not match a single space character, but any amount of
    'whitespace' in the input. More precisely, a space inside the format
    string matches {e any number} of tab, space, line feed and carriage
    return characters. Similarly, a line feed character in the format string
    matches either a single line feed or a carriage return followed by a line
    feed.

    Matching {e any} amount of whitespace, a space in the format string
    also matches no amount of whitespace at all; hence, the call [bscanf ib
    "Price = %d $" (fun p -> p)] succeeds and returns [1] when reading an
    input with various whitespace in it, such as [Price = 1 $],
    [Price  =  1    $], or even [Price=1$].
*)

(** {2:conversion Conversion specifications in format strings} *)

(** Conversion specifications consist in the [%] character, followed by
    an optional flag, an optional field width, and followed by one or
    two conversion characters.

    The conversion characters and their meanings are:

    - [d]: reads an optionally signed decimal integer ([0-9]+).
    - [i]: reads an optionally signed integer
      (usual input conventions for decimal ([0-9]+), hexadecimal
       ([0x[0-9a-f]+] and [0X[0-9A-F]+]), octal ([0o[0-7]+]), and binary
       ([0b[0-1]+]) notations are understood).
    - [u]: reads an unsigned decimal integer.
    - [x] or [X]: reads an unsigned hexadecimal integer ([[0-9a-fA-F]+]).
    - [o]: reads an unsigned octal integer ([[0-7]+]).
    - [s]: reads a string argument that spreads as much as possible, until the
      following bounding condition holds: {ul
      {- a whitespace has been found (see {!Scanf.space}),}
      {- a scanning indication (see scanning {!Scanf.indication}) has been
         encountered,}
      {- the end-of-input has been reached.}}
      Hence, this conversion always succeeds: it returns an empty
      string if the bounding condition holds when the scan begins.
    - [S]: reads a delimited string argument (delimiters and special
      escaped characters follow the lexical conventions of OCaml).
    - [c]: reads a single character. To test the current input character
      without reading it, specify a null field width, i.e. use
      specification [%0c]. Raise [Invalid_argument], if the field width
      specification is greater than 1.
    - [C]: reads a single delimited character (delimiters and special
      escaped characters follow the lexical conventions of OCaml).
    - [f], [e], [E], [g], [G]: reads an optionally signed
      floating-point number in decimal notation, in the style [dddd.ddd
      e/E+-dd].
    - [h], [H]: reads an optionally signed floating-point number
      in hexadecimal notation.
    - [F]: reads a floating point number according to the lexical
      conventions of OCaml (hence the decimal point is mandatory if the
      exponent part is not mentioned).
    - [B]: reads a boolean argument ([true] or [false]).
    - [b]: reads a boolean argument (for backward compatibility; do not use
      in new programs).
    - [ld], [li], [lu], [lx], [lX], [lo]: reads an [int32] argument to
      the format specified by the second letter for regular integers.
    - [nd], [ni], [nu], [nx], [nX], [no]: reads a [nativeint] argument to
      the format specified by the second letter for regular integers.
    - [Ld], [Li], [Lu], [Lx], [LX], [Lo]: reads an [int64] argument to
      the format specified by the second letter for regular integers.
    - [[ range ]]: reads characters that matches one of the characters
      mentioned in the range of characters [range] (or not mentioned in
      it, if the range starts with [^]). Reads a [string] that can be
      empty, if the next input character does not match the range. The set of
      characters from [c1] to [c2] (inclusively) is denoted by [c1-c2].
      Hence, [%[0-9]] returns a string representing a decimal number
      or an empty string if no decimal digit is found; similarly,
      [%[0-9a-f]] returns a string of hexadecimal digits.
      If a closing bracket appears in a range, it must occur as the
      first character of the range (or just after the [^] in case of
      range negation); hence [[\]]] matches a [\]] character and
      [[^\]]] matches any character that is not [\]].
      Use [%%] and [%@] to include a [%] or a [@] in a range.
    - [r]: user-defined reader. Takes the next [ri] formatted input
      function and applies it to the scanning buffer [ib] to read the
      next argument. The input function [ri] must therefore have type
      [Scanning.in_channel -> 'a] and the argument read has type ['a].
    - [{ fmt %}]: reads a format string argument. The format string
      read must have the same type as the format string specification
      [fmt]. For instance, ["%{ %i %}"] reads any format string that
      can read a value of type [int]; hence, if [s] is the string
      ["fmt:\"number is %u\""], then [Scanf.sscanf s "fmt: %{%i%}"]
      succeeds and returns the format string ["number is %u"].
    - [( fmt %)]: scanning sub-format substitution.
      Reads a format string [rf] in the input, then goes on scanning with
      [rf] instead of scanning with [fmt].
      The format string [rf] must have the same type as the format string
      specification [fmt] that it replaces.
      For instance, ["%( %i %)"] reads any format string that can read a value
      of type [int].
      The conversion returns the format string read [rf], and then a value
      read using [rf].
      Hence, if [s] is the string ["\"%4d\"1234.00"], then
      [Scanf.sscanf s "%(%i%)" (fun fmt i -> fmt, i)] evaluates to
      [("%4d", 1234)].
      This behaviour is not mere format substitution, since the conversion
      returns the format string read as additional argument. If you need
      pure format substitution, use special flag [_] to discard the
      extraneous argument: conversion [%_( fmt %)] reads a format string
      [rf] and then behaves the same as format string [rf].  Hence, if [s] is
      the string ["\"%4d\"1234.00"], then [Scanf.sscanf s "%_(%i%)"] is
      simply equivalent to [Scanf.sscanf "1234.00" "%4d"].
    - [l]: returns the number of lines read so far.
    - [n]: returns the number of characters read so far.
    - [N] or [L]: returns the number of tokens read so far.
    - [!]: matches the end of input condition.
    - [%]: matches one [%] character in the input.
    - [@]: matches one [@] character in the input.
    - [,]: does nothing.

    Following the [%] character that introduces a conversion, there may be
    the special flag [_]: the conversion that follows occurs as usual,
    but the resulting value is discarded.
    For instance, if [f] is the function [fun i -> i + 1], and [s] is the
    string ["x = 1"], then [Scanf.sscanf s "%_s = %i" f] returns [2].

    The field width is composed of an optional integer literal
    indicating the maximal width of the token to read.
    For instance, [%6d] reads an integer, having at most 6 decimal digits;
    [%4f] reads a float with at most 4 characters; and [%8[\000-\255]]
    returns the next 8 characters (or all the characters still available,
    if fewer than 8 characters are available in the input).

    Notes:

    - as mentioned above, a [%s] conversion always succeeds, even if there is
      nothing to read in the input: in this case, it simply returns [""].

    - in addition to the relevant digits, ['_'] characters may appear
    inside numbers (this is reminiscent to the usual OCaml lexical
    conventions). If stricter scanning is desired, use the range
    conversion facility instead of the number conversions.

    - the [scanf] facility is not intended for heavy duty lexical
    analysis and parsing. If it appears not expressive enough for your
    needs, several alternative exists: regular expressions (module
    {!Str}), stream parsers, [ocamllex]-generated lexers,
    [ocamlyacc]-generated parsers.
*)

(** {2:indication Scanning indications in format strings} *)

(** Scanning indications appear just after the string conversions [%s]
    and [%[ range ]] to delimit the end of the token. A scanning
    indication is introduced by a [@] character, followed by some
    plain character [c]. It means that the string token should end
    just before the next matching [c] (which is skipped). If no [c]
    character is encountered, the string token spreads as much as
    possible. For instance, ["%s@\t"] reads a string up to the next
    tab character or to the end of input. If a [@] character appears
    anywhere else in the format string, it is treated as a plain character.

    Note:

    - As usual in format strings, [%] and [@] characters must be escaped
    using [%%] and [%@]; this rule still holds within range specifications
    and scanning indications.
    For instance, format ["%s@%%"] reads a string up to the next [%]
    character, and format ["%s@%@"] reads a string up to the next [@].
    - The scanning indications introduce slight differences in the syntax of
    {!Scanf} format strings, compared to those used for the {!Printf}
    module. However, the scanning indications are similar to those used in
    the {!Format} module; hence, when producing formatted text to be scanned
    by {!Scanf.bscanf}, it is wise to use printing functions from the
    {!Format} module (or, if you need to use functions from {!Printf}, banish
    or carefully double check the format strings that contain ['@']
    characters).
*)

(** {2 Exceptions during scanning} *)

(** Scanners may raise the following exceptions when the input cannot be read
    according to the format string:

    - Raise {!Scanf.Scan_failure} if the input does not match the format.

    - Raise [Failure] if a conversion to a number is not possible.

    - Raise [End_of_file] if the end of input is encountered while some more
      characters are needed to read the current conversion specification.

    - Raise [Invalid_argument] if the format string is invalid.

    Note:

    - as a consequence, scanning a [%s] conversion never raises exception
    [End_of_file]: if the end of input is reached the conversion succeeds and
    simply returns the characters read so far, or [""] if none were ever read.
*)

(** {1 Specialised formatted input functions} *)

val sscanf : string -> ('a, 'b, 'c, 'd) scanner
(** Same as {!Scanf.bscanf}, but reads from the given string. *)

val scanf : ('a, 'b, 'c, 'd) scanner
(** Same as {!Scanf.bscanf}, but reads from the predefined formatted input
    channel {!Scanf.Scanning.stdin} that is connected to {!Pervasives.stdin}.
*)

val kscanf :
  Scanning.in_channel -> (Scanning.in_channel -> exn -> 'd) ->
    ('a, 'b, 'c, 'd) scanner
(** Same as {!Scanf.bscanf}, but takes an additional function argument
    [ef] that is called in case of error: if the scanning process or
    some conversion fails, the scanning function aborts and calls the
    error handling function [ef] with the formatted input channel and the
    exception that aborted the scanning process as arguments.
*)

val ksscanf :
  string -> (Scanning.in_channel -> exn -> 'd) ->
    ('a, 'b, 'c, 'd) scanner
(** Same as {!Scanf.kscanf} but reads from the given string.
    @since 4.02.0 *)

(** {1 Reading format strings from input} *)

val bscanf_format :
  Scanning.in_channel -> ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
    (('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g) -> 'g
(** [bscanf_format ic fmt f] reads a format string token from the formatted
    input channel [ic], according to the given format string [fmt], and
    applies [f] to the resulting format string value.
    Raise {!Scan_failure} if the format string value read does not have the
    same type as [fmt].
    @since 3.09.0
*)

val sscanf_format :
  string -> ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
    (('a, 'b, 'c, 'd, 'e, 'f) format6 -> 'g) -> 'g
(** Same as {!Scanf.bscanf_format}, but reads from the given string.
    @since 3.09.0
*)

val format_from_string :
  string ->
    ('a, 'b, 'c, 'd, 'e, 'f) format6 -> ('a, 'b, 'c, 'd, 'e, 'f) format6
(** [format_from_string s fmt] converts a string argument to a format string,
    according to the given format string [fmt].
    Raise {!Scan_failure} if [s], considered as a format string, does not
    have the same type as [fmt].
    @since 3.10.0
*)

val unescaped : string -> string
(** [unescaped s] return a copy of [s] with escape sequences (according to
    the lexical conventions of OCaml) replaced by their corresponding special
    characters.
    More precisely, [Scanf.unescaped] has the following property:
    for all string [s], [Scanf.unescaped (String.escaped s) = s].

    Always return a copy of the argument, even if there is no escape sequence
    in the argument.
    Raise {!Scan_failure} if [s] is not properly escaped (i.e. [s] has invalid
    escape sequences or special characters that are not properly escaped).
    For instance, [String.unescaped "\""] will fail.
    @since 4.00.0
*)

(** {1 Deprecated} *)

val fscanf : Pervasives.in_channel -> ('a, 'b, 'c, 'd) scanner
  [@@ocaml.deprecated "Use Scanning.from_channel then Scanf.bscanf."]
(** @deprecated [Scanf.fscanf] is error prone and deprecated since 4.03.0.

    This function violates the following invariant of the {!Scanf} module:
    To preserve scanning semantics, all scanning functions defined in {!Scanf}
    must read from a user defined {!Scanning.in_channel} formatted input
    channel.

    If you need to read from a {!Pervasives.in_channel} input channel
    [ic], simply define a {!Scanning.in_channel} formatted input channel as in
    [let ib = Scanning.from_channel ic],
    then use [Scanf.bscanf ib] as usual.
*)

val kfscanf :
  Pervasives.in_channel -> (Scanning.in_channel -> exn -> 'd) ->
    ('a, 'b, 'c, 'd) scanner
  [@@ocaml.deprecated "Use Scanning.from_channel then Scanf.kscanf."]
(** @deprecated [Scanf.kfscanf] is error prone and deprecated since 4.03.0. *)
