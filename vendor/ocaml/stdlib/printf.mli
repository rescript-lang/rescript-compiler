(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*  Xavier Leroy and Pierre Weis, projet Cristal, INRIA Rocquencourt   *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Formatted output functions. *)

val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
(** [fprintf outchan format arg1 ... argN] formats the arguments
   [arg1] to [argN] according to the format string [format], and
   outputs the resulting string on the channel [outchan].

   The format string is a character string which contains two types of
   objects: plain characters, which are simply copied to the output
   channel, and conversion specifications, each of which causes
   conversion and printing of arguments.

   Conversion specifications have the following form:

   [% [flags] [width] [.precision] type]

   In short, a conversion specification consists in the [%] character,
   followed by optional modifiers and a type which is made of one or
   two characters.

   The types and their meanings are:

   - [d], [i]: convert an integer argument to signed decimal.
   - [u], [n], [l], [L], or [N]: convert an integer argument to
     unsigned decimal.  Warning: [n], [l], [L], and [N] are
     used for [scanf], and should not be used for [printf].
   - [x]: convert an integer argument to unsigned hexadecimal,
     using lowercase letters.
   - [X]: convert an integer argument to unsigned hexadecimal,
     using uppercase letters.
   - [o]: convert an integer argument to unsigned octal.
   - [s]: insert a string argument.
   - [S]: convert a string argument to OCaml syntax (double quotes, escapes).
   - [c]: insert a character argument.
   - [C]: convert a character argument to OCaml syntax
     (single quotes, escapes).
   - [f]: convert a floating-point argument to decimal notation,
     in the style [dddd.ddd].
   - [F]: convert a floating-point argument to OCaml syntax ([dddd.]
     or [dddd.ddd] or [d.ddd e+-dd]).
   - [e] or [E]: convert a floating-point argument to decimal notation,
     in the style [d.ddd e+-dd] (mantissa and exponent).
   - [g] or [G]: convert a floating-point argument to decimal notation,
     in style [f] or [e], [E] (whichever is more compact).
   - [B]: convert a boolean argument to the string [true] or [false]
   - [b]: convert a boolean argument (deprecated; do not use in new
     programs).
   - [ld], [li], [lu], [lx], [lX], [lo]: convert an [int32] argument to
     the format specified by the second letter (decimal, hexadecimal, etc).
   - [nd], [ni], [nu], [nx], [nX], [no]: convert a [nativeint] argument to
     the format specified by the second letter.
   - [Ld], [Li], [Lu], [Lx], [LX], [Lo]: convert an [int64] argument to
     the format specified by the second letter.
   - [a]: user-defined printer. Take two arguments and apply the
     first one to [outchan] (the current output channel) and to the
     second argument. The first argument must therefore have type
     [out_channel -> 'b -> unit] and the second ['b].
     The output produced by the function is inserted in the output of
     [fprintf] at the current point.
   - [t]: same as [%a], but take only one argument (with type
     [out_channel -> unit]) and apply it to [outchan].
   - [\{ fmt %\}]: convert a format string argument to its type digest.
     The argument must have the same type as the internal format string
     [fmt].
   - [( fmt %)]: format string substitution. Take a format string
     argument and substitute it to the internal format string [fmt]
     to print following arguments. The argument must have the same
     type as the internal format string [fmt].
   - [!]: take no argument and flush the output.
   - [%]: take no argument and output one [%] character.
   - [\@]: take no argument and output one [\@] character.
   - [,]: take no argument and output nothing: a no-op delimiter for
     conversion specifications.

   The optional [flags] are:
   - [-]: left-justify the output (default is right justification).
   - [0]: for numerical conversions, pad with zeroes instead of spaces.
   - [+]: for signed numerical conversions, prefix number with a [+]
     sign if positive.
   - space: for signed numerical conversions, prefix number with a
     space if positive.
   - [#]: request an alternate formatting style for the hexadecimal
     and octal integer types ([x], [X], [o], [lx], [lX], [lo], [Lx],
     [LX], [Lo]).

   The optional [width] is an integer indicating the minimal
   width of the result. For instance, [%6d] prints an integer,
   prefixing it with spaces to fill at least 6 characters.

   The optional [precision] is a dot [.] followed by an integer
   indicating how many digits follow the decimal point in the [%f],
   [%e], and [%E] conversions. For instance, [%.4f] prints a [float] with
   4 fractional digits.

   The integer in a [width] or [precision] can also be specified as
   [*], in which case an extra integer argument is taken to specify
   the corresponding [width] or [precision]. This integer argument
   precedes immediately the argument to print.
   For instance, [%.*f] prints a [float] with as many fractional
   digits as the value of the argument given before the float. *)

val printf : ('a, out_channel, unit) format -> 'a
(** Same as {!Printf.fprintf}, but output on [stdout]. *)

val eprintf : ('a, out_channel, unit) format -> 'a
(** Same as {!Printf.fprintf}, but output on [stderr]. *)

val sprintf : ('a, unit, string) format -> 'a
(** Same as {!Printf.fprintf}, but instead of printing on an output channel,
   return a string containing the result of formatting the arguments. *)

val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
(** Same as {!Printf.fprintf}, but instead of printing on an output channel,
   append the formatted arguments to the given extensible buffer
   (see module {!Buffer}). *)

val ifprintf : 'a -> ('b, 'a, unit) format -> 'b
(** Same as {!Printf.fprintf}, but does not print anything.
    Useful to ignore some material when conditionally printing.
    @since 3.10.0
*)

(** Formatted output functions with continuations. *)

val kfprintf : (out_channel -> 'a) -> out_channel ->
              ('b, out_channel, unit, 'a) format4 -> 'b
(** Same as [fprintf], but instead of returning immediately,
   passes the out channel to its first argument at the end of printing.
   @since 3.09.0
*)

val ikfprintf : (out_channel -> 'a) -> out_channel ->
              ('b, out_channel, unit, 'a) format4 -> 'b
(** Same as [kfprintf] above, but does not print anything.
   Useful to ignore some material when conditionally printing.
   @since 4.0
*)

val ksprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** Same as [sprintf] above, but instead of returning the string,
   passes it to the first argument.
   @since 3.09.0
*)

val kbprintf : (Buffer.t -> 'a) -> Buffer.t ->
              ('b, Buffer.t, unit, 'a) format4 -> 'b
(** Same as [bprintf], but instead of returning immediately,
   passes the buffer to its first argument at the end of printing.
   @since 3.10.0
*)

(** Deprecated *)

val kprintf : (string -> 'a) -> ('b, unit, string, 'a) format4 -> 'b
(** A deprecated synonym for [ksprintf]. *)
