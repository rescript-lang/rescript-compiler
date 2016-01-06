(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** Parsing of command line arguments.

   This module provides a general mechanism for extracting options and
   arguments from the command line to the program.

   Syntax of command lines:
    A keyword is a character string starting with a [-].
    An option is a keyword alone or followed by an argument.
    The types of keywords are: [Unit], [Bool], [Set], [Clear],
    [String], [Set_string], [Int], [Set_int], [Float], [Set_float],
    [Tuple], [Symbol], and [Rest].
    [Unit], [Set] and [Clear] keywords take no argument. A [Rest]
    keyword takes the remaining of the command line as arguments.
    Every other keyword takes the following word on the command line
    as argument.
    Arguments not preceded by a keyword are called anonymous arguments.

   Examples ([cmd] is assumed to be the command name):
-   [cmd -flag           ](a unit option)
-   [cmd -int 1          ](an int option with argument [1])
-   [cmd -string foobar  ](a string option with argument ["foobar"])
-   [cmd -float 12.34    ](a float option with argument [12.34])
-   [cmd a b c           ](three anonymous arguments: ["a"], ["b"], and ["c"])
-   [cmd a b -- c d      ](two anonymous arguments and a rest option with
                           two arguments)
*)

type spec =
  | Unit of (unit -> unit)     (** Call the function with unit argument *)
  | Bool of (bool -> unit)     (** Call the function with a bool argument *)
  | Set of bool ref            (** Set the reference to true *)
  | Clear of bool ref          (** Set the reference to false *)
  | String of (string -> unit) (** Call the function with a string argument *)
  | Set_string of string ref   (** Set the reference to the string argument *)
  | Int of (int -> unit)       (** Call the function with an int argument *)
  | Set_int of int ref         (** Set the reference to the int argument *)
  | Float of (float -> unit)   (** Call the function with a float argument *)
  | Set_float of float ref     (** Set the reference to the float argument *)
  | Tuple of spec list         (** Take several arguments according to the
                                   spec list *)
  | Symbol of string list * (string -> unit)
                               (** Take one of the symbols as argument and
                                   call the function with the symbol *)
  | Rest of (string -> unit)   (** Stop interpreting keywords and call the
                                   function with each remaining argument *)
(** The concrete type describing the behavior associated
   with a keyword. *)

type key = string
type doc = string
type usage_msg = string
type anon_fun = (string -> unit)

val parse :
  (key * spec * doc) list -> anon_fun -> usage_msg -> unit
(** [Arg.parse speclist anon_fun usage_msg] parses the command line.
    [speclist] is a list of triples [(key, spec, doc)].
    [key] is the option keyword, it must start with a ['-'] character.
    [spec] gives the option type and the function to call when this option
    is found on the command line.
    [doc] is a one-line description of this option.
    [anon_fun] is called on anonymous arguments.
    The functions in [spec] and [anon_fun] are called in the same order
    as their arguments appear on the command line.

    If an error occurs, [Arg.parse] exits the program, after printing
    to standard error an error message as follows:
-   The reason for the error: unknown option, invalid or missing argument, etc.
-   [usage_msg]
-   The list of options, each followed by the corresponding [doc] string.
    Beware: options that have an empty [doc] string will not be included in the
    list.

    For the user to be able to specify anonymous arguments starting with a
    [-], include for example [("-", String anon_fun, doc)] in [speclist].

    By default, [parse] recognizes two unit options, [-help] and [--help],
    which will print to standard output [usage_msg] and the list of
    options, and exit the program.  You can override this behaviour
    by specifying your own [-help] and [--help] options in [speclist].
*)

val parse_dynamic :
  (key * spec * doc) list ref -> anon_fun -> usage_msg -> unit
(** Same as {!Arg.parse}, except that the [speclist] argument is a reference
    and may be updated during the parsing. A typical use for this feature
    is to parse command lines of the form:
-     command subcommand [options]
    where the list of options depends on the value of the subcommand argument.
*)

val parse_argv : ?current: int ref -> string array ->
  (key * spec * doc) list -> anon_fun -> usage_msg -> unit
(** [Arg.parse_argv ~current args speclist anon_fun usage_msg] parses
  the array [args] as if it were the command line.  It uses and updates
  the value of [~current] (if given), or [Arg.current].  You must set
  it before calling [parse_argv].  The initial value of [current]
  is the index of the program name (argument 0) in the array.
  If an error occurs, [Arg.parse_argv] raises [Arg.Bad] with
  the error message as argument.  If option [-help] or [--help] is
  given, [Arg.parse_argv] raises [Arg.Help] with the help message
  as argument.
*)

val parse_argv_dynamic : ?current:int ref -> string array ->
  (key * spec * doc) list ref -> anon_fun -> string -> unit
(** Same as {!Arg.parse_argv}, except that the [speclist] argument is a
    reference and may be updated during the parsing.
    See {!Arg.parse_dynamic}.
*)

exception Help of string
(** Raised by [Arg.parse_argv] when the user asks for help. *)

exception Bad of string
(** Functions in [spec] or [anon_fun] can raise [Arg.Bad] with an error
    message to reject invalid arguments.
    [Arg.Bad] is also raised by [Arg.parse_argv] in case of an error. *)

val usage : (key * spec * doc) list -> usage_msg -> unit
(** [Arg.usage speclist usage_msg] prints to standard error
    an error message that includes the list of valid options.  This is
    the same message that {!Arg.parse} prints in case of error.
    [speclist] and [usage_msg] are the same as for [Arg.parse]. *)

val usage_string : (key * spec * doc) list -> usage_msg -> string
(** Returns the message that would have been printed by {!Arg.usage},
    if provided with the same parameters. *)

val align: ?limit: int -> (key * spec * doc) list -> (key * spec * doc) list;;
(** Align the documentation strings by inserting spaces at the first
    space, according to the length of the keyword.  Use a
    space as the first character in a doc string if you want to
    align the whole string.  The doc strings corresponding to
    [Symbol] arguments are aligned on the next line.
    @param limit options with keyword and message longer than
    [limit] will not be used to compute the alignement.
*)

val current : int ref
(** Position (in {!Sys.argv}) of the argument being processed.  You can
    change this value, e.g. to force {!Arg.parse} to skip some arguments.
    {!Arg.parse} uses the initial value of {!Arg.current} as the index of
    argument 0 (the program name) and starts parsing arguments
    at the next element. *)
