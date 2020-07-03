(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A generic lexical analyzer.


   This module implements a simple 'standard' lexical analyzer, presented
   as a function from character streams to token streams. It implements
   roughly the lexical conventions of OCaml, but is parameterized by the
   set of keywords of your language.


   Example: a lexer suitable for a desk calculator is obtained by
   {[     let lexer = make_lexer ["+";"-";"*";"/";"let";"="; "("; ")"]  ]}

   The associated parser would be a function from [token stream]
   to, for instance, [int], and would have rules such as:

   {[
           let rec parse_expr = parser
             | [< n1 = parse_atom; n2 = parse_remainder n1 >] -> n2
           and parse_atom = parser
             | [< 'Int n >] -> n
             | [< 'Kwd "("; n = parse_expr; 'Kwd ")" >] -> n
           and parse_remainder n1 = parser
             | [< 'Kwd "+"; n2 = parse_expr >] -> n1+n2
             | [< >] -> n1
   ]}

   One should notice that the use of the [parser] keyword and associated
   notation for streams are only available through camlp4 extensions. This
   means that one has to preprocess its sources {i e. g.} by using the
   ["-pp"] command-line switch of the compilers.
*)

(** The type of tokens. The lexical classes are: [Int] and [Float]
   for integer and floating-point numbers; [String] for
   string literals, enclosed in double quotes; [Char] for
   character literals, enclosed in single quotes; [Ident] for
   identifiers (either sequences of letters, digits, underscores
   and quotes, or sequences of 'operator characters' such as
   [+], [*], etc); and [Kwd] for keywords (either identifiers or
   single 'special characters' such as [(], [}], etc). *)
type token =
    Kwd of string
  | Ident of string
  | Int of int
  | Float of float
  | String of string
  | Char of char

val make_lexer : string list -> char Stream.t -> token Stream.t
(** Construct the lexer function. The first argument is the list of
   keywords. An identifier [s] is returned as [Kwd s] if [s]
   belongs to this list, and as [Ident s] otherwise.
   A special character [s] is returned as [Kwd s] if [s]
   belongs to this list, and cause a lexical error (exception
   {!Stream.Error} with the offending lexeme as its parameter) otherwise.
   Blanks and newlines are skipped. Comments delimited by [(*] and [*)]
   are skipped as well, and can be nested. A {!Stream.Failure} exception
   is raised if end of stream is unexpectedly reached.*)
