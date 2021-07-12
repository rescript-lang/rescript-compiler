(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The lexical analyzer *)

val init : unit -> unit
val token: Lexing.lexbuf -> Parser.token
val skip_hash_bang: Lexing.lexbuf -> unit

type directive_type 

type error =
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment of Location.t
  | Unterminated_string
  | Unterminated_string_in_comment of Location.t * Location.t
  | Keyword_as_label of string
  | Invalid_literal of string
  | Invalid_directive of string * string option
  | Unterminated_paren_in_conditional
  | Unterminated_if
  | Unterminated_else 
  | Unexpected_token_in_conditional 
  | Expect_hash_then_in_conditional
  | Illegal_semver of string
  | Unexpected_directive
  | Conditional_expr_expected_type of directive_type * directive_type                           
;;

exception Error of error * Location.t



val in_comment : unit -> bool;;
val in_string : unit -> bool;;


val print_warnings : bool ref
val handle_docstrings: bool ref
val comments : unit -> (string * Location.t) list
val token_with_comments : Lexing.lexbuf -> Parser.token

(*
  [set_preprocessor init preprocessor] registers [init] as the function
to call to initialize the preprocessor when the lexer is initialized,
and [preprocessor] a function that is called when a new token is needed
by the parser, as [preprocessor lexer lexbuf] where [lexer] is the
lexing function.

When a preprocessor is configured by calling [set_preprocessor], the lexer
changes its behavior to accept backslash-newline as a token-separating blank.
*)

val set_preprocessor :
  (unit -> unit) ->
  ((Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Parser.token) ->
  unit

(** semantic version predicate *)
val semver : Location.t ->   string -> string -> bool

val filter_directive_from_lexbuf : Lexing.lexbuf -> (int * int) list

val replace_directive_int : string -> int -> unit
val replace_directive_string : string -> string -> unit
val replace_directive_bool : string -> bool -> unit 
val remove_directive_built_in_value : string -> unit

(** @return false means failed to define *)
val define_key_value : string -> string -> bool
val list_variables : Format.formatter -> unit
