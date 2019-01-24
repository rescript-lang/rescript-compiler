(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Lexer definitions for the Tests Specification Language *)

{
open Tsl_parser

let comment_start_pos = ref []

let lexer_error message =
  Printf.eprintf "%s\n%!" message;
  exit 2

}

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

rule token = parse
  | blank * { token lexbuf }
  | newline { Lexing.new_line lexbuf; token lexbuf }
  | "/*" blank* "TEST" { TSL_BEGIN_C_STYLE }
  | "*/" { TSL_END_C_STYLE }
  | "(*" blank* "TEST" { TSL_BEGIN_OCAML_STYLE }
  | "*)" { TSL_END_OCAML_STYLE }
  | "," { COMA }
  | '*'+ { TEST_DEPTH (String.length (Lexing.lexeme lexbuf)) }
  | "=" { EQUAL }
  | identchar *
    { let s = Lexing.lexeme lexbuf in
      match s with
        | "include" -> INCLUDE
        | "with" -> WITH
        | _ -> IDENTIFIER s
    }
  | "(*"
    {
      comment_start_pos := [Lexing.lexeme_start_p lexbuf];
      comment lexbuf
    }
  | "\"" [^'"']* "\""
    { let s = Lexing.lexeme lexbuf in
      let string_length = (String.length s) -2 in
      let s' = String.sub s 1 string_length in
      STRING s'
    }
  | _
    {
      let pos = Lexing.lexeme_start_p lexbuf in
      let file = pos.Lexing.pos_fname in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      let message = Printf.sprintf "%s:%d:%d: unexpected character %s"
        file line column (Lexing.lexeme lexbuf) in
      lexer_error message
    }
and comment = parse
  | "(*"
    {
      comment_start_pos :=
        (Lexing.lexeme_start_p lexbuf) :: !comment_start_pos;
      comment lexbuf
    }
  | "*)"
    {
      comment_start_pos := List.tl !comment_start_pos;
      if !comment_start_pos = [] then token lexbuf else comment lexbuf
    }
  | eof
    {
      let pos = List.hd !comment_start_pos in
      let file = pos.Lexing.pos_fname in
      let line = pos.Lexing.pos_lnum in
      let column = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
      let message = Printf.sprintf "%s:%d:%d: unterminated comment"
        file line column in
      lexer_error message
    }
  | _
    {
      comment lexbuf
    }
