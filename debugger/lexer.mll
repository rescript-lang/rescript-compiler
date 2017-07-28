(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          OCaml port by John Malecki and Xavier Leroy                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

{

open Parser

}

rule line =     (* Read a whole line *)
  parse
    ([ ^ '\n' '\r' ]* as s) ('\n' | '\r' | "\r\n")
      { s }
  | [ ^ '\n' '\r' ]*
      { Lexing.lexeme lexbuf }
  | eof
      { raise Exit }

and argument =  (* Read a raw argument *)
  parse
    [ ^ ' ' '\t' ]+
      { ARGUMENT (Lexing.lexeme lexbuf) }
  | [' ' '\t']+
      { argument lexbuf }
  | eof
      { EOL }
  | _
      { raise Parsing.Parse_error }

and line_argument =
  parse
    _ *
      { ARGUMENT (Lexing.lexeme lexbuf) }
  | eof
      { EOL }

and lexeme =    (* Read a lexeme *)
  parse
    [' ' '\t'] +
      { lexeme lexbuf }
  | ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
    (['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
      '\'' '0'-'9' ]) *
      { LIDENT(Lexing.lexeme lexbuf) }
  | ['A'-'Z' '\192'-'\214' '\216'-'\222' ]
    (['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
      '\'' '0'-'9' ]) *
      { UIDENT(Lexing.lexeme lexbuf) }
  | '"' [^ '"']* "\""
      { let s = Lexing.lexeme lexbuf in
        LIDENT(String.sub s 1 (String.length s - 2)) }
  | ['0'-'9']+
    | '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
    | '0' ['o' 'O'] ['0'-'7']+
    | '0' ['b' 'B'] ['0'-'1']+
      { INTEGER (Int64.of_string (Lexing.lexeme lexbuf)) }
  | '*'
      { STAR }
  | "-"
      { MINUS }
  | "."
      { DOT }
  | "#"
      { SHARP }
  | "@"
      { AT }
  | "$"
      { DOLLAR }
  | "!"
      { BANG }
  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | "["
      { LBRACKET }
  | "]"
      { RBRACKET }
  | ['!' '?' '~' '=' '<' '>' '|' '&' '$' '@' '^' '+' '-' '*' '/' '%']
    ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~'] *
      { OPERATOR (Lexing.lexeme lexbuf) }
  | eof
      { EOL }
  | _
      { raise Parsing.Parse_error }
