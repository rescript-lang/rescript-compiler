{
(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let print_DEBUG2 s = print_string s ; print_newline ()

(** the lexer for special comments. *)

open Lexing
open Odoc_parser

let buf = Buffer.create 32

}

rule main = parse
  [' ' '\013' '\009' '\012'] +
  {
    print_DEBUG2 "[' ' '\013' '\009' '\012'] +";
    main lexbuf
  }

  | [ '\010' ]
      {
        print_DEBUG2 " [ '\010' ] ";
        main lexbuf
      }

  | "<"
      {
        print_DEBUG2 "call url lexbuf" ;
        url lexbuf
      }

  | "\""
      {
        print_DEBUG2 "call doc lexbuf" ;
        doc lexbuf
      }


  | '\''
      {
        print_DEBUG2 "call file lexbuf" ;
        file lexbuf
      }

  | eof
      {
        print_DEBUG2 "EOF";
        EOF
      }

  | _
      {
        Buffer.reset buf ;
        Buffer.add_string buf (Lexing.lexeme lexbuf);
        desc lexbuf
      }

and url = parse
  | ([^'>'] | '\n')+">"
      {
        let s = Lexing.lexeme lexbuf in
        print_DEBUG2 ("([^'>'] | '\n')+ \">\" with "^s) ;
        See_url (String.sub s 0 ((String.length s) -1))
      }


and doc = parse
  | ([^'"'] | '\n' | "\\'")* "\""
      {
        let s = Lexing.lexeme lexbuf in
        See_doc (String.sub s 0 ((String.length s) -1))
      }

and file = parse
  | ([^'\''] | '\n' | "\\\"")* "'"
      {
        let s = Lexing.lexeme lexbuf in
        See_file (String.sub s 0 ((String.length s) -1))
      }


and desc = parse
    eof
      { Desc (Buffer.contents buf) }
  | _
      {
        Buffer.add_string buf (Lexing.lexeme lexbuf);
        desc lexbuf
      }
