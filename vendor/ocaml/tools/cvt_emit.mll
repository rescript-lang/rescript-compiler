(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

{
let first_item = ref false
let command_beginning = ref 0

let add_semicolon () =
  if !first_item
  then first_item := false
  else print_string "; "

let print_unescaped_string s =
  let l = String.length s in
  let i = ref 0 in
  while !i < l do
    if s.[!i] = '\\'
    && !i+1 < l
    && (let c = s.[!i+1] in c = '{' || c = '`') (* ` *)
    then i := !i+1;
    print_char s.[!i];
    i := !i + 1
  done
}

rule main = parse
    "`" { command_beginning := Lexing.lexeme_start lexbuf;
          first_item := true;
          print_char '(';
          command lexbuf;
          print_char ')';
          main lexbuf }
  | "\\`"
        { print_string "`"; main lexbuf }
  | eof { () }
  | _   { print_char(Lexing.lexeme_char lexbuf 0); main lexbuf }

and command = parse
    "`" { () }
  | eof { prerr_string "Unterminated `...` at character ";
          prerr_int !command_beginning;
          prerr_newline();
          exit 2 }
  | "{" [^ '}'] * "}"
        { let s = Lexing.lexeme lexbuf in
          add_semicolon();
          print_string (String.sub s 1 (String.length s - 2));
          command lexbuf }
  | ( [^ '`' '{' '\\'] |
      '\\' ['\\' '"' 'n' 't' 'b' 'r' '`' '{' ] |
      '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] |
      '\\' ('\n' | "\r\n")) +
        { let s = Lexing.lexeme lexbuf in
          add_semicolon();
          (* Optimise one-character strings *)
          if String.length s = 1 && s.[0] <> '\\' && s.[0] <> '\''
          || String.length s = 2 && s.[0] = '\\' && s.[1] <> '`' && s.[1]<>'{'
          (* ` *)
          then begin
            print_string "emit_char '";
            print_unescaped_string s;
            print_string "'"
          end else begin
            print_string "emit_string \"";
            print_unescaped_string s;
            print_string "\""
          end;
          command lexbuf }

{
let _ = main(Lexing.from_channel stdin)

let _ = exit (0)
}
