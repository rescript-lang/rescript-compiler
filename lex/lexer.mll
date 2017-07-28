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

(* The lexical analyzer for lexer definitions. Bootstrapped! *)

{
open Syntax
open Parser

(* Auxiliaries for the lexical analyzer *)

let brace_depth = ref 0
and comment_depth = ref 0

let in_pattern () = !brace_depth = 0 && !comment_depth = 0

exception Lexical_error of string * string * int * int

let string_buff = Buffer.create 256

let reset_string_buffer () = Buffer.clear string_buff

let store_string_char c = Buffer.add_char string_buff c
let store_string_chars s = Buffer.add_string string_buff s

let get_stored_string () = Buffer.contents string_buff

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let raise_lexical_error lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  raise (Lexical_error (msg,
                        p.Lexing.pos_fname,
                        p.Lexing.pos_lnum,
                        p.Lexing.pos_cnum - p.Lexing.pos_bol + 1))
;;

let handle_lexical_error fn lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line = p.Lexing.pos_lnum
  and column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1
  and file = p.Lexing.pos_fname
  in
  try
    fn lexbuf
  with Lexical_error (msg, "", 0, 0) ->
    raise(Lexical_error(msg, file, line, column))

let get_input_name () = Sys.argv.(Array.length Sys.argv - 1)

let warning lexbuf msg =
  let p = Lexing.lexeme_start_p lexbuf in
  Printf.eprintf "ocamllex warning:\nFile \"%s\", line %d, character %d: %s.\n"
    p.Lexing.pos_fname p.Lexing.pos_lnum
    (p.Lexing.pos_cnum - p.Lexing.pos_bol + 1) msg;
  flush stderr

let decimal_code  c d u =
  100 * (Char.code c - 48) + 10 * (Char.code d - 48) + (Char.code u - 48)

let char_for_hexadecimal_code d u =
  let d1 = Char.code d in
  let val1 = if d1 >= 97 then d1 - 87
             else if d1 >= 65 then d1 - 55
             else d1 - 48
  in
  let d2 = Char.code u in
  let val2 = if d2 >= 97 then d2 - 87
             else if d2 >= 65 then d2 - 55
             else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

let incr_loc lexbuf delta =
  let pos = lexbuf.Lexing.lex_curr_p in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum - delta;
  }
;;

let update_loc lexbuf opt_file line =
  let pos = lexbuf.Lexing.lex_curr_p in
  let new_file = match opt_file with
                 | None -> pos.Lexing.pos_fname
                 | Some f -> f
  in
  lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_fname = new_file;
    Lexing.pos_lnum = line;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }
;;

}

let identstart =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255']
let identbody =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let backslash_escapes =
  ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']

rule main = parse
    [' ' '\013' '\009' '\012' ] +
    { main lexbuf }
  | '\010'
    { incr_loc lexbuf 0;
      main lexbuf }
  | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
    ('\"' ([^ '\010' '\013' '\"']* as name) '\"')?
    [^ '\010' '\013']* '\010'
    { update_loc lexbuf name (int_of_string num);
      main lexbuf
    }
  | "(*"
    { comment_depth := 1;
      handle_lexical_error comment lexbuf;
      main lexbuf }
  | '_' { Tunderscore }
  | identstart identbody *
    { match Lexing.lexeme lexbuf with
        "rule" -> Trule
      | "parse" -> Tparse
      | "shortest" -> Tparse_shortest
      | "and" -> Tand
      | "eof" -> Teof
      | "let" -> Tlet
      | "as"  -> Tas
      | "refill" -> Trefill
      | s -> Tident s }
  | '"'
    { reset_string_buffer();
      handle_lexical_error string lexbuf;
      Tstring(get_stored_string()) }
(* note: ''' is a valid character literal (by contrast with the compiler) *)
  | "'" [^ '\\'] "'"
    { Tchar(Char.code(Lexing.lexeme_char lexbuf 1)) }
  | "'" '\\' backslash_escapes "'"
    { Tchar(Char.code(char_for_backslash (Lexing.lexeme_char lexbuf 2))) }
  | "'" '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9'] as u)"'"
    { let v = decimal_code c d u in
      if v > 255 then
        raise_lexical_error lexbuf
          (Printf.sprintf "illegal escape sequence \\%c%c%c" c d u)
      else
        Tchar v }
  | "'" '\\' 'x'
       (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u) "'"
       { Tchar(Char.code(char_for_hexadecimal_code d u)) }
  | "'" '\\' (_ as c)
    { raise_lexical_error lexbuf
        (Printf.sprintf "illegal escape sequence \\%c" c)
    }
  | '{'
    { let p = Lexing.lexeme_end_p lexbuf in
      let f = p.Lexing.pos_fname in
      let n1 = p.Lexing.pos_cnum
      and l1 = p.Lexing.pos_lnum
      and s1 = p.Lexing.pos_bol in
      brace_depth := 1;
      let n2 = handle_lexical_error action lexbuf in
      Taction({loc_file = f; start_pos = n1; end_pos = n2;
               start_line = l1; start_col = n1 - s1}) }
  | '='  { Tequal }
  | '|'  { Tor }
  | '['  { Tlbracket }
  | ']'  { Trbracket }
  | '*'  { Tstar }
  | '?'  { Tmaybe }
  | '+'  { Tplus }
  | '('  { Tlparen }
  | ')'  { Trparen }
  | '^'  { Tcaret }
  | '-'  { Tdash }
  | '#'  { Tsharp }
  | eof  { Tend }
  | _
    { raise_lexical_error lexbuf
        ("illegal character " ^ String.escaped(Lexing.lexeme lexbuf))
    }


(* String parsing comes from the compiler lexer *)
and string = parse
    '"'
    { () }
   | '\\' ('\013'* '\010') ([' ' '\009'] * as spaces)
    { incr_loc lexbuf (String.length spaces);
      string lexbuf }
  | '\\' (backslash_escapes as c)
    { store_string_char(char_for_backslash c);
      string lexbuf }
  | '\\' (['0'-'9'] as c) (['0'-'9'] as d) (['0'-'9']  as u)
    { let v = decimal_code c d u in
      if in_pattern () && v > 255 then
       warning lexbuf
        (Printf.sprintf
          "illegal backslash escape in string: '\\%c%c%c'" c d u) ;
      store_string_char (Char.chr v);
      string lexbuf }
 | '\\' 'x' (['0'-'9' 'a'-'f' 'A'-'F'] as d) (['0'-'9' 'a'-'f' 'A'-'F'] as u)
    { store_string_char (char_for_hexadecimal_code d u) ;
      string lexbuf }
  | '\\' (_ as c)
    {if in_pattern () then
       warning lexbuf
        (Printf.sprintf "illegal backslash escape in string: '\\%c'" c) ;
      store_string_char '\\' ;
      store_string_char c ;
      string lexbuf }
  | eof
    { raise(Lexical_error("unterminated string", "", 0, 0)) }
  | '\013'* '\010' as s
    { warning lexbuf (Printf.sprintf "unescaped newline in string") ;
      store_string_chars s;
      incr_loc lexbuf 0;
      string lexbuf }
  | _ as c
    { store_string_char c;
      string lexbuf }

(*
   Lexers comment and action are quite similar,
   they should lex both strings and characters,
   in order not to be confused by what is inside then
*)

and comment = parse
    "(*"
    { incr comment_depth; comment lexbuf }
  | "*)"
    { decr comment_depth;
      if !comment_depth = 0 then () else comment lexbuf }
  | '"'
    { reset_string_buffer();
      string lexbuf;
      reset_string_buffer();
      comment lexbuf }
  | "'"
    { skip_char lexbuf ;
      comment lexbuf }
  | eof
    { raise(Lexical_error("unterminated comment", "", 0, 0)) }
  | '\010'
    { incr_loc lexbuf 0;
      comment lexbuf }
  | _
    { comment lexbuf }

and action = parse
    '{'
    { incr brace_depth;
      action lexbuf }
  | '}'
    { decr brace_depth;
      if !brace_depth = 0 then Lexing.lexeme_start lexbuf else action lexbuf }
  | '"'
    { reset_string_buffer();
      handle_lexical_error string lexbuf;
      reset_string_buffer();
      action lexbuf }
 | "'"
    { skip_char lexbuf ;
      action lexbuf }
 | "(*"
    { comment_depth := 1;
      comment lexbuf;
      action lexbuf }
  | eof
    { raise (Lexical_error("unterminated action", "", 0, 0)) }
  | '\010'
    { incr_loc lexbuf 0;
      action lexbuf }
  | _
    { action lexbuf }

and skip_char = parse
  | '\\'? '\010' "'"
     { incr_loc lexbuf 1;
     }
  | [^ '\\' '\''] "'" (* regular character *)
(* one character and numeric escape sequences *)
  | '\\' _ "'"
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
  | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
     {()}
(* A dieu va ! *)
  | "" {()}
