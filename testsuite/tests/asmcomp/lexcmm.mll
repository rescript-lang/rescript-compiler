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
open Parsecmm

type error =
    Illegal_character
  | Unterminated_comment
  | Unterminated_string

exception Error of error

(* For nested comments *)

let comment_depth = ref 0

(* The table of keywords *)

let keyword_table =
  Misc.create_hashtable 149 [
    "absf", ABSF;
    "addr", ADDR;
    "align", ALIGN;
    "alloc", ALLOC;
    "and", AND;
    "app", APPLY;
    "assign", ASSIGN;
    "byte", BYTE;
    "case", CASE;
    "catch", CATCH;
    "checkbound", CHECKBOUND;
    "exit", EXIT;
    "extcall", EXTCALL;
    "float", FLOAT;
    "float32", FLOAT32;
    "float64", FLOAT64;
    "floatofint", FLOATOFINT;
    "function", FUNCTION;
    "half", HALF;
    "if", IF;
    "int", INT;
    "int32", INT32;
    "intoffloat", INTOFFLOAT;
    "string", KSTRING;
    "let", LET;
    "load", LOAD;
    "mod", MODI;
    "or", OR;
    "proj", PROJ;
    "raise", RAISE Lambda.Raise_regular;
    "reraise", RAISE Lambda.Raise_reraise;
    "raise_notrace", RAISE Lambda.Raise_notrace;
    "seq", SEQ;
    "signed", SIGNED;
    "skip", SKIP;
    "store", STORE;
    "switch", SWITCH;
    "try", TRY;
    "unit", UNIT;
    "unsigned", UNSIGNED;
    "while", WHILE;
    "with", WITH;
    "xor", XOR;
    "addraref", ADDRAREF;
    "intaref", INTAREF;
    "floataref", FLOATAREF;
    "addraset", ADDRASET;
    "intaset", INTASET;
    "floataset", FLOATASET
]

(* To buffer string literals *)

let initial_string_buffer = Bytes.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= Bytes.length (!string_buff) then begin
    let new_buff = Bytes.create (Bytes.length (!string_buff) * 2) in
    Bytes.blit (!string_buff) 0 new_buff 0 (Bytes.length (!string_buff));
    string_buff := new_buff
  end;
  Bytes.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = Bytes.sub_string (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To translate escape sequences *)

let char_for_backslash = function
    'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  Char.chr(100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
               10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                    (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48))

(* Error report *)

let report_error lexbuf msg =
  prerr_string "Lexical error around character ";
  prerr_int (Lexing.lexeme_start lexbuf);
  match msg with
    Illegal_character ->
      prerr_string ": illegal character"
  | Unterminated_comment ->
      prerr_string ": unterminated comment"
  | Unterminated_string ->
      prerr_string ": unterminated string"

}

rule token = parse
    [' ' '\010' '\013' '\009' '\012'] +
      { token lexbuf }
  | "+a" { ADDA }
  | "+f" { ADDF }
  | "+" { ADDI }
  | ">>s" { ASR }
  | ":" { COLON }
  | "/f" { DIVF }
  | "/" { DIVI }
  | eof { EOF }
  | "==a" { EQA }
  | "==f" { EQF }
  | "==" { EQI }
  | ">=a" { GEA }
  | ">=f" { GEF }
  | ">=" { GEI }
  | ">a" { GTA }
  | ">f" { GTF }
  | ">" { GTI }
  | "[" { LBRACKET }
  | "<=a" { LEA }
  | "<=f" { LEF }
  | "<=" { LEI }
  | "(" { LPAREN }
  | "<<" { LSL }
  | ">>u" { LSR }
  | "<a" { LTA }
  | "<f" { LTF }
  | "<" { LTI }
  | "*f" { MULF }
  | "*" { MULI }
  | "!=a" { NEA }
  | "!=f" { NEF }
  | "!=" { NEI }
  | "]" { RBRACKET }
  | ")" { RPAREN }
  | "*" { STAR }
  | "-a" { SUBA }
  | "-f" { SUBF }
  | "-" { SUBI }
  | '-'? (['0'-'9']+ | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
                     | "0o" ['0'-'7']+ | "0b" ['0'-'1']+)
      { INTCONST(int_of_string(Lexing.lexeme lexbuf)) }
  | '-'? ['0'-'9']+ 'a'
      { let s = Lexing.lexeme lexbuf in
        POINTER(int_of_string(String.sub s 0 (String.length s - 1))) }
  | '-'? ['0'-'9']+ ('.' ['0'-'9']*)? (['e' 'E'] ['+' '-']? ['0'-'9']+)?
      { FLOATCONST(Lexing.lexeme lexbuf) }
  | ['A'-'Z' 'a'-'z' '\223'-'\246' '\248'-'\255' ]
    (['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255'
      '\'' '0'-'9' ]) *
      { let s = Lexing.lexeme lexbuf in
        try
          Hashtbl.find keyword_table s
        with Not_found ->
          IDENT s }
  | "\""
      { reset_string_buffer();
        string lexbuf;
        STRING (get_stored_string()) }
  | "(*"
      { comment_depth := 1;
        comment lexbuf;
        token lexbuf }
  | _ { raise(Error(Illegal_character)) }

and comment = parse
    "(*"
      { comment_depth := succ !comment_depth; comment lexbuf }
  | "*)"
      { comment_depth := pred !comment_depth;
        if !comment_depth > 0 then comment lexbuf }
  | eof
      { raise (Error(Unterminated_comment)) }
  | _
      { comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' [' ' '\010' '\013' '\009' '\026' '\012'] +
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r']
      { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      { store_string_char(char_for_decimal_code lexbuf 1);
         string lexbuf }
  | eof
      { raise (Error(Unterminated_string)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
