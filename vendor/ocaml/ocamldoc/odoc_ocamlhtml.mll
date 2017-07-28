
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

(** Generation of html code to display OCaml code. *)
open Lexing

exception Fatal_error

let fatal_error msg =
  prerr_string ">> Fatal error: "; prerr_endline msg; raise Fatal_error

type error =
  | Illegal_character of char
  | Unterminated_comment
  | Unterminated_string
  | Unterminated_string_in_comment
  | Keyword_as_label of string
;;

exception Error of error * int * int

let base_escape_strings = [
    ("&", "&amp;") ;
    ("<", "&lt;") ;
    (">", "&gt;") ;
]

let pre_escape_strings = [
  (" ", "&nbsp;") ;
  ("\n", "<br>\n") ;
  ("\t", "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;") ;
  ]


let pre = ref false
let fmt = ref Format.str_formatter

(** Escape the strings which would clash with html syntax,
   and some other strings if we want to get a PRE style.*)
let escape s =
  List.fold_left
    (fun acc -> fun (s, s2) -> Str.global_replace (Str.regexp s) s2 acc)
    s
    (if !pre then base_escape_strings @ pre_escape_strings else base_escape_strings)

(** Escape the strings which would clash with html syntax. *)
let escape_base s =
  List.fold_left
    (fun acc -> fun (s, s2) -> Str.global_replace (Str.regexp s) s2 acc)
    s
    base_escape_strings

(** The output functions *)

let print ?(esc=true) s =
  Format.pp_print_string !fmt (if esc then escape s else s)
;;

let print_class ?(esc=true) cl s =
  print ~esc: false ("<span class=\""^cl^"\">"^
                     (if esc then escape s else s)^
                     "</span>")
;;

(** The table of keywords with colors *)
let create_hashtable size init =
  let tbl = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  tbl

(** The function used to return html code for the given comment body. *)
let html_of_comment = ref
    (fun (s : string) -> "<b>Odoc_ocamlhtml.html_of_comment not initialized</b>")

let keyword_table =
  create_hashtable 149 [
    "and", "keyword" ;
    "as", "keyword" ;
    "assert", "keyword" ;
    "begin", "keyword" ;
    "class", "keyword" ;
    "constraint", "keyword" ;
    "do", "keyword" ;
    "done", "keyword" ;
    "downto", "keyword" ;
    "else", "keyword" ;
    "end", "keyword" ;
    "exception", "keyword" ;
    "external", "keyword" ;
    "false", "keyword" ;
    "for", "keyword" ;
    "fun", "keyword" ;
    "function", "keyword" ;
    "functor", "keyword" ;
    "if", "keyword" ;
    "in", "keyword" ;
    "include", "keyword" ;
    "inherit", "keyword" ;
    "initializer", "keyword" ;
    "lazy", "keyword" ;
    "let", "keyword" ;
    "match", "keyword" ;
    "method", "keyword" ;
    "module", "keyword" ;
    "mutable", "keyword" ;
    "new", "keyword" ;
    "object", "keyword" ;
    "of", "keyword" ;
    "open", "keyword" ;
    "or", "keyword" ;
    "parser", "keyword" ;
    "private", "keyword" ;
    "rec", "keyword" ;
    "sig", "keyword" ;
    "struct", "keyword" ;
    "then", "keyword" ;
    "to", "keyword" ;
    "true", "keyword" ;
    "try", "keyword" ;
    "type", "keyword" ;
    "val", "keyword" ;
    "virtual", "keyword" ;
    "when", "keyword" ;
    "while", "keyword" ;
    "with", "keyword" ;

    "mod", "keyword" ;
    "land", "keyword" ;
    "lor", "keyword" ;
    "lxor", "keyword" ;
    "lsl", "keyword" ;
    "lsr", "keyword" ;
    "asr", "keyword" ;
]

let kwsign_class = "keywordsign"
let constructor_class = "constructor"
let comment_class = "comment"
let string_class = "string"
let code_class = "code"


(** To buffer and print comments *)


let margin = ref 0

let comment_buffer = Buffer.create 32
let reset_comment_buffer () = Buffer.reset comment_buffer
let store_comment_char = Buffer.add_char comment_buffer
let add_comment_string = Buffer.add_string comment_buffer

let make_margin () =
  let rec iter n =
    if n <= 0 then ""
    else "&nbsp;"^(iter (n-1))
  in
  iter !margin

let print_comment () =
  let s = Buffer.contents comment_buffer in
  let len = String.length s in
  let code =
    if len < 1 then
      "<span class=\""^comment_class^"\">(*"^(escape s)^"*)</span>"
    else
      match s.[0] with
        '*' ->
          (
           try
             let html = !html_of_comment (String.sub s 1 (len-1)) in
             "</code><table><tr><td>"^(make_margin ())^"</td><td>"^
             "<span class=\""^comment_class^"\">"^
             "(**"^html^"*)"^
             "</span></td></tr></table><code class=\""^code_class^"\">"
           with
             e ->
               prerr_endline (Printexc.to_string e);
               "<span class=\""^comment_class^"\">(*"^(escape s)^"*)</span>"
          )
      | _ ->
          "<span class=\""^comment_class^"\">(*"^(escape s)^"*)</span>"
  in
  print ~esc: false code

(** To buffer string literals *)

let string_buffer = Buffer.create 32
let reset_string_buffer () = Buffer.reset string_buffer
let store_string_char = Buffer.add_char string_buffer
let get_stored_string () =
  let s = Buffer.contents string_buffer in
  s

(** To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr(c land 0xFF)

let char_for_hexa_code lexbuf i =
  let c = 16 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) in
  Char.chr(c land 0xFF)

(** To store the position of the beginning of a string and comment *)
let string_start_pos = ref 0;;
let comment_start_pos = ref [];;
let in_comment () = !comment_start_pos <> [];;

(** Error report *)

open Format

let report_error ppf = function
  | Illegal_character c ->
      fprintf ppf "Illegal character (%s)" (Char.escaped c)
  | Unterminated_comment ->
      fprintf ppf "Comment not terminated"
  | Unterminated_string ->
      fprintf ppf "String literal not terminated"
  | Unterminated_string_in_comment ->
      fprintf ppf "This comment contains an unterminated string literal"
  | Keyword_as_label kwd ->
      fprintf ppf "`%s' is a keyword, it cannot be used as label name" kwd
;;

}

let blank = [' ' '\010' '\013' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal = ['0'-'9']+
let hex_literal = '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']+
let oct_literal = '0' ['o' 'O'] ['0'-'7']+
let bin_literal = '0' ['b' 'B'] ['0'-'1']+
let float_literal =
  ['0'-'9']+ ('.' ['0'-'9']* )? (['e' 'E'] ['+' '-']? ['0'-'9']+)?

rule token = parse
    blank
      {
        let s = Lexing.lexeme lexbuf in
        (
         match s with
           " " -> incr margin
         | "\t" -> margin := !margin + 8
         | "\n" -> margin := 0
         | _ -> ()
        );
        print s;
        token lexbuf
      }
  | "_"
      { print "_" ; token lexbuf }
  | "~"  { print "~" ; token lexbuf }
  | "~" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        if Hashtbl.mem keyword_table name then
          raise (Error(Keyword_as_label name, Lexing.lexeme_start lexbuf,
                       Lexing.lexeme_end lexbuf));
        print s ; token lexbuf }
  | "?"  { print "?" ; token lexbuf }
  | "?" lowercase identchar * ':'
      { let s = Lexing.lexeme lexbuf in
        let name = String.sub s 1 (String.length s - 2) in
        if Hashtbl.mem keyword_table name then
          raise (Error(Keyword_as_label name, Lexing.lexeme_start lexbuf,
                       Lexing.lexeme_end lexbuf));
        print s ; token lexbuf }
  | lowercase identchar *
      { let s = Lexing.lexeme lexbuf in
          try
            let cl = Hashtbl.find keyword_table s in
            (print_class cl s ; token lexbuf )
          with Not_found ->
            (print s ; token lexbuf )}
  | uppercase identchar *
      { print_class constructor_class (Lexing.lexeme lexbuf) ; token lexbuf }       (* No capitalized keywords *)
  | decimal_literal | hex_literal | oct_literal | bin_literal
      { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | float_literal
      { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "\""
      { reset_string_buffer();
        let string_start = Lexing.lexeme_start lexbuf in
        string_start_pos := string_start;
        string lexbuf;
        lexbuf.Lexing.lex_start_pos <-
          string_start - lexbuf.Lexing.lex_abs_pos;
        print_class string_class ("\""^(get_stored_string())^"\"") ;
        token lexbuf }
  | "'" [^ '\\' '\''] "'"
      { print_class string_class (Lexing.lexeme lexbuf) ;
        token lexbuf }
  | "'" '\\' ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      { print_class string_class (Lexing.lexeme lexbuf ) ;
        token lexbuf }
  | "'" '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
      { print_class string_class (Lexing.lexeme lexbuf ) ;
        token lexbuf }
  | "(*"
      {
        reset_comment_buffer ();
        comment_start_pos := [Lexing.lexeme_start lexbuf];
        comment lexbuf ;
        print_comment ();
        token lexbuf }
  | "(*)"
      { reset_comment_buffer ();
        comment_start_pos := [Lexing.lexeme_start lexbuf];
        comment lexbuf ;
        print_comment ();
        token lexbuf
      }
  | "*)"
      { lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
        lexbuf.Lexing.lex_curr_p <-
          { lexbuf.Lexing.lex_curr_p with
            pos_cnum = lexbuf.Lexing.lex_curr_p.pos_cnum - 1
          } ;
        print (Lexing.lexeme lexbuf) ;
        token lexbuf
      }
  | "#" [' ' '\t']* ['0'-'9']+ [^ '\n' '\r'] * ('\n' | '\r' | "\r\n")
      (* # linenum ...  *)
      {
        print (Lexing.lexeme lexbuf);
        token lexbuf
      }
  | "#"  { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | "&"  { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | "&&" { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | "`"  { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | "'"  { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | "("  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ")"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "*"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ","  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "??" { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | "->" { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | "."  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ".." { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ":"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "::" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ":=" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ":>" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ";"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ";;" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "<"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "<-" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "="  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "["  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "[|" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "[<" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "]"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "{"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "{<" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "|"  { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | "||" { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | "|]" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ">"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ">]" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "}"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ">}" { print (Lexing.lexeme lexbuf) ; token lexbuf }

  | "!=" { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "+"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "-"  { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "-." { print (Lexing.lexeme lexbuf) ; token lexbuf }

  | "!" symbolchar *
            { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ['~' '?'] symbolchar +
            { print_class kwsign_class (Lexing.lexeme lexbuf) ; token lexbuf }
  | ['=' '<' '>' '|' '&' '$'] symbolchar *
            { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ['@' '^'] symbolchar *
            { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ['+' '-'] symbolchar *
            { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | "**" symbolchar *
            { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | ['*' '/' '%'] symbolchar *
            { print (Lexing.lexeme lexbuf) ; token lexbuf }
  | eof { () }
  | _
      { raise (Error(Illegal_character ((Lexing.lexeme lexbuf).[0]),
                     Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)) }

and comment = parse
    "(*"
      { comment_start_pos := Lexing.lexeme_start lexbuf :: !comment_start_pos;
        store_comment_char '(';
        store_comment_char '*';
        comment lexbuf;
      }
  | "*)"
      { match !comment_start_pos with
        | [] -> assert false
        | [x] -> comment_start_pos := []
        | _ :: l ->
            store_comment_char '*';
            store_comment_char ')';
            comment_start_pos := l;
            comment lexbuf;
       }
(* These filters are useless
  | "\""
      { reset_string_buffer();
        string_start_pos := Lexing.lexeme_start lexbuf;
        store_comment_char '"';
        begin
          try string lexbuf; add_comment_string ((get_stored_string()^"\""))
          with Error (Unterminated_string, _, _) ->
          let st = List.hd !comment_start_pos in
          raise (Error (Unterminated_string_in_comment, st, st + 2))
        end;
        comment lexbuf }
  | "'" [^ '\\' '\''] "'"
      {
        store_comment_char '\'';
        store_comment_char (Lexing.lexeme_char lexbuf 1);
        store_comment_char '\'';
        comment lexbuf }
  | "'\\" ['\\' '\'' 'n' 't' 'b' 'r'] "'"
      {
        store_comment_char '\'';
        store_comment_char '\\';
        store_comment_char(char_for_backslash(Lexing.lexeme_char lexbuf 1)) ;
        store_comment_char '\'';
        comment lexbuf }
  | "\\" ['0'-'9'] ['0'-'9'] ['0'-'9']
      {
        store_comment_char(char_for_decimal_code lexbuf 1);
        comment lexbuf }
  | "\\x" ['0'-'9' 'A'-'Z' 'a'-'z' ] ['0'-'9' 'A'-'Z' 'a'-'z']
      {
        store_comment_char(char_for_hexa_code lexbuf 2);
        string lexbuf }
  | "''"
      {
        store_comment_char '\'';
        store_comment_char '\'';
        comment lexbuf }
*)
  | eof
      { let st = List.hd !comment_start_pos in
        raise (Error (Unterminated_comment, st, st + 2));
      }
  | _
      { store_comment_char(Lexing.lexeme_char lexbuf 0);
        comment lexbuf }

and string = parse
    '"'
      { () }
  | '\\' ("\010" | "\013" | "\013\010") [' ' '\009'] *
      { string lexbuf }
  | '\\' ['\\' '"' 'n' 't' 'b' 'r' ]
      { Buffer.add_string string_buffer (Lexing.lexeme lexbuf) ;
        string lexbuf }
  | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
      {
        Buffer.add_string string_buffer (Lexing.lexeme lexbuf) ;
        string lexbuf
      }
  | '\\' 'x' ['0'-'9' 'A'-'Z' 'a'-'z' ] ['0'-'9' 'A'-'Z' 'a'-'z']
      {  Buffer.add_string string_buffer (Lexing.lexeme lexbuf) ;
         string lexbuf }
  | eof
      { raise (Error (Unterminated_string,
                      !string_start_pos, !string_start_pos+1)) }
  | _
      { store_string_char(Lexing.lexeme_char lexbuf 0);
        string lexbuf }
{

let html_of_code b ?(with_pre=true) code =
  let old_pre = !pre in
  let old_margin = !margin in
  let old_comment_buffer = Buffer.contents comment_buffer in
  let old_string_buffer = Buffer.contents string_buffer in
  let buf = Buffer.create 256 in
  let old_fmt = !fmt in
  fmt := Format.formatter_of_buffer buf ;
  pre := with_pre;
  margin := 0;

  let start = "<code class=\""^code_class^"\">" in
  let ending = "</code>" in
  let html =
    (
     try
       print ~esc: false start ;
       let lexbuf = Lexing.from_string code in
       let _ = token lexbuf  in
       print ~esc: false ending ;
       Format.pp_print_flush !fmt () ;
       Buffer.contents buf
     with
       _ ->
         (* flush str_formatter because we already output
            something in it *)
         Format.pp_print_flush !fmt () ;
         start^code^ending
    )
  in
  pre := old_pre;
  margin := old_margin ;
  Buffer.reset comment_buffer;
  Buffer.add_string comment_buffer old_comment_buffer ;
  Buffer.reset string_buffer;
  Buffer.add_string string_buffer old_string_buffer ;
  fmt := old_fmt ;

  Buffer.add_string b html

}
