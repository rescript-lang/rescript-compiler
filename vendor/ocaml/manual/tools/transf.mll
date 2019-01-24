{
  open Lexing;;
  open Printf;;

  let print_char_repr c =
  match c with
  | '\'' -> printf "{\\textquotesingle}"
  | '`' -> printf "{\\textasciigrave}"
  | _ -> printf "\\char%d" (int_of_char c);
  ;;
}

rule main = parse
    "\\begin{syntax}" {
      print_string "\\begin{syntax}";
      syntax lexbuf }
  | "\\begin{verbatim}" {
      print_string "\\begin{verbatim}";
      verbatim lexbuf }
  | "\\@" {
      print_string "@";
      main lexbuf }
  | "@" {
      print_string "\\synt{";
      syntax lexbuf }
  | _ {
      print_char (lexeme_char lexbuf 0); main lexbuf }
  | eof {
      () }

and syntax = parse
    "\\end{syntax}" {
      print_string "\\end{syntax}";
      main lexbuf }
  | "@" {
      print_string "}";
      main lexbuf }
  | '\'' {
      print_string "\\token{";
      inquote lexbuf }
  | '\"' {
      print_string "\\token{";
      indoublequote lexbuf }
  | "epsilon" { print_string "\\emptystring"; syntax lexbuf }
  | ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-'] * as lxm {
      print_string "\\nonterm{";
      print_string lxm ;
      print_string"}";
      syntax lexbuf }
  | '@' (['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '-'] * as lxm) '@' {
      print_string "\\nt{";
      print_string lxm ;
      print_string"}";
      syntax lexbuf }

  | '\\' ['a'-'z''A'-'Z'] + {
      print_string (lexeme lexbuf);
      syntax lexbuf }
  | ['_' '^'] _ {
      print_string (lexeme lexbuf);
      syntax lexbuf }
  | "{" { print_string "\\brepet{}"; syntax lexbuf }
  | "}" { print_string "\\erepet{}"; syntax lexbuf }
  | "{{" { print_string "\\brepets{}"; syntax lexbuf }
  | "}}" { print_string "\\erepets{}"; syntax lexbuf }
  | "[" { print_string "\\boption{}"; syntax lexbuf }
  | "]" { print_string "\\eoption{}"; syntax lexbuf }
  | "(" { print_string "\\bparen{}"; syntax lexbuf }
  | ")" { print_string "\\eparen{}"; syntax lexbuf }
  | "||" { print_string "\\orelse{}"; syntax lexbuf }
  | ":" { print_string "\\is{}"; syntax lexbuf }
  | "|" { print_string "\\alt{}"; syntax lexbuf }
  | ";" { print_string "\\sep{}"; syntax lexbuf }
  | "\\\\" { print_string "\\cutline{}"; syntax lexbuf }
  | _ {
      print_char (lexeme_char lexbuf 0);
      syntax lexbuf }

and inquote = parse
    ['A'-'Z' 'a'-'z' '0'-'9'] {
      print_char (lexeme_char lexbuf 0);
      inquote lexbuf }
  | '\'' {
      print_string "}";
      syntax lexbuf }
  | _ {
      print_char_repr (lexeme_char lexbuf 0);
      inquote lexbuf }

and indoublequote = parse
    ['A'-'Z' 'a'-'z' '0'-'9'] {
      print_char (lexeme_char lexbuf 0);
      indoublequote lexbuf }
  | '"' {
      print_string "}";
      syntax lexbuf }
  | _ {
      print_char_repr (lexeme_char lexbuf 0);
      indoublequote lexbuf }

and verbatim = parse
    "\n\\end{verbatim}" {
      print_string "\n\\end{verbatim}";
      main lexbuf }
  | _ {
      print_char (lexeme_char lexbuf 0);
      verbatim lexbuf }
