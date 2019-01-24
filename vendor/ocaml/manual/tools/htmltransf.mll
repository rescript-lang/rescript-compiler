{
open Lexing;;

let need_space =
  ref false;;

let addspace () =
  if !need_space then begin print_char ' '; need_space := false end;;
}

rule main = parse
    "\\begin{syntax}" {
      print_string "\\begin{rawhtml}\n<PRE>\n";
      need_space := false;
      syntax lexbuf;
      print_string "</PRE>\n\\end{rawhtml}\n";
      main lexbuf }
  | "\\@" {
      print_string "@";
      main lexbuf }
  | "@" {
      print_string "%\n\\begin{rawhtml}";
      need_space := false;
      syntax lexbuf;
      print_string "\\end{rawhtml}%\n";
      main lexbuf }
  | _ {
      print_char (lexeme_char lexbuf 0); main lexbuf }
  | eof {
      () }

and syntax = parse
    "\\end{syntax}" { () }
  | "@" { () }
  | '\'' {
      addspace();
      print_string "<font color=\"blue\"><code>";
      inquote lexbuf;
      print_string "</code></font>";
      need_space := true;
      syntax lexbuf }
  | '\"' {
      addspace();
      print_string "<font color=\"blue\"><code>";
      indoublequote lexbuf;
      print_string "</code></font>";
      need_space := true;
      syntax lexbuf }
  | ['a'-'z'] ['a'-'z' '0'-'9' '-'] * {
      addspace();
      print_string "<i>";
      print_string (lexeme lexbuf);
      print_string "</i>";
      need_space := true;
      syntax lexbuf }
  | '\\' ['a'-'z''A'-'Z'] + {
      begin match lexeme lexbuf with
        "\\ldots" -> print_string "..."; need_space := false
      | s -> Printf.eprintf "Warning: %s ignored.\n" s
      end;
      syntax lexbuf }
  | '_' _ {
      print_string "<SUB>";
      print_char(lexeme_char lexbuf 1);
      print_string "</SUB>";
      syntax lexbuf }
  | '^' _ {
      print_string "<SUP>";
      print_char(lexeme_char lexbuf 1);
      print_string "</SUP>";
      syntax lexbuf }
  | ":" {
      print_string ":\n      ";
      need_space := false;
      syntax lexbuf }
  | "|" {
      print_string "\n   |  ";
      need_space := false;
      syntax lexbuf }
  | ";" {
      print_string "\n\n";
      need_space := false;
      syntax lexbuf }
  | [ '{' '[' '('] {
      addspace(); print_string (lexeme lexbuf); syntax lexbuf }
  | [ '}' ']' ')'] {
      print_string (lexeme lexbuf); syntax lexbuf }
  | "{{" {
      addspace(); print_string "{"; syntax lexbuf }
  | "}}" {
      print_string "}+"; syntax lexbuf }
  | "||" {
      print_string " | "; need_space := false; syntax lexbuf }
  | [ ' ' '\n' '\t' '~'] {
      syntax lexbuf }
  | [ ',' ] {
      print_char(lexeme_char lexbuf 0); syntax lexbuf }
  | _ {
      Printf.eprintf "Warning: %s ignored at char %d.\n"
                      (lexeme lexbuf) (lexeme_start lexbuf);
      syntax lexbuf }

and inquote = parse
    '\'' { () }
  | '&' { print_string "&amp;"; inquote lexbuf }
  | '<' { print_string "&lt;"; inquote lexbuf }
  | '>' { print_string "&gt;"; inquote lexbuf }
  | _   { print_char (lexeme_char lexbuf 0); inquote lexbuf }

and indoublequote = parse
    '"' { () }
  | '&' { print_string "&amp;"; indoublequote lexbuf }
  | '<' { print_string "&lt;"; indoublequote lexbuf }
  | '>' { print_string "&gt;"; indoublequote lexbuf }
  | _   { print_char (lexeme_char lexbuf 0); indoublequote lexbuf }


