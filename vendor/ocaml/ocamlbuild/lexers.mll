(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
{
exception Error of (string * Loc.location)

let error source lexbuf fmt =
  Printf.ksprintf (fun s ->
    raise (Error (s, Loc.of_lexbuf source lexbuf))
  ) fmt

open Glob_ast

type conf_values =
  { plus_tags   : (string * Loc.location) list;
    minus_tags  : (string * Loc.location) list }

type conf = (Glob.globber * conf_values) list

let empty = { plus_tags = []; minus_tags = [] }

let locate source lexbuf txt =
  (txt, Loc.of_lexbuf source lexbuf)

let sublex lexer s = lexer (Lexing.from_string s)
}

let newline = ('\n' | '\r' | "\r\n")
let space = [' ' '\t' '\012']
let space_or_esc_nl = (space | '\\' newline)
let sp = space_or_esc_nl
let blank = newline | space
let not_blank = [^' ' '\t' '\012' '\n' '\r']
let not_space_nor_comma = [^' ' '\t' '\012' ',']
let not_newline = [^ '\n' '\r' ]
let not_newline_nor_colon = [^ '\n' '\r' ':' ]
let normal_flag_value = [^ '(' ')' '\n' '\r']
let normal = [^ ':' ',' '(' ')' ''' ' ' '\n' '\r']
let tag = normal+ | ( normal+ ':' normal+ ) | normal+ '(' [^ ')' ]* ')'
let variable = [ 'a'-'z' 'A'-'Z' '_' '-' '0'-'9' ]*
let pattern = ([^ '(' ')' '\\' ] | '\\' [ '(' ')' ])*

rule ocamldep_output source = parse
  | ([^ ':' '\n' '\r' ]+ as k) ':' { let x = (k, space_sep_strings_nl source lexbuf) in x :: ocamldep_output source lexbuf }
  | eof { [] }
  | _ { error source lexbuf "Expecting colon followed by space-separated module name list" }

and space_sep_strings_nl source = parse
  | space* (not_blank+ as word) { word :: space_sep_strings_nl source lexbuf }
  | space* newline { Lexing.new_line lexbuf; [] }
  | _ { error source lexbuf "Expecting space-separated strings terminated with newline" }

and space_sep_strings source = parse
  | space* (not_blank+ as word) { word :: space_sep_strings source lexbuf }
  | space* newline? eof { [] }
  | _ { error source lexbuf "Expecting space-separated strings" }

and blank_sep_strings source = parse
  | blank* '#' not_newline* newline { blank_sep_strings source lexbuf }
  | blank* '#' not_newline* eof { [] }
  | blank* (not_blank+ as word) { word :: blank_sep_strings source lexbuf }
  | blank* eof { [] }
  | _ { error source lexbuf "Expecting blank-separated strings" }

and comma_sep_strings source = parse
  | space* (not_space_nor_comma+ as word) space* eof { [word] }
  | space* (not_space_nor_comma+ as word) { word :: comma_sep_strings_aux source lexbuf }
  | space* eof { [] }
  | _ { error source lexbuf "Expecting comma-separated strings (1)" }
and comma_sep_strings_aux source = parse
  | space* ',' space* (not_space_nor_comma+ as word) { word :: comma_sep_strings_aux source lexbuf }
  | space* eof { [] }
  | _ { error source lexbuf "Expecting comma-separated strings (2)" }

and comma_or_blank_sep_strings source = parse
  | space* (not_space_nor_comma+ as word) space* eof { [word] }
  | space* (not_space_nor_comma+ as word) { word :: comma_or_blank_sep_strings_aux source lexbuf }
  | space* eof { [] }
  | _ { error source lexbuf "Expecting (comma|blank)-separated strings (1)" }
and comma_or_blank_sep_strings_aux source = parse
  | space* ',' space* (not_space_nor_comma+ as word) { word :: comma_or_blank_sep_strings_aux source lexbuf }
  | space* (not_space_nor_comma+ as word) { word :: comma_or_blank_sep_strings_aux source lexbuf }
  | space* eof { [] }
  | _ { error source lexbuf "Expecting (comma|blank)-separated strings (2)" }

and parse_environment_path_w source = parse
  | ([^ ';']* as word) { word :: parse_environment_path_aux_w source lexbuf }
  | ';' ([^ ';']* as word) { "" :: word :: parse_environment_path_aux_w source lexbuf }
  | eof { [] }
and parse_environment_path_aux_w source = parse
  | ';' ([^ ';']* as word) { word :: parse_environment_path_aux_w source lexbuf }
  | eof { [] }
  | _ { error source lexbuf "Impossible: expecting colon-separated strings" }

and parse_environment_path source = parse
  | ([^ ':']* as word) { word :: parse_environment_path_aux source lexbuf }
  | ':' ([^ ':']* as word) { "" :: word :: parse_environment_path_aux source lexbuf }
  | eof { [] }
and parse_environment_path_aux source = parse
  | ':' ([^ ':']* as word) { word :: parse_environment_path_aux source lexbuf }
  | eof { [] }
  | _ { error source lexbuf "Impossible: expecting colon-separated strings" }

and conf_lines dir source = parse
  | space* '#' not_newline* newline { Lexing.new_line lexbuf; conf_lines dir source lexbuf }
  | space* '#' not_newline* eof { [] }
  | space* newline { Lexing.new_line lexbuf; conf_lines dir source lexbuf }
  | space* eof { [] }
  | space* (not_newline_nor_colon+ as k) (sp* as s1) ':' (sp* as s2)
      {
        let bexpr =
          try Glob.parse ?dir k
          with exn -> error source lexbuf "Invalid globbing pattern %S" k (Printexc.to_string exn)
        in
        sublex (count_lines lexbuf) s1; sublex (count_lines lexbuf) s2;
        let v1 = conf_value empty source lexbuf in
        let v2 = conf_values v1 source lexbuf in
        let rest = conf_lines dir source lexbuf in (bexpr,v2) :: rest
      }
  | _ { error source lexbuf "Invalid line syntax" }

and conf_value x source = parse
  | '-'  (tag as tag) { { (x) with minus_tags = locate source lexbuf tag :: x.minus_tags } }
  | '+'? (tag as tag) { { (x) with plus_tags = locate source lexbuf tag :: x.plus_tags } }
  | (_ | eof) { error source lexbuf "Invalid tag modifier only '+ or '-' are allowed as prefix for tag" }

and conf_values x source = parse
  | (sp* as s1) ',' (sp* as s2) {
      sublex (count_lines lexbuf) s1; sublex (count_lines lexbuf) s2;
      conf_values (conf_value x source lexbuf) source lexbuf
    }
  | newline { Lexing.new_line lexbuf; x }
  | eof { x }
  | _ { error source lexbuf "Only ',' separated tags are alllowed" }

and path_scheme patt_allowed source = parse
  | ([^ '%' ]+ as prefix)
      { `Word prefix :: path_scheme patt_allowed source lexbuf }
  | "%(" (variable as var) ')'
      { `Var (var, Bool.True) :: path_scheme patt_allowed source lexbuf }
  | "%(" (variable as var) ':' (pattern as patt) ')'
      { if patt_allowed then
          let patt = My_std.String.implode (unescape (Lexing.from_string patt)) in
          `Var (var, Glob.parse patt) :: path_scheme patt_allowed source lexbuf
        else
          error source lexbuf "Patterns are not allowed in this pathname (%%(%s:%s) only in ~prod)" var patt }
  | '%'
      { `Var ("", Bool.True) :: path_scheme patt_allowed source lexbuf }
  | eof
      { [] }
  | _ { error source lexbuf "Bad pathanme scheme" }

and unescape = parse
  | '\\' (['(' ')'] as c)        { c :: unescape lexbuf }
  | _ as c                       { c :: unescape lexbuf }
  | eof                          { [] }

and ocamlfind_query source = parse
  | newline*
    "package:" space* (not_newline* as n) newline+
    "description:" space* (not_newline* as d) newline+
    "version:" space* (not_newline* as v) newline+
    "archive(s):" space* (not_newline* as a) newline+
    "linkopts:" space* (not_newline* as lo) newline+
    "location:" space* (not_newline* as l) newline+
    { n, d, v, a, lo, l }
  | _ { error source lexbuf "Bad ocamlfind query" }

and trim_blanks source = parse
  | blank* (not_blank* as word) blank* { word }
  | _ { error source lexbuf "Bad input for trim_blanks" }

and tag_gen source = parse
  | (normal+ as name) ('(' ([^')']* as param) ')')? { name, param }
  | _ { error source lexbuf "Not a valid parametrized tag" }

and count_lines lb = parse
  | space* { count_lines lb lexbuf }
  | '\\' newline { Lexing.new_line lb; count_lines lb lexbuf }
  | eof { () }
