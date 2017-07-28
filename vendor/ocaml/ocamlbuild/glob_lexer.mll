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


(* Original author: Berke Durak *)
(* Glob *)
{
open Bool;;
open Glob_ast;;

type token =
| ATOM of pattern atom
| AND
| OR
| NOT
| LPAR
| RPAR
| TRUE
| FALSE
| EOF
;;

let sf = Printf.sprintf;;

let concat_patterns p1 p2 =
  match (p1,p2) with
  | (Epsilon,_) -> p2
  | (_,Epsilon) -> p1
  | (_,_)       -> Concat(p1,p2)
;;

let slash = Class(Atom('/','/'));;
let not_slash = Class(Not(Atom('/','/')));;
let any = Class True;;
}

let pattern_chars = ['a'-'z']|['A'-'Z']|'_'|'-'|['0'-'9']|'.'
let space_chars = [' ' '\t' '\n' '\r' '\012']

rule token = parse
| '<'             { ATOM(Pattern(let (p,_) = parse_pattern ['>'] Epsilon lexbuf in p)) }
| '"'             { ATOM(Constant(parse_string (Buffer.create 32) lexbuf)) }
| "and"|"AND"|"&" { AND }
| "or"|"OR"|"|"   { OR }
| "not"|"NOT"|"~" { NOT }
| "true"|"1"      { TRUE }
| "false"|"0"     { FALSE }
| "("             { LPAR }
| ")"             { RPAR }
| space_chars+    { token lexbuf }
| eof             { EOF }

and parse_pattern eof_chars p = parse
| (pattern_chars+ as u) { parse_pattern eof_chars (concat_patterns p (Word u)) lexbuf }
| '{'
  {
    let rec loop pl =
      let (p',c) = parse_pattern ['}';','] Epsilon lexbuf in
      let pl = p' :: pl in
      if c = ',' then
        loop pl
      else
        parse_pattern eof_chars (concat_patterns p (Union pl)) lexbuf
    in
    loop []
  }
| "[^"
  {
    let cl = Not(Or(parse_class [] lexbuf)) in
    parse_pattern eof_chars (concat_patterns p (Class cl)) lexbuf
  }
| '['
  {
    let cl = Or(parse_class [] lexbuf) in
    parse_pattern eof_chars (concat_patterns p (Class cl)) lexbuf
  }
(* Random thought... **/* seems to be equal to True *)
| "/**/" (* / | /\Sigma^*/ *)
  { let q = Union[slash; Concat(slash, Concat(Star any, slash)) ] in
    parse_pattern eof_chars (concat_patterns p q) lexbuf }
| "/**" (* \varepsilon | /\Sigma^* *)
  { let q = Union[Epsilon; Concat(slash, Star any)] in
    parse_pattern eof_chars (concat_patterns p q) lexbuf }
| "**/" (* \varepsilon | \Sigma^*/ *)
  { let q = Union[Epsilon; Concat(Star any, slash)] in
    parse_pattern eof_chars (concat_patterns p q) lexbuf }
| "**" { raise (Parse_error("Ambiguous ** pattern not allowed unless surrounded by one or more slashes")) }
| '*' { parse_pattern eof_chars (concat_patterns p (Star not_slash)) lexbuf }
| '/' { parse_pattern eof_chars (concat_patterns p slash) lexbuf }
| '?' { parse_pattern eof_chars (concat_patterns p not_slash) lexbuf }
| _ as c
  { if List.mem c eof_chars then
      (p,c)
    else
      raise (Parse_error(sf "Unexpected character %C in glob pattern" c))
  }

and parse_string b = parse
| "\""                  { Buffer.contents b }
| "\\\""                { Buffer.add_char b '"'; parse_string b lexbuf }
| [^'"' '\\']+ as u     { Buffer.add_string b u; parse_string b lexbuf }
| _ as c                { raise (Parse_error(sf "Unexpected character %C in string" c)) }

and parse_class cl = parse
| ']'                     { cl }
| "-]"                    { ((Atom('-','-'))::cl) }
| (_ as c1) '-' (_ as c2) { parse_class ((Atom(c1,c2))::cl) lexbuf }
| _ as c                  { parse_class ((Atom(c,c))::cl) lexbuf }
