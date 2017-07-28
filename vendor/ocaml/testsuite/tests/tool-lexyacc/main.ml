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

(* The lexer generator. Command-line parsing. *)

open Syntax
open Scanner
open Grammar
open Lexgen
open Output

let main () =
  if Array.length Sys.argv <> 2 then begin
    prerr_string "Usage: camllex <input file>\n";
    exit 2
  end;
  let source_name = Sys.argv.(1) in
  let dest_name =
    if Filename.check_suffix source_name ".mll" then
      Filename.chop_suffix source_name ".mll" ^ ".ml"
    else
      source_name ^ ".ml" in
  ic := open_in source_name;
(*  oc := open_out dest_name; *) ignore dest_name;
  oc := stdout;
  let lexbuf = Lexing.from_channel !ic in
  let (Lexdef(header,_) as def) =
    try
      Grammar.lexer_definition Scanner.main lexbuf
    with
      Parsing.Parse_error ->
        prerr_string "Syntax error around char ";
        prerr_int (Lexing.lexeme_start lexbuf);
        prerr_endline ".";
        exit 2
    | Scan_aux.Lexical_error s ->
        prerr_string "Lexical error around char ";
        prerr_int (Lexing.lexeme_start lexbuf);
        prerr_string ": ";
        prerr_string s;
        prerr_endline ".";
        exit 2 in
  let ((init, states, acts) as dfa) = make_dfa def in
  output_lexdef header dfa;
  close_in !ic;
  close_out !oc

let _ = main(); exit 0


(*****
let main () =
  ic := stdin;
  oc := stdout;
  let lexbuf = lexing.from_channel ic in
  let (Lexdef(header,_) as def) =
    try
      grammar.lexer_definition scanner.main lexbuf
    with
      parsing.Parse_error x ->
        prerr_string "Syntax error around char ";
        prerr_int (lexing.lexeme_start lexbuf);
        prerr_endline ".";
        sys.exit 2
    | scan_aux.Lexical_error s ->
        prerr_string "Lexical error around char ";
        prerr_int (lexing.lexeme_start lexbuf);
        prerr_string ": ";
        prerr_string s;
        prerr_endline ".";
        sys.exit 2 in
  let ((init, states, acts) as dfa) = make_dfa def in
  output_lexdef header dfa

****)

(****
let debug_scanner lexbuf =
  let tok = scanner.main lexbuf in
  begin match tok with
    Tident s -> prerr_string "Tident "; prerr_string s
  | Tchar c -> prerr_string "Tchar "; prerr_char c
  | Tstring s -> prerr_string "Tstring "; prerr_string s
  | Taction(Location(i1,i2)) ->
      prerr_string "Taction "; prerr_int i1; prerr_string "-";
      prerr_int i2
  | Trule -> prerr_string "Trule"
  | Tparse -> prerr_string "Tparse"
  | Tand -> prerr_string "Tand"
  | Tequal -> prerr_string "Tequal"
  | Tend -> prerr_string "Tend"
  | Tor -> prerr_string "Tor"
  | Tunderscore -> prerr_string "Tunderscore"
  | Teof -> prerr_string "Teof"
  | Tlbracket -> prerr_string "Tlbracket"
  | Trbracket -> prerr_string "Trbracket"
  | Tstar -> prerr_string "Tstar"
  | Tmaybe -> prerr_string "Tmaybe"
  | Tplus -> prerr_string "Tplus"
  | Tlparen -> prerr_string "Tlparen"
  | Trparen -> prerr_string "Trparen"
  | Tcaret -> prerr_string "Tcaret"
  | Tdash -> prerr_string "Tdash"
  end;
  prerr_newline();
  tok

****)
