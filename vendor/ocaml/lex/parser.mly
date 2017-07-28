/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

/* The grammar for lexer definitions */

%{
open Syntax

(* Auxiliaries for the parser. *)

let named_regexps =
  (Hashtbl.create 13 : (string, regular_expression) Hashtbl.t)

let regexp_for_string s =
  let rec re_string n =
    if n >= String.length s then Epsilon
    else if succ n = String.length s then
      Characters (Cset.singleton (Char.code s.[n]))
    else
      Sequence
        (Characters(Cset.singleton (Char.code s.[n])),
         re_string (succ n))
  in re_string 0

let rec remove_as = function
  | Bind (e,_) -> remove_as e
  | Epsilon|Eof|Characters _ as e -> e
  | Sequence (e1, e2) -> Sequence (remove_as e1, remove_as e2)
  | Alternative (e1, e2) -> Alternative (remove_as e1, remove_as e2)
  | Repetition e -> Repetition (remove_as e)

let as_cset = function
  | Characters s -> s
  | _ -> raise Cset.Bad

%}

%token <string> Tident
%token <int> Tchar
%token <string> Tstring
%token <Syntax.location> Taction
%token Trule Tparse Tparse_shortest Tand Tequal Tend Tor Tunderscore Teof
       Tlbracket Trbracket Trefill
%token Tstar Tmaybe Tplus Tlparen Trparen Tcaret Tdash Tlet Tas Tsharp

%right Tas
%left Tor
%nonassoc CONCAT
%nonassoc Tmaybe Tstar Tplus
%left Tsharp
%nonassoc Tident Tchar Tstring Tunderscore Teof Tlbracket Tlparen

%start lexer_definition
%type <Syntax.lexer_definition> lexer_definition

%%

lexer_definition:
    header named_regexps refill_handler Trule definition other_definitions
    header Tend
        { {header = $1;
           refill_handler = $3;
           entrypoints = $5 :: List.rev $6;
           trailer = $7} }
;
header:
    Taction
        { $1 }
  | /*epsilon*/
        { { loc_file = ""; start_pos = 0; end_pos = 0; start_line = 1;
            start_col = 0 } }
;
named_regexps:
    named_regexps Tlet Tident Tequal regexp
        { Hashtbl.add named_regexps $3 $5 }
  | /*epsilon*/
        { () }
;
other_definitions:
    other_definitions Tand definition
        { $3::$1 }
  | /*epsilon*/
        { [] }
;
refill_handler:
  | Trefill Taction { Some $2 }
  | /*empty*/ { None }
;
definition:
    Tident arguments Tequal Tparse entry
        { {name=$1 ; shortest=false ; args=$2 ; clauses=$5} }
  |  Tident arguments Tequal Tparse_shortest entry
        { {name=$1 ; shortest=true ; args=$2 ; clauses=$5} }
;

arguments:
    Tident arguments        { $1::$2 }
|     /*epsilon*/           { [] }
;


entry:
    case rest_of_entry
        { $1::List.rev $2 }
|   Tor case rest_of_entry
        { $2::List.rev $3 }
;

rest_of_entry:
    rest_of_entry Tor case
        { $3::$1 }
  |
        { [] }
;
case:
    regexp Taction
        { ($1,$2) }
;
regexp:
    Tunderscore
        { Characters Cset.all_chars }
  | Teof
        { Eof }
  | Tchar
        { Characters (Cset.singleton $1) }
  | Tstring
        { regexp_for_string $1 }
  | Tlbracket char_class Trbracket
        { Characters $2 }
  | regexp Tstar
        { Repetition $1 }
  | regexp Tmaybe
        { Alternative(Epsilon, $1) }
  | regexp Tplus
        { Sequence(Repetition (remove_as $1), $1) }
  | regexp Tsharp regexp
        {
          let s1 = as_cset $1
          and s2 = as_cset $3 in
          Characters (Cset.diff s1 s2)
        }
  | regexp Tor regexp
        { Alternative($1,$3) }
  | regexp regexp %prec CONCAT
        { Sequence($1,$2) }
  | Tlparen regexp Trparen
        { $2 }
  | Tident
        { try
            Hashtbl.find named_regexps $1
          with Not_found ->
            let p = Parsing.symbol_start_pos () in
            Printf.eprintf "File \"%s\", line %d, character %d:\n\
                             Reference to unbound regexp name `%s'.\n"
                           p.Lexing.pos_fname p.Lexing.pos_lnum
                           (p.Lexing.pos_cnum - p.Lexing.pos_bol)
                           $1;
            exit 2 }
  | regexp Tas ident
        {let p1 = Parsing.rhs_start_pos 3
         and p2 = Parsing.rhs_end_pos 3 in
         let p = {
           loc_file = p1.Lexing.pos_fname ;
           start_pos = p1.Lexing.pos_cnum ;
           end_pos = p2.Lexing.pos_cnum ;
           start_line = p1.Lexing.pos_lnum ;
           start_col = p1.Lexing.pos_cnum - p1.Lexing.pos_bol ; } in
         Bind ($1, ($3, p))}
;

ident:
  Tident {$1}
;

char_class:
    Tcaret char_class1
        { Cset.complement $2 }
  | char_class1
        { $1 }
;
char_class1:
    Tchar Tdash Tchar
        { Cset.interval $1 $3 }
  | Tchar
        { Cset.singleton $1 }
  | char_class1 char_class1 %prec CONCAT
        { Cset.union $1 $2 }
;

%%
