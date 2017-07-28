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
open Gram_aux
%}

%token <string> Tident
%token <char> Tchar
%token <string> Tstring
%token <Syntax.location> Taction
%token Trule Tparse Tand Tequal Tend Tor Tunderscore Teof Tlbracket Trbracket
%token Tstar Tmaybe Tplus Tlparen Trparen Tcaret Tdash

%left Tor
%left CONCAT
%nonassoc Tmaybe
%left Tstar
%left Tplus

%start lexer_definition
%type <Syntax.lexer_definition> lexer_definition

%%

lexer_definition:
    header Trule definition other_definitions Tend
        { Lexdef($1, $3::(List.rev $4)) }
;
header:
    Taction
        { $1 }
  |
        { Location(0,0) }
;
other_definitions:
    other_definitions Tand definition
        { $3::$1 }
  |
        { [] }
;
definition:
    Tident Tequal entry
        { ($1,$3) }
;
entry:
    Tparse case rest_of_entry
        { $2 :: List.rev $3 }
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
        { Characters all_chars }
  | Teof
        { Characters ['\000'] }
  | Tchar
        { Characters [$1] }
  | Tstring
        { regexp_for_string $1 }
  | Tlbracket char_class Trbracket
        { Characters $2 }
  | regexp Tstar
        { Repetition $1 }
  | regexp Tmaybe
        { Alternative($1, Epsilon) }
  | regexp Tplus
        { Sequence($1, Repetition $1) }
  | regexp Tor regexp
        { Alternative($1,$3) }
  | regexp regexp %prec CONCAT
        { Sequence($1,$2) }
  | Tlparen regexp Trparen
        { $2 }
;
char_class:
    Tcaret char_class1
        { subtract all_chars $2 }
  | char_class1
        { $1 }
;
char_class1:
    Tchar Tdash Tchar
        { char_class $1 $3 }
  | Tchar
        { [$1] }
  | char_class char_class %prec CONCAT
        { $1 @ $2 }
;

%%
