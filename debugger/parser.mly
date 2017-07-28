/***********************************************************************/
/*                                                                     */
/*                                OCaml                                */
/*                                                                     */
/*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        */
/*          OCaml port by John Malecki and Xavier Leroy                */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the Q Public License version 1.0.               */
/*                                                                     */
/***********************************************************************/

%{

open Int64ops
open Input_handling
open Longident
open Parser_aux

%}

%token <string> ARGUMENT
%token <string> LIDENT
%token <string> UIDENT
%token <string> OPERATOR
%token <int64>  INTEGER
%token          STAR                    /* *  */
%token          MINUS                   /* -  */
%token          DOT                     /* . */
%token          SHARP                   /* #  */
%token          AT                      /* @  */
%token          DOLLAR                  /* $ */
%token          BANG                    /* ! */
%token          LPAREN                  /* (  */
%token          RPAREN                  /* )  */
%token          LBRACKET                /* [  */
%token          RBRACKET                /* ]  */
%token          EOL

%right DOT
%right BANG

%start argument_list_eol
%type <string list> argument_list_eol

%start argument_eol
%type <string> argument_eol

%start integer_list_eol
%type <int list> integer_list_eol

%start integer_eol
%type <int> integer_eol

%start int64_eol
%type <int64> int64_eol

%start integer
%type <int> integer

%start opt_integer_eol
%type <int option> opt_integer_eol

%start opt_signed_integer_eol
%type <int option> opt_signed_integer_eol

%start opt_signed_int64_eol
%type <int64 option> opt_signed_int64_eol

%start identifier
%type <string> identifier

%start identifier_eol
%type <string> identifier_eol

%start identifier_or_eol
%type <string option> identifier_or_eol

%start opt_identifier
%type <string option> opt_identifier

%start opt_identifier_eol
%type <string option> opt_identifier_eol

%start expression_list_eol
%type <Parser_aux.expression list> expression_list_eol

%start break_argument_eol
%type <Parser_aux.break_arg> break_argument_eol

%start list_arguments_eol
%type <Longident.t option * int option * int option> list_arguments_eol

%start end_of_line
%type <unit> end_of_line

%start longident_eol
%type <Longident.t> longident_eol

%start opt_longident
%type <Longident.t option> opt_longident

%start opt_longident_eol
%type <Longident.t option> opt_longident_eol

%%

/* Raw arguments */

argument_list_eol :
    ARGUMENT argument_list_eol
      { $1::$2 }
  | end_of_line
      { [] };

argument_eol :
    ARGUMENT end_of_line
      { $1 };

/* Integer */

integer_list_eol :
    INTEGER integer_list_eol
      { (to_int $1) :: $2 }
  | end_of_line
      { [] };

integer_eol :
    INTEGER end_of_line
      { to_int $1 };

int64_eol :
    INTEGER end_of_line
      { $1 };

integer :
    INTEGER
      { to_int $1 };

opt_integer_eol :
    INTEGER end_of_line
      { Some (to_int $1) }
  | end_of_line
      { None };

opt_int64_eol :
    INTEGER end_of_line
      { Some $1 }
  | end_of_line
      { None };

opt_signed_integer_eol :
    MINUS integer_eol
      { Some (- $2) }
  | opt_integer_eol
      { $1 };

opt_signed_int64_eol :
    MINUS int64_eol
      { Some (Int64.neg $2) }
  | opt_int64_eol
      { $1 };

/* Identifiers and long identifiers */

longident :
    LIDENT                      { Lident $1 }
  | module_path DOT LIDENT      { Ldot($1, $3) }
  | OPERATOR                    { Lident $1 }
  | module_path DOT OPERATOR    { Ldot($1, $3) }
  | module_path DOT LPAREN OPERATOR RPAREN { Ldot($1, $4) }
;

module_path :
    UIDENT                      { Lident $1 }
  | module_path DOT UIDENT      { Ldot($1, $3) }
;

longident_eol :
    longident end_of_line       { $1 };

opt_longident :
    UIDENT                      { Some (Lident $1) }
  | LIDENT                      { Some (Lident $1) }
  | module_path DOT UIDENT      { Some (Ldot($1, $3)) }
  |                             { None };

opt_longident_eol :
    opt_longident end_of_line   { $1 };

identifier :
    LIDENT                      { $1 }
  | UIDENT                      { $1 };

identifier_eol :
    identifier end_of_line      { $1 };

identifier_or_eol :
    identifier                  { Some $1 }
  | end_of_line                 { None };

opt_identifier :
    identifier                  { Some $1 }
  |                             { None };

opt_identifier_eol :
    opt_identifier end_of_line  { $1 };

/* Expressions */

expression:
    longident                                  { E_ident $1 }
  | STAR                                        { E_result }
  | DOLLAR INTEGER                              { E_name (to_int $2) }
  | expression DOT INTEGER                      { E_item($1, (to_int $3)) }
  | expression DOT LBRACKET INTEGER RBRACKET    { E_item($1, (to_int $4)) }
  | expression DOT LPAREN INTEGER RPAREN        { E_item($1, (to_int $4)) }
  | expression DOT LIDENT                       { E_field($1, $3) }
  | BANG expression                             { E_field($2, "contents") }
  | LPAREN expression RPAREN                    { $2 }
;

/* Lists of expressions */

expression_list_eol :
    expression expression_list_eol              { $1::$2 }
  | end_of_line                                 { [] }
;

/* Arguments for breakpoint */

break_argument_eol :
    end_of_line                                 { BA_none }
  | integer_eol                                 { BA_pc $1 }
  | expression end_of_line                      { BA_function $1 }
  | AT opt_longident INTEGER opt_integer_eol    { BA_pos1 ($2, (to_int $3), $4)}
  | AT opt_longident SHARP integer_eol          { BA_pos2 ($2, $4) }
;

/* Arguments for list */

list_arguments_eol :
    opt_longident integer opt_integer_eol
      { ($1, Some $2, $3) }
  | opt_longident_eol
      { ($1, None, None) };

/* End of line */

end_of_line :
    EOL { stop_user_input () }
;
