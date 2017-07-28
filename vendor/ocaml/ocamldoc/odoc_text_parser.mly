%{
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

open Odoc_types

let identchar =
  "[A-Z a-z_\192-\214\216-\246\248-\255'0-9]"
let blank = "[ \010\013\009\012]"

let remove_beginning_blanks s =
  Str.global_replace (Str.regexp ("^"^blank^"+")) "" s

let remove_trailing_blanks s =
  Str.global_replace (Str.regexp (blank^"+$")) "" s

let print_DEBUG s = print_string s; print_newline ()
%}

%token END
%token <int * string option> Title
%token BOLD
%token EMP
%token CENTER
%token LEFT
%token RIGHT
%token ITALIC
%token <string> CUSTOM
%token LIST
%token ENUM
%token ITEM
%token LINK
%token CODE
%token END_CODE
%token CODE_PRE
%token END_CODE_PRE
%token VERB
%token END_VERB
%token LATEX
%token <string> Target
%token END_TARGET
%token LBRACE

%token ELE_REF
%token VAL_REF
%token TYP_REF
%token EXT_REF
%token EXC_REF
%token MOD_REF
%token MODT_REF
%token CLA_REF
%token CLT_REF
%token ATT_REF
%token MET_REF
%token SEC_REF
%token RECF_REF
%token CONST_REF
%token MOD_LIST_REF
%token INDEX_LIST

%token SUPERSCRIPT
%token SUBSCRIPT

%token BEGIN_SHORTCUT_LIST_ITEM
%token BEGIN_SHORTCUT_ENUM_ITEM
%token SHORTCUT_LIST_ITEM
%token SHORTCUT_ENUM_ITEM
%token END_SHORTCUT_LIST

%token BLANK_LINE

%token EOF
%token <string> Char

/* Start Symbols */
%start main located_element_list
%type <Odoc_types.text> main
%type <(int * int * Odoc_types.text_element) list> located_element_list

%%
main:
  text EOF { $1 }
| EOF { [Raw ""] }
;

text:
  text_element_list { $1 }
;

text_element_list:
  text_element { [ $1 ] }
| text_element text_element_list { $1 :: $2 }
;

located_element_list:
  located_element { [ $1 ] }
| located_element located_element_list { $1 :: $2 }
;

located_element:
  text_element { Parsing.symbol_start (), Parsing.symbol_end (), $1}
;


ele_ref_kind:
  ELE_REF { None }
| VAL_REF { Some RK_value }
| TYP_REF { Some RK_type }
| EXT_REF { Some RK_extension }
| EXC_REF { Some RK_exception }
| MOD_REF { Some RK_module }
| MODT_REF { Some RK_module_type }
| CLA_REF { Some RK_class }
| CLT_REF { Some RK_class_type }
| ATT_REF { Some RK_attribute }
| MET_REF { Some RK_method }
| SEC_REF { Some (RK_section [])}
| RECF_REF { Some RK_recfield }
| CONST_REF { Some RK_const }
;

text_element:
  Title text END { let n, l_opt = $1 in Title (n, l_opt, $2) }
| BOLD text END { Bold $2 }
| ITALIC text END { Italic $2 }
| CUSTOM text END { Custom ($1, $2) }
| EMP text END { Emphasize $2 }
| SUPERSCRIPT text END { Superscript $2 }
| SUBSCRIPT text END { Subscript $2 }
| CENTER text END { Center $2 }
| LEFT text END { Left $2 }
| RIGHT text END { Right $2 }
| LIST list END { List $2 }
| ENUM list END { Enum $2 }
| CODE string END_CODE { Code $2 }
| CODE_PRE string END_CODE_PRE { CodePre $2 }
| ele_ref_kind string END  {
      let s2 = remove_beginning_blanks $2 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, $1, None)
     }
| LBRACE ele_ref_kind string END text END {
      let s2 = remove_beginning_blanks $3 in
      let s3 = remove_trailing_blanks s2 in
      Ref (s3, $2, Some $5)
    }

| MOD_LIST_REF string END {
      let s2 = remove_beginning_blanks $2 in
      let s3 = remove_trailing_blanks s2 in
      let l = Odoc_misc.split_with_blanks s3 in
      Module_list l
     }
| INDEX_LIST { Index_list }
| VERB string END_VERB { Verbatim $2 }
| LATEX string END_TARGET { Latex $2 }
| Target string END_TARGET { Target ($1, $2) }
| LINK string END text END { Link ($2, $4) }
| BLANK_LINE { Newline }
| BEGIN_SHORTCUT_LIST_ITEM shortcut_list END_SHORTCUT_LIST { List $2 }
| BEGIN_SHORTCUT_LIST_ITEM shortcut_list EOF { List $2 }
| BEGIN_SHORTCUT_ENUM_ITEM shortcut_enum END_SHORTCUT_LIST { Enum $2 }
| BEGIN_SHORTCUT_ENUM_ITEM shortcut_enum EOF { Enum $2 }
| string { Raw $1 }
;

list:
| string { [] (* A VOIR : un test pour voir qu'il n'y a que des blancs *) }
| string list { $2 }
| list string  { $1 }
| item { [ $1 ] }
| item list { $1 :: $2 }

;

item:
    ITEM text END { $2 }
;

shortcut_list:
    text shortcut_list2  { $1 :: $2 }
| text { [ $1 ] }
;

shortcut_list2:
| SHORTCUT_LIST_ITEM shortcut_list { $2 }
;

shortcut_enum:
    text shortcut_enum2  { $1 :: $2 }
| text { [ $1 ] }
;

shortcut_enum2:
| SHORTCUT_ENUM_ITEM shortcut_enum { $2 }
;


string:
    Char { $1 }
| Char string { $1^$2 }
;

%%
