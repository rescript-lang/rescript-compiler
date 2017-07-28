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

(** The lexer for string to build text structures. *)

open Lexing
open Odoc_text_parser

let line_number = ref 0
let char_number = ref 0

let string_buffer = Buffer.create 32

(** Fonction de remise a zero de la chaine de caracteres tampon *)
let reset_string_buffer () = Buffer.reset string_buffer

(** Fonction d'ajout d'un caractere dans la chaine de caracteres tampon *)
let ajout_char_string = Buffer.add_char string_buffer

(** Add a string to the buffer. *)
let ajout_string = Buffer.add_string string_buffer

let lecture_string () = Buffer.contents string_buffer


(** the variable which will contain the description string.
   Is initialized when we encounter the start of a special comment. *)

let description = ref ""

let blank = "[ \013\009\012]"


let print_DEBUG s = print_string s; print_newline ()

(** this flag indicates whether we're in a string between begin_code and end_code tokens, to
   remember the number of open '[' and handle ']' correctly. *)
let open_brackets = ref 0

(** this flag indicates if we're in verbatim mode or not, to handle any special expression
   like a string when we're in verbatim mode.*)
let verb_mode = ref false

(** this flag indicates if we're in "target format" mode or not, to handle any special expression
   like a string when we're in this mode.*)
let target_mode = ref false

(** this flag indicates if we're in shortcut list mode or not, to handle end_shortcut_list correctly.*)
let shortcut_list_mode = ref false

(** this flag indicates if we're in an element reference. *)
let ele_ref_mode = ref false

(** this flag indicates if we're in a preformatted code string. *)
let code_pre_mode = ref false

let init () =
  open_brackets := 0;
  verb_mode := false;
  target_mode := false;
  shortcut_list_mode := false;
  ele_ref_mode := false ;
  code_pre_mode := false ;
  line_number := 0 ;
  char_number := 0

let incr_cpts lexbuf =
  let s = Lexing.lexeme lexbuf in
  let l = Str.split_delim (Str.regexp_string "\n") s in
  match List.rev l with
    [] -> () (* should not occur *)
  | [s2] -> (* no newline *)
      char_number := !char_number + (String.length s2)
  | s2 :: _ ->
      line_number := !line_number + ((List.length l) - 1) ;
      char_number := String.length s2

}

(** html marks, to use as alternative possible special strings *)

let html_bold = "<"('b'|'B')">"
let html_end_bold = "</"('b'|'B')">"
let html_italic = "<"('i'|'I')">"
let html_end_italic = "</"('i'|'I')">"
let html_title = "<"('h'|'H')(['0'-'9'])+">"
let html_end_title = "</"('h'|'H')(['0'-'9'])+">"
let html_list = "<"('u'|'U')('l'|'L')">"
let html_end_list = "</"('u'|'U')('l'|'L')">"
let html_enum = "<"('o'|'O')('l'|'L')">"
let html_end_enum = "</"('o'|'O')('l'|'L')">"
let html_item = "<"('l'|'L')('i'|'I')">"
let html_end_item = "</"('l'|'L')('i'|'I')">"
let html_code = "<"('c'|'C')('o'|'O')('d'|'D')('e'|'E')">"
let html_end_code = "</"('c'|'C')('o'|'O')('d'|'D')('e'|'E')">"
let html_center = "<"('c'|'C')('e'|'E')('n'|'N')('t'|'T')('e'|'E')('r'|'R')">"
let html_end_center = "</"('c'|'C')('e'|'E')('n'|'N')('t'|'T')('e'|'E')('r'|'R')">"
let html_left = "<"('l'|'L')('e'|'E')('f'|'F')('t'|'T')">"
let html_end_left = "</"('l'|'L')('e'|'E')('f'|'F')('t'|'T')">"
let html_right = "<"('r'|'R')('i'|'I')('g'|'G')('h'|'H')('t'|'T')">"
let html_end_right = "</"('r'|'R')('i'|'I')('g'|'G')('h'|'H')('t'|'T')">"


let blank = [' ' '\013' '\009' '\012']
let blank_nl = [' ' '\013' '\009' '\012' '\010']
let label = ['a'-'z']+['a'-'z' 'A'-'Z' '0'-'9' '_']*

(** special strings *)

let end = "}"
  | html_end_bold
  | html_end_italic
  | html_end_title
  | html_end_list
  | html_end_enum
  | html_end_item
  | html_end_center
let begin_title =
  ("{" ['0'-'9']+(":"label)? blank_nl)
  | html_title

let begin_bold = "{b"blank_nl | html_bold
let begin_emp = "{e"blank_nl
let begin_center = "{C"blank_nl | html_center
let begin_left = "{L"blank_nl
let begin_right = "{R"blank_nl
let begin_italic = "{i"blank_nl | html_italic
let begin_list = "{ul"blank_nl? | html_list
let begin_enum = "{ol"blank_nl? | html_enum
let begin_item = "{li"blank_nl | "{- " | html_item
let begin_link = "{{:"
let begin_target = "{%"['a'-'z''A'-'Z''0'-'9''-''_']+":"blank_nl?
let begin_latex = "{%"blank_nl
let end_target = "%}"
let begin_code = "[" | html_code
let end_code = "]" | html_end_code
let begin_code_pre = "{["
let end_code_pre = "]}"
let begin_verb = "{v"blank_nl
let end_verb = blank_nl"v}"

let begin_ele_ref = "{!"blank_nl | "{!"
let begin_val_ref = "{!val:"blank_nl | "{!val:"
let begin_typ_ref = "{!type:"blank_nl | "{!type:"
let begin_ext_ref = "{!extension:"blank_nl | "{!extension:"
let begin_exc_ref = "{!exception:"blank_nl | "{!exception:"
let begin_mod_ref = "{!module:"blank_nl | "{!module:"
let begin_modt_ref = "{!modtype:"blank_nl | "{!modtype:"
let begin_cla_ref = "{!class:"blank_nl | "{!class:"
let begin_clt_ref = "{!classtype:"blank_nl | "{!classtype:"
let begin_att_ref = "{!attribute:"blank_nl | "{!attribute:"
let begin_met_ref = "{!method:"blank_nl | "{!method:"
let begin_sec_ref = "{!section:"blank_nl | "{!section:"
let begin_recf_ref = "{!recfield:"blank_nl | "{!recfield:"
let begin_const_ref = "{!const:"blank_nl | "{!const:"
let begin_mod_list_ref = "{!modules:"blank_nl | "{!modules:"
let index_list = "{!indexlist}"
let begin_custom = "{"['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']*
let begin_superscript = "{^"blank_nl | "{^"
let begin_subscript = "{_"blank_nl | "{_"

let shortcut_list_item = '\n'blank*"- "
let shortcut_enum_item = '\n'blank*"+ "
let end_shortcut_list = '\n'(blank*'\n')+

rule main = parse
| "\\{"
| "\\}"
| "\\["
| "\\]"
    {
      incr_cpts lexbuf ;
      let s = Lexing.lexeme lexbuf in
      Char (String.sub s 1 1)
    }

| end
    {
      print_DEBUG "end";
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) then
        Char (Lexing.lexeme lexbuf)
      else
        let _ =
          if !ele_ref_mode then
            ele_ref_mode := false
        in
        END
    }
| begin_title
    {
      print_DEBUG "begin_title";
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        let s = Lexing.lexeme lexbuf in
        try
          (* chech if the "{..." or html_title mark was used. *)
          if s.[0] = '<' then
            let (n, l) = (2, (String.length s - 3)) in
            let s2 = String.sub s n l in
            Title (int_of_string s2, None)
          else
            let (n, l) = (1, (String.length s - 2)) in
            let s2 = String.sub s n l in
            try
              let i = String.index s2 ':' in
              let s_n = String.sub s2 0 i in
              let s_label = String.sub s2 (i+1) (l-i-1) in
              Title (int_of_string s_n, Some s_label)
            with
              Not_found ->
                Title (int_of_string s2, None)
        with
          _ ->
            Title (1, None)
    }
| begin_bold
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        BOLD
    }
| begin_italic
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        ITALIC
    }
| begin_link
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        LINK
    }
| begin_emp
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        EMP
    }
| begin_superscript
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        SUPERSCRIPT
    }
| begin_subscript
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        SUBSCRIPT
    }
| begin_center
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        CENTER
    }
| begin_left
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        LEFT
    }
| begin_right
     {
      incr_cpts lexbuf ;
       if !verb_mode || !target_mode || !code_pre_mode
           || (!open_brackets >= 1) || !ele_ref_mode then
         Char (Lexing.lexeme lexbuf)
       else
         RIGHT
     }
| begin_list
    {
      print_DEBUG "LIST";
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        LIST
    }
| begin_enum
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        ENUM
    }
| begin_item
    {
      print_DEBUG "ITEM";
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        ITEM
    }
| begin_target
   {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        (
         let s = Lexing.lexeme lexbuf in
         let fmt =
           let p1 = String.index s '%' in
           let p2 = String.index s ':' in
           String.sub s (p1 + 1) (p2 - p1 - 1)
         in
         target_mode := true;
         Target fmt
        )
    }
| begin_latex
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        (
         target_mode := true;
         LATEX
        )
    }
| end_target
    {
      incr_cpts lexbuf ;
      if !verb_mode || (!open_brackets >= 1) || !code_pre_mode ||
        !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        (
         target_mode := false;
         END_TARGET
        )
    }
| begin_code end_code
    {
      incr_cpts lexbuf ;
      Char (Lexing.lexeme lexbuf)
    }

| begin_code
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        if !open_brackets <= 0 then
          (
           open_brackets := 1;
           CODE
          )
        else
          (
           incr open_brackets;
           Char (Lexing.lexeme lexbuf)
          )
    }
| end_code
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        if !open_brackets > 1 then
          (
           decr open_brackets;
           Char "]"
          )
        else
          (
           open_brackets := 0;
           END_CODE
          )
    }

| begin_code_pre end_code_pre
    {
      incr_cpts lexbuf ;
      Char (Lexing.lexeme lexbuf)
    }

| begin_code_pre
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        (
         code_pre_mode := true;
         CODE_PRE
        )
    }
| end_code_pre
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        if !open_brackets >= 1 then
          (
           lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
           lexbuf.Lexing.lex_curr_p <-
             { lexbuf.Lexing.lex_curr_p with
               pos_cnum = lexbuf.Lexing.lex_curr_p.pos_cnum - 1
             } ;
           decr char_number ;
           if !open_brackets > 1 then
             (
              decr open_brackets;
              Char "]"
             )
           else
             (
              open_brackets := 0;
              END_CODE
             )
          )
        else
          if !code_pre_mode then
            (
             code_pre_mode := false;
             END_CODE_PRE
            )
          else
            Char (Lexing.lexeme lexbuf)
    }

| begin_ele_ref end
    {
      incr_cpts lexbuf ;
      Char (Lexing.lexeme lexbuf)
    }

| begin_ele_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           ELE_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }


| begin_val_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           VAL_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_typ_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           TYP_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_ext_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           EXT_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_exc_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           EXC_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_mod_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           MOD_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_modt_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           MODT_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_cla_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           CLA_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_clt_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           CLT_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_att_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           ATT_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_met_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           MET_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| begin_sec_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           SEC_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }
| begin_recf_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           RECF_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }
| begin_const_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           CONST_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }
| begin_mod_list_ref
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          (
           ele_ref_mode := true;
           MOD_LIST_REF
          )
        else
          (
           Char (Lexing.lexeme lexbuf)
          )
    }

| index_list
    {
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode || !open_brackets >= 1 then
        Char (Lexing.lexeme lexbuf)
      else
        if not !ele_ref_mode then
          INDEX_LIST
        else
          Char (Lexing.lexeme lexbuf)
    }

| begin_verb
    {
      incr_cpts lexbuf ;
      if !target_mode || (!open_brackets >= 1) || !code_pre_mode || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        (
         verb_mode := true;
         VERB
        )
    }
| end_verb
    {
      incr_cpts lexbuf ;
      if !target_mode || (!open_brackets >= 1) || !code_pre_mode || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        (
         verb_mode := false;
         END_VERB
        )
    }

| shortcut_list_item
    {
      incr_cpts lexbuf ;
      if !target_mode || (!open_brackets >= 1) || !code_pre_mode
          || !ele_ref_mode || !verb_mode then
        Char (Lexing.lexeme lexbuf)
      else if !shortcut_list_mode then
        (
         SHORTCUT_LIST_ITEM
        )
      else
        (
         shortcut_list_mode := true;
         BEGIN_SHORTCUT_LIST_ITEM
        )
    }

| shortcut_enum_item
    {
      incr_cpts lexbuf ;
      if !target_mode || (!open_brackets >= 1) || !code_pre_mode
         || !ele_ref_mode || !verb_mode then
        Char (Lexing.lexeme lexbuf)
      else if !shortcut_list_mode then
        SHORTCUT_ENUM_ITEM
      else
        (
         shortcut_list_mode := true;
         BEGIN_SHORTCUT_ENUM_ITEM
        )
    }
| end_shortcut_list
    {
      incr_cpts lexbuf ;
      lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
      lexbuf.Lexing.lex_curr_p <-
        { lexbuf.Lexing.lex_curr_p with
          pos_cnum = lexbuf.Lexing.lex_curr_p.pos_cnum - 1 ;
        } ;
      decr line_number ;
      if !shortcut_list_mode then
        (
         shortcut_list_mode := false;
                        (* go back one char to re-use the last '\n', so we can
                           restart another shortcut-list with a single blank line,
                           and not two.*)
         END_SHORTCUT_LIST
        )
      else
        if !target_mode || (!open_brackets >= 1) || !code_pre_mode || !ele_ref_mode || !verb_mode then
          Char (Lexing.lexeme lexbuf)
        else
          BLANK_LINE
    }

| eof           { EOF }

| begin_custom
    {
      print_DEBUG "begin_custom";
      incr_cpts lexbuf ;
      if !verb_mode || !target_mode || !code_pre_mode ||
        (!open_brackets >= 1) || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        let s = Lexing.lexeme lexbuf in
        let len = String.length s in
        (* remove this starting '{' *)
        let tag = Odoc_misc.no_blanks (String.sub s 1 (len - 1)) in
        CUSTOM tag
    }

|  "{"
    {
      incr_cpts lexbuf ;
      if !target_mode || (!open_brackets >= 1) || !code_pre_mode || !ele_ref_mode then
        Char (Lexing.lexeme lexbuf)
      else
        LBRACE
    }
| _
    {
      incr_cpts lexbuf ;
      Char (Lexing.lexeme lexbuf)
    }
