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
open Odoc_comments_global

let uppercase = "[A-Z\192-\214\216-\222]"
let identchar =
  "[A-Za-z_\192-\214\216-\246\248-\255'0-9]"
let blank = "[ \010\013\009\012]"

let print_DEBUG s = print_string s; print_newline ()
%}

%token <string * (string option)> Description

%token <string> See_url
%token <string> See_file
%token <string> See_doc

%token T_PARAM
%token T_AUTHOR
%token T_VERSION
%token T_SEE
%token T_SINCE
%token T_BEFORE
%token T_DEPRECATED
%token T_RAISES
%token T_RETURN
%token <string> T_CUSTOM

%token EOF

%token <string> Desc

/* Start Symbols */
%start main info_part2 see_info
%type <(string * (string option)) option> main
%type <unit> info_part2
%type <Odoc_types.see_ref * string> see_info


%%
see_info:
  see_ref Desc { ($1, $2) }
;

see_ref:
    See_url { Odoc_types.See_url $1 }
| See_file { Odoc_types.See_file $1 }
| See_doc { Odoc_types.See_doc $1 }
;

main:
  Description { Some $1 }
| EOF { None }
;

info_part2:
  element_list EOF { () }
;

element_list:
  element { () }
| element element_list { () }
;

element:
| param { () }
| author { () }
| version { () }
| see { () }
| since { () }
| before { () }
| deprecated { () }
| raise_exc { () }
| return { () }
| custom { () }
;

param:
    T_PARAM Desc
    {
      (* isolate the identificator *)
      (* we only look for simple id, no pattern nor tuples *)
      let s = $2 in
      match Str.split (Str.regexp (blank^"+")) s with
        []
      | _ :: [] ->
          raise (Failure "usage: @param id description")
      | id :: _ ->
          print_DEBUG ("Identificator "^id);
          let reg = identchar^"+" in
          print_DEBUG ("reg="^reg);
          if Str.string_match (Str.regexp reg) id 0 then
            let remain = String.sub s (String.length id) ((String.length s) - (String.length id)) in
            print_DEBUG ("T_PARAM Desc remain="^remain);
            let remain2 = Str.replace_first (Str.regexp ("^"^blank^"+")) "" remain in
            params := !params @ [(id, remain2)]
          else
            raise (Failure (id^" is not a valid parameter identificator in \"@param "^s^"\""))
    }
;
author:
    T_AUTHOR Desc { authors := !authors @ [ $2 ] }
;
version:
    T_VERSION Desc { version := Some $2 }
;
see:
    T_SEE Desc { sees := !sees @ [$2] }
;
since:
    T_SINCE Desc { since := Some $2 }
;
before:
    T_BEFORE Desc
    {
      (* isolate the version name *)
      let s = $2 in
      match Str.split (Str.regexp (blank^"+")) s with
        []
      | _ :: [] ->
          raise (Failure "usage: @before version description")
      | id :: _ ->
          print_DEBUG ("version "^id);
            let remain = String.sub s (String.length id) ((String.length s) - (String.length id)) in
            let remain2 = Str.replace_first (Str.regexp ("^"^blank^"+")) "" remain in
            before := !before @ [(id, remain2)]
    }
;
deprecated:
    T_DEPRECATED Desc { deprecated := Some $2 }
;
raise_exc:
    T_RAISES Desc
    {
      (* isolate the exception construtor name *)
      let s = $2 in
      match Str.split (Str.regexp (blank^"+")) s with
        []
      | _ :: [] ->
          raise (Failure "usage: @raise Exception description")
      | id :: _ ->
          print_DEBUG ("exception "^id);
          let reg = uppercase^identchar^"*"^"\\(\\."^uppercase^identchar^"*\\)*" in
          print_DEBUG ("reg="^reg);
          if Str.string_match (Str.regexp reg) id 0 then
            let remain = String.sub s (String.length id) ((String.length s) - (String.length id)) in
            let remain2 = Str.replace_first (Str.regexp ("^"^blank^"+")) "" remain in
            raised_exceptions := !raised_exceptions @ [(id, remain2)]
          else
            raise (Failure (id^" is not a valid exception constructor in \"@raise "^s^"\""))
    }
;
return:
    T_RETURN Desc { return_value := Some $2 }
;

custom:
    T_CUSTOM Desc { customs := !customs @ [($1, $2)] }
;


%%
