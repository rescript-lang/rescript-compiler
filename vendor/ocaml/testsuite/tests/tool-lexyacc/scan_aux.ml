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

(* Auxiliaries for the lexical analyzer *)

let brace_depth = ref 0
let comment_depth = ref 0

exception Lexical_error of string

let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0


let store_string_char c =
  begin
    if !string_index >= String.length !string_buff then begin
      let new_buff = String.create (String.length !string_buff * 2) in
      String.blit new_buff 0 !string_buff 0 (String.length !string_buff);
      string_buff := new_buff
    end
  end;
  String.unsafe_set !string_buff !string_index c;
  incr string_index

let get_stored_string () =
  let s = String.sub !string_buff 0 !string_index in
  string_buff := initial_string_buffer;
  s


let char_for_backslash = function
    'n' -> '\010' (* '\n' when bootstrapped *)
  | 't' -> '\009' (* '\t' *)
  | 'b' -> '\008' (* '\b' *)
  | 'r' -> '\013' (* '\r' *)
  | c   -> c


let char_for_decimal_code lexbuf i =
  Char.chr(100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
            10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                 (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48))
