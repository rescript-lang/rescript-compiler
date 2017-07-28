(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Counts characters, lines and words in one or several files. *)

let chars = ref 0
and words = ref 0
and lines = ref 0

type state = Inside_word | Outside_word

let count_channel in_channel =
  let rec count status =
    let c = input_char in_channel in
    incr chars;
    match c with
      '\n' ->
        incr lines; count Outside_word
    | ' ' | '\t' ->
        count Outside_word
    | _ ->
        if status = Outside_word then begin incr words; () end;
        count Inside_word
  in
    try
      count Outside_word
    with End_of_file ->
      ()

let count_file name =
  let ic = open_in name in
  count_channel ic;
  close_in ic

let print_result () =
  print_int !chars; print_string " characters, ";
  print_int !words; print_string " words, ";
  print_int !lines; print_string " lines";
  print_newline()

let count name =
  count_file name;
  print_result ()

let _ =
try
  if Array.length Sys.argv <= 1 then
    count_channel stdin                (* No command-line arguments *)
  else
    for i = 1 to Array.length Sys.argv - 1 do
      count_file  Sys.argv.(i)
    done;
  print_result ()
with Sys_error s ->
  print_string "I/O error: ";
  print_string s;
  print_newline()
