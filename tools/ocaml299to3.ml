(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*              Jacques Garrigue, Kyoto University RIMS                *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Lexer299

let input_buffer = Buffer.create 16383
let input_function ic buf len =
  let len = input ic buf 0 len in
  Buffer.add_substring input_buffer buf 0 len;
  len

let output_buffer = Buffer.create 16383

let modified = ref false

let convert buffer =
  let input_pos = ref 0 in
  let copy_input stop =
    Buffer.add_substring output_buffer (Buffer.contents input_buffer)
      !input_pos (stop - !input_pos);
    input_pos := stop
  in
  let last = ref (EOF, 0, 0) in
  try while true do
    let token = Lexer299.token buffer
    and start = Lexing.lexeme_start buffer
    and stop = Lexing.lexeme_end buffer
    and last_token, last_start, last_stop = !last in
    begin match token with
    | LABEL l0 ->
        let l = if l0 = "fun" then "f" else l0 in
        begin match last_token with
        | PREFIXOP "?(" ->
            modified := true;
            copy_input last_start;
            Buffer.add_char output_buffer '?';
            Buffer.add_string output_buffer l;
            Buffer.add_string output_buffer ":(";
            input_pos := stop
        | QUESTION | LPAREN | LBRACE | SEMI | MINUSGREATER
        | EQUAL | COLON | COLONGREATER
        | VAL | MUTABLE | EXTERNAL | METHOD | OF ->
            if l0 = "fun" then begin
              modified := true;
              copy_input start;
              Buffer.add_string output_buffer l;
              Buffer.add_char output_buffer ':';
              input_pos := stop
            end
        | _ ->
            modified := true;
            copy_input start;
            Buffer.add_char output_buffer '~';
            Buffer.add_string output_buffer l;
            Buffer.add_char output_buffer ':';
            input_pos := stop
        end
    | LABELID l ->
        modified := true;
        begin match last_token with
        | PREFIXOP "?(" ->
            copy_input last_start;
            Buffer.add_string output_buffer "?(";
            Buffer.add_string output_buffer l;
            input_pos := stop
        | LPAREN ->
            copy_input last_start;
            Buffer.add_string output_buffer "~(";
            Buffer.add_string output_buffer l;
            input_pos := stop
        | QUESTION ->
            copy_input last_stop;
            Buffer.add_string output_buffer l;
            input_pos := stop
        | _ ->
            copy_input start;
            Buffer.add_char output_buffer '~';
            Buffer.add_string output_buffer l;
            input_pos := stop
       end
    | EOF -> raise End_of_file
    | _ -> ()
    end;
    if last_token = QUESTION && token = LPAREN then
      last := (PREFIXOP "?(", last_start, stop)
    else
      last := (token, start, stop)
  done with
    End_of_file ->
      copy_input (Buffer.length input_buffer)

let convert_file name =
  let ic = open_in name in
  Buffer.clear input_buffer;
  Buffer.clear output_buffer;
  modified := false;
  begin
    try convert (Lexing.from_function (input_function ic)); close_in ic
    with exn -> close_in ic; raise exn
  end;
  if !modified then begin
    let backup = name ^ ".bak" in
    if Sys.file_exists backup then Sys.remove name
    else Sys.rename name backup;
    let oc = open_out name in
    Buffer.output_buffer oc output_buffer;
    close_out oc
  end

let _ =
  if Array.length Sys.argv < 2 || Sys.argv.(1) = "-h" || Sys.argv.(1) = "-help"
  then begin
    print_endline "Usage: ocaml299to3 <source file> ...";
    print_endline "Description:";
    print_endline
      "Convert OCaml 2.99 O'Labl-style labels in implementation files to";
    print_endline
      "a syntax compatible with version 3. Also `fun:' labels are replaced \
       by `f:'.";
    print_endline "Other syntactic changes are not handled.";
    print_endline "Old files are renamed to <file>.bak.";
    print_endline "Interface files do not need label syntax conversion.";
    exit 0
  end;
  for i = 1 to Array.length Sys.argv - 1 do
    let name = Sys.argv.(i) in
    prerr_endline ("Converting " ^ name);
    Printexc.catch convert_file name
  done
