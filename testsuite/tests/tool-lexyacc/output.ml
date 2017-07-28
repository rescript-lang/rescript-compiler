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

(* Generating a DFA as a set of mutually recursive functions *)

open Syntax

let ic = ref stdin
let oc = ref stdout

(* 1- Generating the actions *)

let copy_buffer = String.create 1024

let copy_chunk (Location(start,stop)) =
  seek_in !ic start;
  let tocopy = ref(stop - start) in
  while !tocopy > 0 do
    let m =
      input !ic copy_buffer 0 (min !tocopy (String.length copy_buffer)) in
    output !oc copy_buffer 0 m;
    tocopy := !tocopy - m
  done


let output_action (i,act) =
  output_string !oc ("action_" ^ string_of_int i ^ " lexbuf = (\n");
  copy_chunk act;
  output_string !oc ")\nand "


(* 2- Generating the states *)

let states = ref ([||] : automata array)

type occurrence =
  { mutable pos: int list;
    mutable freq: int }

let enumerate_vect v =
  let env = ref [] in
  for pos = 0 to Array.length v - 1 do
    try
      let occ = List.assoc v.(pos) !env in
      occ.pos <- pos :: occ.pos;
      occ.freq <- occ.freq + 1
    with Not_found ->
      env := (v.(pos), {pos = [pos]; freq = 1 }) :: !env
  done;
  Sort.list (fun (e1, occ1) (e2, occ2) -> occ1.freq >= occ2.freq) !env


let output_move = function
    Backtrack ->
      output_string !oc "lexing.backtrack lexbuf"
  | Goto dest ->
      match !states.(dest) with
        Perform act_num ->
          output_string !oc ("action_" ^ string_of_int act_num ^ " lexbuf")
      | _ ->
          output_string !oc ("state_" ^ string_of_int dest ^ " lexbuf")


(* Cannot use standard char_for_read because the characters to escape
   are not the same in CL6 and CL1999. *)

let output_char_lit oc = function
    '\'' -> output_string oc "\\'"
  | '\\' -> output_string oc "\\\\"
  | '\n' -> output_string oc "\\n"
  | '\t' -> output_string oc "\\t"
  | c ->  if Char.code c >= 32 && Char.code c < 128 then
            output_char oc c
          else begin
            let n = Char.code c in
            output_char oc '\\';
            output_char oc (Char.chr (48 + n / 100));
            output_char oc (Char.chr (48 + (n / 10) mod 10));
            output_char oc (Char.chr (48 + n mod 10))
          end

let rec output_chars = function
    [] ->
      failwith "output_chars"
  | [c] ->
      output_string !oc "'";
      output_char_lit !oc (Char.chr c);
      output_string !oc "'"
  | c::cl ->
      output_string !oc "'";
      output_char_lit !oc (Char.chr c);
      output_string !oc "'|";
      output_chars cl

let output_one_trans (dest, occ) =
  output_chars occ.pos;
  output_string !oc " -> ";
  output_move dest;
  output_string !oc "\n |  "

let output_all_trans trans =
  output_string !oc "  match lexing.next_char lexbuf with\n    ";
  match enumerate_vect trans with
    [] ->
      failwith "output_all_trans"
  | (default, _) :: rest ->
      List.iter output_one_trans rest;
      output_string !oc "_ -> ";
      output_move default;
      output_string !oc "\nand "

let output_state state_num = function
    Perform i ->
      ()
  | Shift(what_to_do, moves) ->
      output_string !oc
        ("state_"  ^ string_of_int state_num ^ " lexbuf =\n");
      begin match what_to_do with
        No_remember -> ()
      | Remember i ->
          output_string !oc
            ("  Lexing.set_backtrack lexbuf action_" ^
             string_of_int i ^ ";\n")
      end;
      output_all_trans moves


(* 3- Generating the entry points *)

let rec output_entries = function
    [] -> failwith "output_entries"
  | (name,state_num) :: rest ->
      output_string !oc (name ^ " lexbuf =\n");
      output_string !oc "  Lexing.init lexbuf;\n";
      output_string !oc ("  state_" ^ string_of_int state_num ^
                        " lexbuf\n");
      match rest with
        [] -> ()
      | _  -> output_string !oc "\nand "; output_entries rest


(* All together *)

let output_lexdef header (initial_st, st, actions) =
  print_int (Array.length st); print_string " states, ";
  print_int (List.length actions); print_string " actions.";
  print_newline();
  copy_chunk header;
  output_string !oc "\nlet rec ";
  states := st;
  List.iter output_action actions;
  for i = 0 to Array.length st - 1 do
    output_state i st.(i)
  done;
  output_entries initial_st
