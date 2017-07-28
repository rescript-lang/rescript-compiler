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

(* The shallow abstract syntax *)

type location =
    Location of int * int

type regular_expression =
    Epsilon
  | Characters of char list
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression

type lexer_definition =
    Lexdef of location * (string * (regular_expression * location) list) list

(* Representation of automata *)

type automata =
    Perform of int
  | Shift of automata_trans * automata_move array
and automata_trans =
    No_remember
  | Remember of int
and automata_move =
    Backtrack
  | Goto of int
