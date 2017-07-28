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

(* Compaction of an automata *)
type lex_tables =
  { tbl_base: int array;                 (* Perform / Shift *)
    tbl_backtrk: int array;              (* No_remember / Remember *)
    tbl_default: int array;              (* Default transition *)
    tbl_trans: int array;                (* Transitions (compacted) *)
    tbl_check: int array;                (* Check (compacted) *)
(* code addresses are managed in a similar fashion as transitions *)
    tbl_base_code : int array;           (* code ptr / base for Shift *)
    tbl_backtrk_code : int array;        (* nothing / code when Remember *)
(* moves to execute before transitions (compacted) *)
    tbl_default_code : int array;
    tbl_trans_code : int array;
    tbl_check_code : int array;
(* byte code itself *)
    tbl_code: int array;}


val compact_tables: Lexgen.automata array -> lex_tables
