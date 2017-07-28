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

(* From C-- to assembly code *)

open Format
open Cmm

let dump_cmm = ref false
let dump_selection = ref false
let dump_live = ref false
let dump_spill = ref false
let dump_split = ref false
let dump_interf = ref false
let dump_prefer = ref false
let dump_regalloc = ref false
let dump_reload = ref false
let dump_linear = ref false

let rec regalloc fd =
  if !dump_live then Printmach.phase "Liveness analysis" fd;
  Interf.build_graph fd;
  if !dump_interf then Printmach.interferences();
  if !dump_prefer then Printmach.preferences();
  Coloring.allocate_registers();
  if !dump_regalloc then
    Printmach.phase "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  if !dump_reload then
    Printmach.phase "After insertion of reloading code" newfd;
  if redo_regalloc
  then begin Reg.reinit(); Liveness.fundecl newfd; regalloc newfd end
  else newfd

let fundecl ppf fd_cmm =
  if !dump_cmm then begin
    fprintf ppf "*** C-- code@.";
    fprintf ppf "%a@." Printcmm.fundecl fd_cmm
  end;
  Reg.reset();
  let fd_sel = Sequence.fundecl fd_cmm in
  if !dump_selection then
    Printmach.phase "After instruction selection" fd_sel;
  Liveness.fundecl fd_sel;
  if !dump_live then Printmach.phase "Liveness analysis" fd_sel;
  let fd_spill = Spill.fundecl fd_sel in
  Liveness.fundecl fd_spill;
  if !dump_spill then
    Printmach.phase "After spilling" fd_spill;
  let fd_split = Split.fundecl fd_spill in
  Liveness.fundecl fd_split;
  if !dump_split then
    Printmach.phase "After live range splitting" fd_split;
  let fd_reload = regalloc fd_split in
  let fd_linear = Linearize.fundecl fd_reload in
  if !dump_linear then begin
    printf "*** Linearized code@.";
    Printlinear.fundecl fd_linear; print_newline()
  end;
  Emit.fundecl fd_linear

let phrase = function
    Cfunction fd -> fundecl fd
  | Cdata dl -> Emit.data dl

let file filename =
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  try
    while true do
      phrase(Parsecmm.phrase Lexcmm.token lb)
    done
  with
      End_of_file ->
        close_in ic
    | Lexcmm.Error msg ->
        close_in ic; Lexcmm.report_error lb msg
    | Parsing.Parse_error ->
        close_in ic;
        prerr_string "Syntax error near character ";
        prerr_int (Lexing.lexeme_start lb);
        prerr_newline()
    | Parsecmmaux.Error msg ->
        close_in ic; Parsecmmaux.report_error msg
    | x ->
        close_in ic; raise x
