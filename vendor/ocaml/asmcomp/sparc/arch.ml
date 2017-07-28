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

(* Specific operations for the Sparc processor *)

open Format

(* SPARC V8 adds multiply and divide.
   SPARC V9 adds double precision float operations, conditional
   move, and more instructions that are only useful in 64 bit mode.
   Sun calls 32 bit V9 "V8+". *)
type arch_version = SPARC_V7 | SPARC_V8 | SPARC_V9

let arch_version = ref SPARC_V7

let command_line_options =
  [ "-march=v8", Arg.Unit (fun () -> arch_version := SPARC_V8),
        " Generate code for SPARC V8 processors";
    "-march=v9", Arg.Unit (fun () -> arch_version := SPARC_V9),
        " Generate code for SPARC V9 processors" ]

type specific_operation = unit          (* None worth mentioning *)

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)

(* Sizes, endianness *)

let big_endian = true

let size_addr = 4
let size_int = 4
let size_float = 8

let allow_unaligned_access = false

(* Behavior of division *)

let division_crashes_on_overflow = false

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)

let num_args_addressing = function
    Ibased(s, n) -> 0
  | Iindexed n -> 1

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "\"%s\"%s" s idx
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx

let print_specific_operation printreg op ppf arg =
  Misc.fatal_error "Arch_sparc.print_specific_operation"
