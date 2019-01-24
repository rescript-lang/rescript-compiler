(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Specific operations for the PowerPC processor *)

open Format

let ppc64 =
  match Config.model with
  | "ppc" -> false
  | "ppc64" | "ppc64le" -> true
  | _ -> assert false

type abi = ELF32 | ELF64v1 | ELF64v2

let abi =
  match Config.model with
  | "ppc" -> ELF32
  | "ppc64" -> ELF64v1
  | "ppc64le" -> ELF64v2
  | _ -> assert false

(* Machine-specific command-line options *)

let big_toc = ref false

let command_line_options = [
  "-flarge-toc", Arg.Set big_toc,
     " Support TOC (table of contents) greater than 64 kbytes"
]

(* Specific operations *)

type specific_operation =
    Imultaddf                           (* multiply and add *)
  | Imultsubf                           (* multiply and subtract *)
  | Ialloc_far of                       (* allocation in large functions *)
      { words : int; label_after_call_gc : int (*Cmm.label*) option; }

(* note: we avoid introducing a dependency to Cmm since this dep
   is not detected when "make depend" is run under amd64 *)

let spacetime_node_hole_pointer_is_live_before = function
  | Imultaddf | Imultsubf -> false
  | Ialloc_far _ -> true

(* Addressing modes *)

type addressing_mode =
    Ibased of string * int              (* symbol + displ *)
  | Iindexed of int                     (* reg + displ *)
  | Iindexed2                           (* reg + reg *)

(* Sizes, endianness *)

let big_endian =
  match Config.model with
  | "ppc" -> true
  | "ppc64" -> true
  | "ppc64le" -> false
  | _ -> assert false

let size_addr = if ppc64 then 8 else 4
let size_int = size_addr
let size_float = 8

let allow_unaligned_access = true

(* Behavior of division *)

let division_crashes_on_overflow = true

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
    Ibased(s, n) -> Ibased(s, n + delta)
  | Iindexed n -> Iindexed(n + delta)
  | Iindexed2 -> assert false

let num_args_addressing = function
    Ibased _ -> 0
  | Iindexed _ -> 1
  | Iindexed2 -> 2

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Ibased(s, n) ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "\"%s\"%s" s idx
  | Iindexed n ->
      let idx = if n <> 0 then Printf.sprintf " + %i" n else "" in
      fprintf ppf "%a%s" printreg arg.(0) idx
  | Iindexed2 ->
      fprintf ppf "%a + %a" printreg arg.(0) printreg arg.(1)

let print_specific_operation printreg op ppf arg =
  match op with
  | Imultaddf ->
      fprintf ppf "%a *f %a +f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Imultsubf ->
      fprintf ppf "%a *f %a -f %a"
        printreg arg.(0) printreg arg.(1) printreg arg.(2)
  | Ialloc_far { words; _ } ->
      fprintf ppf "alloc_far %d" words
