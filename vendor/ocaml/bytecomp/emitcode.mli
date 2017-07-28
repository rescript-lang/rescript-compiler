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

(* Generation of bytecode for .cmo files *)

open Cmo_format
open Instruct

val to_file: out_channel -> string -> string -> instruction list -> unit
        (* Arguments:
             channel on output file
             name of compilation unit implemented
             path of cmo file being written
             list of instructions to emit *)
val to_memory: instruction list -> instruction list ->
                    bytes * int * (reloc_info * int) list
        (* Arguments:
             initialization code (terminated by STOP)
             function code
           Results:
             block of relocatable bytecode
             size of this block
             relocation information *)
val to_packed_file:
  out_channel -> instruction list -> (reloc_info * int) list
        (* Arguments:
             channel on output file
             list of instructions to emit
           Result:
             relocation information (reversed) *)

val reset: unit -> unit
