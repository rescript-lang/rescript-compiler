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

(* Generation of bytecode for .cmo files *)

open Cmo_format
open Instruct

val to_file: out_channel -> string -> string ->
  required_globals:Ident.Set.t -> instruction list -> unit
        (* Arguments:
             channel on output file
             name of compilation unit implemented
             path of cmo file being written
             required_globals: list of compilation units that must be
               evaluated before this one
             list of instructions to emit *)
val to_memory: instruction list -> instruction list ->
                    bytes * int * (reloc_info * int) list * debug_event list
        (* Arguments:
             initialization code (terminated by STOP)
             function code
           Results:
             block of relocatable bytecode
             size of this block
             relocation information
             debug events *)
val to_packed_file:
  out_channel -> instruction list -> (reloc_info * int) list
        (* Arguments:
             channel on output file
             list of instructions to emit
           Result:
             relocation information (reversed) *)

val reset: unit -> unit

val marshal_to_channel_with_possibly_32bit_compat : filename:string -> kind:string -> out_channel -> 'a -> unit
