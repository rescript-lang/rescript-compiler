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

(* Common functions for emitting assembly code *)

val output_channel: out_channel ref
val emit_string: string -> unit
val emit_int: int -> unit
val emit_nativeint: nativeint -> unit
val emit_int32: int32 -> unit
val emit_symbol: char -> string -> unit
val emit_printf: ('a, out_channel, unit) format -> 'a
val emit_char: char -> unit
val emit_string_literal: string -> unit
val emit_string_directive: string -> string -> unit
val emit_bytes_directive: string -> string -> unit
val emit_float64_directive: string -> int64 -> unit
val emit_float64_split_directive: string -> int64 -> unit
val emit_float32_directive: string -> int32 -> unit

val reset : unit -> unit
val reset_debug_info: unit -> unit
val emit_debug_info: Debuginfo.t -> unit

type frame_descr =
  { fd_lbl: int;                        (* Return address *)
    fd_frame_size: int;                 (* Size of stack frame *)
    fd_live_offset: int list;           (* Offsets/regs of live addresses *)
    fd_debuginfo: Debuginfo.t }         (* Location, if any *)

val frame_descriptors : frame_descr list ref

type emit_frame_actions =
  { efa_label: int -> unit;
    efa_16: int -> unit;
    efa_32: int32 -> unit;
    efa_word: int -> unit;
    efa_align: int -> unit;
    efa_label_rel: int -> int32 -> unit;
    efa_def_label: int -> unit;
    efa_string: string -> unit }

val emit_frames: emit_frame_actions -> unit

val is_generic_function: string -> bool

val cfi_startproc : unit -> unit
val cfi_endproc : unit -> unit
val cfi_adjust_cfa_offset : int -> unit
val cfi_offset : reg:int -> offset:int -> unit
