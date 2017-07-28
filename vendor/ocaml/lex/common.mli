(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Damien Doligez, projet Moscova, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type line_tracker;;
val open_tracker : string -> out_channel -> line_tracker
val close_tracker : line_tracker -> unit
val copy_chunk :
  in_channel -> out_channel -> line_tracker -> Syntax.location -> bool -> unit
val output_mem_access : out_channel -> int -> unit
val output_memory_actions :
  string -> out_channel -> Lexgen.memory_action list -> unit
val output_env :
    in_channel -> out_channel -> line_tracker ->
      (Lexgen.ident * Lexgen.ident_info) list -> unit
val output_args : out_channel -> string list -> unit
val output_refill_handler :
  in_channel -> out_channel -> line_tracker -> Syntax.location option -> bool

val quiet_mode : bool ref;;
