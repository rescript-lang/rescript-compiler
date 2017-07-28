(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2000 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Handling of sections in bytecode executable files *)

(** Recording sections written to a bytecode executable file *)

val init_record: out_channel -> unit
    (* Start recording sections from the current position in out_channel *)

val record: out_channel -> string -> unit
    (* Record the current position in the out_channel as the end of
       the section with the given name *)

val write_toc_and_trailer: out_channel -> unit
    (* Write the table of contents and the standard trailer for bytecode
       executable files *)

(** Reading sections from a bytecode executable file *)

val read_toc: in_channel -> unit
    (* Read the table of sections from a bytecode executable *)

exception Bad_magic_number
    (* Raised by [read_toc] if magic number doesn't match *)

val toc: unit -> (string * int) list
    (* Return the current table of contents as a list of
       (section name, section length) pairs. *)

val seek_section: in_channel -> string -> int
    (* Position the input channel at the beginning of the section named "name",
       and return the length of that section.  Raise Not_found if no
       such section exists. *)

val read_section_string: in_channel -> string -> string
    (* Return the contents of a section, as a string *)

val read_section_struct: in_channel -> string -> 'a
    (* Return the contents of a section, as marshalled data *)

val pos_first_section: in_channel -> int
   (* Return the position of the beginning of the first section *)

val reset: unit -> unit
