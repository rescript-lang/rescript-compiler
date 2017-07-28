(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Nicolas Pouillard *)
(* My_std *)

(** Generic utility functions, and system-independent glue. *)

exception Exit_OK
exception Exit_usage of string
exception Exit_system_error of string
exception Exit_with_code of int
exception Exit_silently_with_code of int

module Outcome : Signatures.OUTCOME

val ksbprintf : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
val sbprintf : ('a, Format.formatter, unit, string) format4 -> 'a

module Set : sig
  module type OrderedTypePrintable = Signatures.OrderedTypePrintable
  module type S = Signatures.SET
  module Make (M : OrderedTypePrintable) : S with type elt = M.t
end

module List : Signatures.LIST

module String : Signatures.STRING

module Digest : sig
  type t = string
  val string : string -> t
  val substring : string -> int -> int -> t
  external channel : in_channel -> int -> t = "caml_md5_chan"
  val file : string -> t
  val output : out_channel -> t -> unit
  val input : in_channel -> t
  val to_hex : t -> string
end

module StringSet : Set.S with type elt = String.t

val sys_readdir : string -> (string array, exn) Outcome.t
val sys_remove : string -> unit
val reset_readdir_cache : unit -> unit
val reset_filesys_cache : unit -> unit
val reset_filesys_cache_for_file : string -> unit
val sys_file_exists : string -> bool
val sys_command : string -> int
val filename_concat : string -> string -> string

val invalid_arg' : ('a, Format.formatter, unit, 'b) format4 -> 'a

include Signatures.MISC

val set_lexbuf_fname : string -> Lexing.lexbuf -> unit
val lexbuf_of_string : ?name:string -> string -> Lexing.lexbuf
