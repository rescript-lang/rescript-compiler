(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

exception
  InvalidCodepoint of int
      [@ocaml.doc
        " This is a module provides the minimal Sedlexing suppport\n  It is mostly a subset of Sedlexing with two functions for performance reasons:\n  - Utf8.lexeme_to_buffer\n  - Utf8.lexeme_to_buffer2\n"]

exception MalFormed

type apos = int

type lexbuf

val lexbuf_clone : lexbuf -> lexbuf

val from_int_array : int array -> lexbuf

val new_line : lexbuf -> unit

val __private__next_int : lexbuf -> int

val mark : lexbuf -> int -> unit

val start : lexbuf -> unit

val backtrack : lexbuf -> int

val rollback : lexbuf -> unit

val lexeme_start : lexbuf -> int

val lexeme_end : lexbuf -> int

val loc : lexbuf -> int * int

val lexeme_length : lexbuf -> int

val sub_lexeme : lexbuf -> int -> int -> int array

val lexeme : lexbuf -> int array

module Utf8 : sig
  val from_string : string -> lexbuf

  val sub_lexeme : lexbuf -> int -> int -> string

  val lexeme : lexbuf -> string

  val lexeme_to_buffer : lexbuf -> Buffer.t -> unit

  val lexeme_to_buffer2 : lexbuf -> Buffer.t -> Buffer.t -> unit
end
