(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type extern_flags =
    No_sharing
  | Closures
  | Compat_32

external to_bytes: 'a -> extern_flags list -> bytes
    = "caml_output_value_to_string"

external to_string: 'a -> extern_flags list -> string
    = "caml_output_value_to_string"

let to_channel chan v flags =
  output_string chan (to_string v flags)

external to_buffer_unsafe:
      bytes -> int -> int -> 'a -> extern_flags list -> int
    = "caml_output_value_to_buffer"

let to_buffer buff ofs len v flags =
  if ofs < 0 || len < 0 || ofs + len > Bytes.length buff
  then invalid_arg "Marshal.to_buffer: substring out of bounds"
  else to_buffer_unsafe buff ofs len v flags

external from_channel: in_channel -> 'a = "caml_input_value"
external from_bytes_unsafe: bytes -> int -> 'a
         = "caml_input_value_from_string"
external data_size_unsafe: bytes -> int -> int = "caml_marshal_data_size"

let header_size = 20
let data_size buff ofs =
  if ofs < 0 || ofs > Bytes.length buff - header_size
  then invalid_arg "Marshal.data_size"
  else data_size_unsafe buff ofs
let total_size buff ofs = header_size + data_size buff ofs

let from_bytes buff ofs =
  if ofs < 0 || ofs > Bytes.length buff - header_size
  then invalid_arg "Marshal.from_bytes"
  else begin
    let len = data_size_unsafe buff ofs in
    if ofs > Bytes.length buff - (header_size + len)
    then invalid_arg "Marshal.from_bytes"
    else from_bytes_unsafe buff ofs
  end

let from_string buff ofs = from_bytes (Bytes.unsafe_of_string buff) ofs
