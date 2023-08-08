/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* Message digest (MD5) */

type t = string

let compare = String.compare
let equal = String.equal

external unsafe_string: (string, int, int) => t = "?md5_string"

let string = str => unsafe_string(str, 0, String.length(str))

let bytes = b => string(Bytes.unsafe_to_string(b))

let substring = (str, ofs, len) =>
  if ofs < 0 || (len < 0 || ofs > String.length(str) - len) {
    invalid_arg("Digest.substring")
  } else {
    unsafe_string(str, ofs, len)
  }

let subbytes = (b, ofs, len) => substring(Bytes.unsafe_to_string(b), ofs, len)

let char_hex = n =>
  Char.unsafe_chr(
    n + if n < 10 {
      Char.code('0')
    } else {
      Char.code('a') - 10
    },
  )

let to_hex = d => {
  if String.length(d) != 16 {
    invalid_arg("Digest.to_hex")
  }
  let result = Bytes.create(32)
  for i in 0 to 15 {
    let x = Char.code(String.get(d, i))
    Bytes.unsafe_set(result, i * 2, char_hex(lsr(x, 4)))
    Bytes.unsafe_set(result, i * 2 + 1, char_hex(land(x, 0x0f)))
  }
  Bytes.unsafe_to_string(result)
}

let from_hex = s => {
  if String.length(s) != 32 {
    invalid_arg("Digest.from_hex")
  }
  let digit = c =>
    switch c {
    | '0' .. '9' => Char.code(c) - Char.code('0')
    | 'A' .. 'F' => Char.code(c) - Char.code('A') + 10
    | 'a' .. 'f' => Char.code(c) - Char.code('a') + 10
    | _ => raise(Invalid_argument("Digest.from_hex"))
    }

  let byte = i => lsl(digit(String.get(s, i)), 4) + digit(String.get(s, i + 1))
  let result = Bytes.create(16)
  for i in 0 to 15 {
    Bytes.set(result, i, Char.chr(byte(2 * i)))
  }
  Bytes.unsafe_to_string(result)
}
