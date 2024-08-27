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

/* Character operations */

external code: char => int = "%identity"
external unsafe_chr: int => char = "%identity"

let chr = n =>
  if n < 0 || n > 255 {
    invalid_arg("Char.chr")
  } else {
    unsafe_chr(n)
  }

let lowercase = c =>
  if (c >= 'A' && c <= 'Z') || ((c >= 'À' && c <= 'Ö') || c >= 'Ø' && c <= 'Þ') {
    unsafe_chr(code(c) + 32)
  } else {
    c
  }

let uppercase = c =>
  if (c >= 'a' && c <= 'z') || ((c >= 'à' && c <= 'ö') || c >= 'ø' && c <= 'þ') {
    unsafe_chr(code(c) - 32)
  } else {
    c
  }

let lowercase_ascii = c =>
  if c >= 'A' && c <= 'Z' {
    unsafe_chr(code(c) + 32)
  } else {
    c
  }

let uppercase_ascii = c =>
  if c >= 'a' && c <= 'z' {
    unsafe_chr(code(c) - 32)
  } else {
    c
  }

type t = char

let compare = (c1, c2) => code(c1) - code(c2)
let equal = (c1: t, c2: t) => compare(c1, c2) == 0
