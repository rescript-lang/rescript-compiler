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

type t<'a> = {mutable c: list<'a>, mutable len: int}

exception Empty

let create = () => {c: list{}, len: 0}

let clear = s => {
  s.c = list{}
  s.len = 0
}

let copy = s => {c: s.c, len: s.len}

let push = (x, s) => {
  s.c = list{x, ...s.c}
  s.len = s.len + 1
}

let pop = s =>
  switch s.c {
  | list{hd, ...tl} =>
    s.c = tl
    s.len = s.len - 1
    hd
  | list{} => raise(Empty)
  }

let top = s =>
  switch s.c {
  | list{hd, ..._} => hd
  | list{} => raise(Empty)
  }

let is_empty = s => s.c == list{}

let length = s => s.len

let iter = (f, s) => List.iter(f, s.c)

let fold = (f, acc, s) => List.fold_left(f, acc, s.c)
