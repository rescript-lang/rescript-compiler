/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Damien Doligez, projet Gallium, INRIA Rocquencourt */
/*  */
/* Copyright 2014 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* String operations, based on byte sequence operations */

/* WARNING: Some functions in this file are duplicated in bytes.ml for
   efficiency reasons. When you modify the one in this file you need to
   modify its duplicate in bytes.ml.
   These functions have a "duplicated" comment above their definition.
*/

external length: string => int = "%string_length"
external get: (string, int) => char = "%string_safe_get"
external unsafe_get: (string, int) => char = "%string_unsafe_get"

module B = Bytes

let bts = B.unsafe_to_string
let bos = B.unsafe_of_string

external make: (int, char) => string = "?string_repeat"

let init = (n, ~f) => bts(B.init(n, f))
let sub = (s, ~pos as ofs, ~len) => bts(B.sub(bos(s), ofs, len))
let blit = (~src, ~src_pos, ~dst, ~dst_pos, ~len) => B.blit_string(src, src_pos, dst, dst_pos, len)

%%private(@send external join: (array<string>, string) => string = "join")

let concat = (~sep: string, xs: list<string>) => xs->Array.of_list->join(sep)

/* duplicated in bytes.ml */
let iter = (~f, s) =>
  for i in 0 to length(s) - 1 {
    f(unsafe_get(s, i))
  }

/* duplicated in bytes.ml */
let iteri = (~f, s) =>
  for i in 0 to length(s) - 1 {
    f(i, unsafe_get(s, i))
  }

let map = (~f, s) => bts(B.map(f, bos(s)))
let mapi = (~f, s) => bts(B.mapi(f, bos(s)))

/* Beware: we cannot use B.trim or B.escape because they always make a
   copy, but String.mli spells out some cases where we are not allowed
   to make a copy. */

let is_space = param =>
  switch param {
  | ' ' | '' | '\n' | '\r' | '\t' => true
  | _ => false
  }

let trim = s =>
  if s == "" {
    s
  } else if is_space(unsafe_get(s, 0)) || is_space(unsafe_get(s, length(s) - 1)) {
    bts(B.trim(bos(s)))
  } else {
    s
  }

let escaped = s => {
  let rec needs_escape = i =>
    if i >= length(s) {
      false
    } else {
      switch unsafe_get(s, i) {
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' => true
      | ' ' .. '~' => needs_escape(i + 1)
      | _ => true
      }
    }

  if needs_escape(0) {
    bts(B.escaped(bos(s)))
  } else {
    s
  }
}

/* duplicated in bytes.ml */
let rec index_rec = (s, lim, i, c) =>
  if i >= lim {
    raise(Not_found)
  } else if unsafe_get(s, i) == c {
    i
  } else {
    index_rec(s, lim, i + 1, c)
  }

/* duplicated in bytes.ml */
let index = (s, c) => index_rec(s, length(s), 0, c)

/* duplicated in bytes.ml */
let rec index_rec_opt = (s, lim, i, c) =>
  if i >= lim {
    None
  } else if unsafe_get(s, i) == c {
    Some(i)
  } else {
    index_rec_opt(s, lim, i + 1, c)
  }

/* duplicated in bytes.ml */
let index_opt = (s, c) => index_rec_opt(s, length(s), 0, c)

/* duplicated in bytes.ml */
let index_from = (s, i, c) => {
  let l = length(s)
  if i < 0 || i > l {
    invalid_arg("String.index_from / Bytes.index_from")
  } else {
    index_rec(s, l, i, c)
  }
}

/* duplicated in bytes.ml */
let index_from_opt = (s, i, c) => {
  let l = length(s)
  if i < 0 || i > l {
    invalid_arg("String.index_from_opt / Bytes.index_from_opt")
  } else {
    index_rec_opt(s, l, i, c)
  }
}

/* duplicated in bytes.ml */
let rec rindex_rec = (s, i, c) =>
  if i < 0 {
    raise(Not_found)
  } else if unsafe_get(s, i) == c {
    i
  } else {
    rindex_rec(s, i - 1, c)
  }

/* duplicated in bytes.ml */
let rindex = (s, c) => rindex_rec(s, length(s) - 1, c)

/* duplicated in bytes.ml */
let rindex_from = (s, i, c) =>
  if i < -1 || i >= length(s) {
    invalid_arg("String.rindex_from / Bytes.rindex_from")
  } else {
    rindex_rec(s, i, c)
  }

/* duplicated in bytes.ml */
let rec rindex_rec_opt = (s, i, c) =>
  if i < 0 {
    None
  } else if unsafe_get(s, i) == c {
    Some(i)
  } else {
    rindex_rec_opt(s, i - 1, c)
  }

/* duplicated in bytes.ml */
let rindex_opt = (s, c) => rindex_rec_opt(s, length(s) - 1, c)

/* duplicated in bytes.ml */
let rindex_from_opt = (s, i, c) =>
  if i < -1 || i >= length(s) {
    invalid_arg("String.rindex_from_opt / Bytes.rindex_from_opt")
  } else {
    rindex_rec_opt(s, i, c)
  }

/* duplicated in bytes.ml */
let contains_from = (s, i, c) => {
  let l = length(s)
  if i < 0 || i > l {
    invalid_arg("String.contains_from / Bytes.contains_from")
  } else {
    try {
      ignore(index_rec(s, l, i, c))
      true
    } catch {
    | Not_found => false
    }
  }
}

/* duplicated in bytes.ml */
let contains = (s, c) => contains_from(s, 0, c)

/* duplicated in bytes.ml */
let rcontains_from = (s, i, c) =>
  if i < 0 || i >= length(s) {
    invalid_arg("String.rcontains_from / Bytes.rcontains_from")
  } else {
    try {
      ignore(rindex_rec(s, i, c))
      true
    } catch {
    | Not_found => false
    }
  }

let uppercase_ascii = s => bts(B.uppercase_ascii(bos(s)))
let lowercase_ascii = s => bts(B.lowercase_ascii(bos(s)))
let capitalize_ascii = s => bts(B.capitalize_ascii(bos(s)))
let uncapitalize_ascii = s => bts(B.uncapitalize_ascii(bos(s)))

type t = string

let compare = (x: t, y: t) => Pervasives.compare(x, y)
let equal: (string, string) => bool = (a, b) => a == b

let split_on_char = (~sep, s) => {
  let r = ref(list{})
  let j = ref(length(s))
  for i in length(s) - 1 downto 0 {
    if unsafe_get(s, i) == sep {
      r := list{sub(s, ~pos=i + 1, ~len=j.contents - i - 1), ...r.contents}
      j := i
    }
  }
  list{sub(s, ~pos=0, ~len=j.contents), ...r.contents}
}
