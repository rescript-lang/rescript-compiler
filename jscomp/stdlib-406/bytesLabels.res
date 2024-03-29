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

/* Byte sequence operations */

/* WARNING: Some functions in this file are duplicated in string.ml for
   efficiency reasons. When you modify the one in this file you need to
   modify its duplicate in string.ml.
   These functions have a "duplicated" comment above their definition.
*/
external \".!()": (string, int) => char = "%string_unsafe_get"
@val
external of_small_int_array: (@as(json`null`) _, array<int>) => string = "String.fromCharCode.apply"

external length: bytes => int = "%bytes_length"
%%private(external string_length: string => int = "%string_length")
external get: (bytes, int) => char = "%bytes_safe_get"
external set: (bytes, int, char) => unit = "%bytes_safe_set"
external create: int => bytes = "?create_bytes"

external unsafe_get: (bytes, int) => char = "%bytes_unsafe_get"
external unsafe_set: (bytes, int, char) => unit = "%bytes_unsafe_set"
external \".![]": (bytes, int) => char = "%bytes_unsafe_get"
external \".![]<-": (bytes, int, char) => unit = "%bytes_unsafe_set"
@new external new_uninitialized: int => bytes = "Array"
external to_int_array: bytes => array<int> = "%identity"

let unsafe_fill: (bytes, int, int, char) => unit = (s: bytes, i, l, c: char) =>
  if l > 0 {
    for k in i to l + i - 1 {
      \".![]<-"(s, k, c)
    }
  }

@ocaml.doc(" Same as {!Array.prototype.copyWithin} ")
let copyWithin = (s1: bytes, i1, i2, len) =>
  if i1 < i2 {
    /* nop for i1 = i2 */
    let range_a = length(s1) - i2 - 1
    let range_b = len - 1
    let range = if range_a > range_b {
      range_b
    } else {
      range_a
    }
    for j in range downto 0 {
      \".![]<-"(s1, i2 + j, \".![]"(s1, i1 + j))
    }
  } else if i1 > i2 {
    let range_a = length(s1) - i1 - 1
    let range_b = len - 1
    let range = if range_a > range_b {
      range_b
    } else {
      range_a
    }
    for k in 0 to range {
      \".![]<-"(s1, i2 + k, \".![]"(s1, i1 + k))
    }
  }

/* TODO: when the compiler could optimize small function calls, 
   use high order functions instead
*/
let unsafe_blit = (s1: bytes, i1, s2: bytes, i2, len) =>
  if len > 0 {
    if s1 === s2 {
      copyWithin(s1, i1, i2, len)
    } else {
      let off1 = length(s1) - i1
      if len <= off1 {
        for i in 0 to len - 1 {
          \".![]<-"(s2, i2 + i, \".![]"(s1, i1 + i))
        }
      } else {
        for i in 0 to off1 - 1 {
          \".![]<-"(s2, i2 + i, \".![]"(s1, i1 + i))
        }
        for i in off1 to len - 1 {
          \".![]<-"(s2, i2 + i, ' ')
        }
      }
    }
  }

let unsafe_blit_string = (s1: string, i1, s2: bytes, i2, len: int) =>
  if len > 0 {
    let off1 = string_length(s1) - i1
    if len <= off1 {
      for i in 0 to len - 1 {
        \".![]<-"(s2, i2 + i, \".!()"(s1, i1 + i))
      }
    } else {
      for i in 0 to off1 - 1 {
        \".![]<-"(s2, i2 + i, \".!()"(s1, i1 + i))
      }
      for i in off1 to len - 1 {
        \".![]<-"(s2, i2 + i, ' ')
      }
    }
  }
let string_of_large_bytes = (bytes: bytes, i, len) => {
  let s = ref("")
  let s_len = ref(len)
  let seg = 1024
  if i == 0 && (len <= 4 * seg && len == length(bytes)) {
    of_small_int_array(to_int_array(bytes))
  } else {
    let offset = ref(0)
    while s_len.contents > 0 {
      let next = if s_len.contents < 1024 {
        s_len.contents
      } else {
        seg
      }
      let tmp_bytes = new_uninitialized(next)
      for k in 0 to next - 1 {
        \".![]<-"(tmp_bytes, k, \".![]"(bytes, k + offset.contents))
      }
      s.contents = s.contents ++ of_small_int_array(to_int_array(tmp_bytes))
      s_len.contents = s_len.contents - next
      offset.contents = offset.contents + next
    }
    s.contents
  }
}

let make = (n, c) => {
  let s = create(n)
  unsafe_fill(s, 0, n, c)
  s
}

let init = (n, ~f) => {
  let s = create(n)
  for i in 0 to n - 1 {
    unsafe_set(s, i, f(i))
  }
  s
}

let empty = create(0)

let copy = s => {
  let len = length(s)
  let r = create(len)
  unsafe_blit(s, 0, r, 0, len)
  r
}

let to_string = (a: bytes): string => string_of_large_bytes(a, 0, length(a))

let unsafe_to_string = to_string

@ocaml.doc(" checkout [Bytes.empty] -- to be inlined? ")
let of_string = (s: string) => {
  let len = string_length(s)
  let res = new_uninitialized(len)
  for i in 0 to len - 1 {
    \".![]<-"(res, i, \".!()"(s, i))
    /* Note that when get a char and convert it to int immedately, should be optimized
       should be [s.charCodeAt[i]]
 */
  }
  res
}

let unsafe_of_string = of_string

let sub = (s, ~pos as ofs, ~len) =>
  if ofs < 0 || (len < 0 || ofs > length(s) - len) {
    invalid_arg("String.sub / Bytes.sub")
  } else {
    let r = create(len)
    unsafe_blit(s, ofs, r, 0, len)
    r
  }

let sub_string = (b, ~pos as ofs, ~len) => unsafe_to_string(sub(b, ~pos=ofs, ~len))

/* addition with an overflow check */
let \"++" = (a, b) => {
  let c = a + b
  switch (a < 0, b < 0, c < 0) {
  | (true, true, false)
  | (false, false, true) =>
    invalid_arg("Bytes.extend") /* overflow */
  | _ => c
  }
}

let extend = (s, ~left, ~right) => {
  let len = \"++"(\"++"(length(s), left), right)
  let r = create(len)
  let (srcoff, dstoff) = if left < 0 {
    (-left, 0)
  } else {
    (0, left)
  }
  let cpylen = min(length(s) - srcoff, len - dstoff)
  if cpylen > 0 {
    unsafe_blit(s, srcoff, r, dstoff, cpylen)
  }
  r
}

let fill = (s, ~pos as ofs, ~len, c) =>
  if ofs < 0 || (len < 0 || ofs > length(s) - len) {
    invalid_arg("String.fill / Bytes.fill")
  } else {
    unsafe_fill(s, ofs, len, c)
  }

let blit = (~src as s1, ~src_pos as ofs1, ~dst as s2, ~dst_pos as ofs2, ~len) =>
  if len < 0 || (ofs1 < 0 || (ofs1 > length(s1) - len || (ofs2 < 0 || ofs2 > length(s2) - len))) {
    invalid_arg("Bytes.blit")
  } else {
    unsafe_blit(s1, ofs1, s2, ofs2, len)
  }

let blit_string = (~src as s1, ~src_pos as ofs1, ~dst as s2, ~dst_pos as ofs2, ~len) =>
  if (
    len < 0 ||
      (ofs1 < 0 ||
      (ofs1 > string_length(s1) - len || (ofs2 < 0 || ofs2 > length(s2) - len)))
  ) {
    invalid_arg("String.blit / Bytes.blit_string")
  } else {
    unsafe_blit_string(s1, ofs1, s2, ofs2, len)
  }

/* duplicated in string.ml */
let iter = (~f, a) =>
  for i in 0 to length(a) - 1 {
    f(unsafe_get(a, i))
  }

/* duplicated in string.ml */
let iteri = (~f, a) =>
  for i in 0 to length(a) - 1 {
    f(i, unsafe_get(a, i))
  }

let ensure_ge = (x: int, y) =>
  if x >= y {
    x
  } else {
    invalid_arg("Bytes.concat")
  }

let rec sum_lengths = (acc, seplen, param) =>
  switch param {
  | list{} => acc
  | list{hd} => length(hd) + acc
  | list{hd, ...tl} => sum_lengths(ensure_ge(length(hd) + seplen + acc, acc), seplen, tl)
  }

let rec unsafe_blits = (dst, pos, sep, seplen, param) =>
  switch param {
  | list{} => dst
  | list{hd} =>
    unsafe_blit(hd, 0, dst, pos, length(hd))
    dst
  | list{hd, ...tl} =>
    unsafe_blit(hd, 0, dst, pos, length(hd))
    unsafe_blit(sep, 0, dst, pos + length(hd), seplen)
    unsafe_blits(dst, pos + length(hd) + seplen, sep, seplen, tl)
  }

let concat = (~sep, param) =>
  switch param {
  | list{} => empty
  | l =>
    let seplen = length(sep)
    unsafe_blits(create(sum_lengths(0, seplen, l)), 0, sep, seplen, l)
  }

let cat = (s1, s2) => {
  let l1 = length(s1)
  let l2 = length(s2)
  let r = create(l1 + l2)
  unsafe_blit(s1, 0, r, 0, l1)
  unsafe_blit(s2, 0, r, l1, l2)
  r
}

external char_chr: int => char = "%identity"

let is_space = param =>
  switch param {
  | ' ' | '' | '\n' | '\r' | '\t' => true
  | _ => false
  }

let trim = s => {
  let len = length(s)
  let i = ref(0)
  while i.contents < len && is_space(unsafe_get(s, i.contents)) {
    incr(i)
  }
  let j = ref(len - 1)
  while j.contents >= i.contents && is_space(unsafe_get(s, j.contents)) {
    decr(j)
  }
  if j.contents >= i.contents {
    sub(s, ~pos=i.contents, ~len=j.contents - i.contents + 1)
  } else {
    empty
  }
}

let escaped = s => {
  let n = ref(0)
  for i in 0 to length(s) - 1 {
    n :=
      n.contents +
      switch unsafe_get(s, i) {
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' => 2
      | ' ' .. '~' => 1
      | _ => 4
      }
  }
  if n.contents == length(s) {
    copy(s)
  } else {
    let s' = create(n.contents)
    n := 0
    for i in 0 to length(s) - 1 {
      switch unsafe_get(s, i) {
      | ('"' | '\\') as c =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, c)
      | '\n' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 'n')
      | '\t' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 't')
      | '\r' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 'r')
      | '\b' =>
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, 'b')
      | ' ' .. '~' as c => unsafe_set(s', n.contents, c)
      | c =>
        let a = (c :> int)
        unsafe_set(s', n.contents, '\\')
        incr(n)
        unsafe_set(s', n.contents, char_chr(48 + a / 100))
        incr(n)
        unsafe_set(s', n.contents, char_chr(48 + mod(a / 10, 10)))
        incr(n)
        unsafe_set(s', n.contents, char_chr(48 + mod(a, 10)))
      }
      incr(n)
    }
    s'
  }
}

let map = (~f, s) => {
  let l = length(s)
  if l == 0 {
    s
  } else {
    let r = create(l)
    for i in 0 to l - 1 {
      unsafe_set(r, i, f(unsafe_get(s, i)))
    }
    r
  }
}

let mapi = (~f, s) => {
  let l = length(s)
  if l == 0 {
    s
  } else {
    let r = create(l)
    for i in 0 to l - 1 {
      unsafe_set(r, i, f(i, unsafe_get(s, i)))
    }
    r
  }
}

let uppercase_ascii = s => map(~f=Char.uppercase_ascii, s)
let lowercase_ascii = s => map(~f=Char.lowercase_ascii, s)

let apply1 = (f, s) =>
  if length(s) == 0 {
    s
  } else {
    let r = copy(s)
    unsafe_set(r, 0, f(unsafe_get(s, 0)))
    r
  }

let capitalize_ascii = s => apply1(Char.uppercase_ascii, s)
let uncapitalize_ascii = s => apply1(Char.lowercase_ascii, s)

/* duplicated in string.ml */
let rec index_rec = (s, lim, i, c) =>
  if i >= lim {
    raise(Not_found)
  } else if unsafe_get(s, i) == c {
    i
  } else {
    index_rec(s, lim, i + 1, c)
  }

/* duplicated in string.ml */
let index = (s, c) => index_rec(s, length(s), 0, c)

/* duplicated in string.ml */
let rec index_rec_opt = (s, lim, i, c) =>
  if i >= lim {
    None
  } else if unsafe_get(s, i) == c {
    Some(i)
  } else {
    index_rec_opt(s, lim, i + 1, c)
  }

/* duplicated in string.ml */
let index_opt = (s, c) => index_rec_opt(s, length(s), 0, c)

/* duplicated in string.ml */
let index_from = (s, i, c) => {
  let l = length(s)
  if i < 0 || i > l {
    invalid_arg("String.index_from / Bytes.index_from")
  } else {
    index_rec(s, l, i, c)
  }
}

/* duplicated in string.ml */
let index_from_opt = (s, i, c) => {
  let l = length(s)
  if i < 0 || i > l {
    invalid_arg("String.index_from_opt / Bytes.index_from_opt")
  } else {
    index_rec_opt(s, l, i, c)
  }
}

/* duplicated in string.ml */
let rec rindex_rec = (s, i, c) =>
  if i < 0 {
    raise(Not_found)
  } else if unsafe_get(s, i) == c {
    i
  } else {
    rindex_rec(s, i - 1, c)
  }

/* duplicated in string.ml */
let rindex = (s, c) => rindex_rec(s, length(s) - 1, c)

/* duplicated in string.ml */
let rindex_from = (s, i, c) =>
  if i < -1 || i >= length(s) {
    invalid_arg("String.rindex_from / Bytes.rindex_from")
  } else {
    rindex_rec(s, i, c)
  }

/* duplicated in string.ml */
let rec rindex_rec_opt = (s, i, c) =>
  if i < 0 {
    None
  } else if unsafe_get(s, i) == c {
    Some(i)
  } else {
    rindex_rec_opt(s, i - 1, c)
  }

/* duplicated in string.ml */
let rindex_opt = (s, c) => rindex_rec_opt(s, length(s) - 1, c)

/* duplicated in string.ml */
let rindex_from_opt = (s, i, c) =>
  if i < -1 || i >= length(s) {
    invalid_arg("String.rindex_from_opt / Bytes.rindex_from_opt")
  } else {
    rindex_rec_opt(s, i, c)
  }

/* duplicated in string.ml */
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

/* duplicated in string.ml */
let contains = (s, c) => contains_from(s, 0, c)

/* duplicated in string.ml */
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

type t = bytes

let compare = (x: t, y: t) => Pervasives.compare(x, y)
let equal = (x: t, y: t) => x == y
