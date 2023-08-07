/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1999 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/* Extensible buffers */

type t = {
  mutable buffer: bytes,
  mutable position: int,
  mutable length: int,
  initial_buffer: bytes,
}

let create = n => {
  let n = if n < 1 {
    1
  } else {
    n
  }
  let s = Bytes.create(n)
  {buffer: s, position: 0, length: n, initial_buffer: s}
}

let contents = b => Bytes.sub_string(b.buffer, 0, b.position)
let to_bytes = b => Bytes.sub(b.buffer, 0, b.position)

let sub = (b, ofs, len) =>
  if ofs < 0 || (len < 0 || ofs > b.position - len) {
    invalid_arg("Buffer.sub")
  } else {
    Bytes.sub_string(b.buffer, ofs, len)
  }

let blit = (src, srcoff, dst, dstoff, len) =>
  if (
    len < 0 ||
      (srcoff < 0 ||
      (srcoff > src.position - len || (dstoff < 0 || dstoff > Bytes.length(dst) - len)))
  ) {
    invalid_arg("Buffer.blit")
  } else {
    Bytes.blit(src.buffer, srcoff, dst, dstoff, len)
  }

let nth = (b, ofs) =>
  if ofs < 0 || ofs >= b.position {
    invalid_arg("Buffer.nth")
  } else {
    Bytes.unsafe_get(b.buffer, ofs)
  }

let length = b => b.position

let clear = b => b.position = 0

let reset = b => {
  b.position = 0
  b.buffer = b.initial_buffer
  b.length = Bytes.length(b.buffer)
}

let resize = (b, more) => {
  let len = b.length
  let new_len = ref(len)
  while b.position + more > new_len.contents {
    new_len := 2 * new_len.contents
  }
  let new_buffer = Bytes.create(new_len.contents)
  /* PR#6148: let's keep using [blit] rather than [unsafe_blit] in
   this tricky function that is slow anyway. */
  Bytes.blit(b.buffer, 0, new_buffer, 0, b.position)
  b.buffer = new_buffer
  b.length = new_len.contents
}

let add_char = (b, c) => {
  let pos = b.position
  if pos >= b.length {
    resize(b, 1)
  }
  Bytes.unsafe_set(b.buffer, pos, c)
  b.position = pos + 1
}

let add_utf_8_uchar = (b, u) =>
  switch Uchar.to_int(u) {
  | u if u < 0 => assert(false)
  | u if u <= 0x007F => add_char(b, Char.unsafe_chr(u))
  | u if u <= 0x07FF =>
    let pos = b.position
    if pos + 2 > b.length {
      resize(b, 2)
    }
    Bytes.unsafe_set(b.buffer, pos, Char.unsafe_chr(lor(0xC0, lsr(u, 6))))
    Bytes.unsafe_set(b.buffer, pos + 1, Char.unsafe_chr(lor(0x80, land(u, 0x3F))))
    b.position = pos + 2
  | u if u <= 0xFFFF =>
    let pos = b.position
    if pos + 3 > b.length {
      resize(b, 3)
    }
    Bytes.unsafe_set(b.buffer, pos, Char.unsafe_chr(lor(0xE0, lsr(u, 12))))
    Bytes.unsafe_set(b.buffer, pos + 1, Char.unsafe_chr(lor(0x80, land(lsr(u, 6), 0x3F))))
    Bytes.unsafe_set(b.buffer, pos + 2, Char.unsafe_chr(lor(0x80, land(u, 0x3F))))
    b.position = pos + 3
  | u if u <= 0x10FFFF =>
    let pos = b.position
    if pos + 4 > b.length {
      resize(b, 4)
    }
    Bytes.unsafe_set(b.buffer, pos, Char.unsafe_chr(lor(0xF0, lsr(u, 18))))
    Bytes.unsafe_set(b.buffer, pos + 1, Char.unsafe_chr(lor(0x80, land(lsr(u, 12), 0x3F))))
    Bytes.unsafe_set(b.buffer, pos + 2, Char.unsafe_chr(lor(0x80, land(lsr(u, 6), 0x3F))))
    Bytes.unsafe_set(b.buffer, pos + 3, Char.unsafe_chr(lor(0x80, land(u, 0x3F))))
    b.position = pos + 4
  | _ => assert(false)
  }

let add_utf_16be_uchar = (b, u) =>
  switch Uchar.to_int(u) {
  | u if u < 0 => assert(false)
  | u if u <= 0xFFFF =>
    let pos = b.position
    if pos + 2 > b.length {
      resize(b, 2)
    }
    Bytes.unsafe_set(b.buffer, pos, Char.unsafe_chr(lsr(u, 8)))
    Bytes.unsafe_set(b.buffer, pos + 1, Char.unsafe_chr(land(u, 0xFF)))
    b.position = pos + 2
  | u if u <= 0x10FFFF =>
    let u' = u - 0x10000
    let hi = lor(0xD800, lsr(u', 10))
    let lo = lor(0xDC00, land(u', 0x3FF))
    let pos = b.position
    if pos + 4 > b.length {
      resize(b, 4)
    }
    Bytes.unsafe_set(b.buffer, pos, Char.unsafe_chr(lsr(hi, 8)))
    Bytes.unsafe_set(b.buffer, pos + 1, Char.unsafe_chr(land(hi, 0xFF)))
    Bytes.unsafe_set(b.buffer, pos + 2, Char.unsafe_chr(lsr(lo, 8)))
    Bytes.unsafe_set(b.buffer, pos + 3, Char.unsafe_chr(land(lo, 0xFF)))
    b.position = pos + 4
  | _ => assert(false)
  }

let add_utf_16le_uchar = (b, u) =>
  switch Uchar.to_int(u) {
  | u if u < 0 => assert(false)
  | u if u <= 0xFFFF =>
    let pos = b.position
    if pos + 2 > b.length {
      resize(b, 2)
    }
    Bytes.unsafe_set(b.buffer, pos, Char.unsafe_chr(land(u, 0xFF)))
    Bytes.unsafe_set(b.buffer, pos + 1, Char.unsafe_chr(lsr(u, 8)))
    b.position = pos + 2
  | u if u <= 0x10FFFF =>
    let u' = u - 0x10000
    let hi = lor(0xD800, lsr(u', 10))
    let lo = lor(0xDC00, land(u', 0x3FF))
    let pos = b.position
    if pos + 4 > b.length {
      resize(b, 4)
    }
    Bytes.unsafe_set(b.buffer, pos, Char.unsafe_chr(land(hi, 0xFF)))
    Bytes.unsafe_set(b.buffer, pos + 1, Char.unsafe_chr(lsr(hi, 8)))
    Bytes.unsafe_set(b.buffer, pos + 2, Char.unsafe_chr(land(lo, 0xFF)))
    Bytes.unsafe_set(b.buffer, pos + 3, Char.unsafe_chr(lsr(lo, 8)))
    b.position = pos + 4
  | _ => assert(false)
  }

let add_substring = (b, s, offset, len) => {
  if offset < 0 || (len < 0 || offset > String.length(s) - len) {
    invalid_arg("Buffer.add_substring/add_subbytes")
  }
  let new_position = b.position + len
  if new_position > b.length {
    resize(b, len)
  }
  Bytes.blit_string(s, offset, b.buffer, b.position, len)
  b.position = new_position
}

let add_subbytes = (b, s, offset, len) => add_substring(b, Bytes.unsafe_to_string(s), offset, len)

let add_string = (b, s) => {
  let len = String.length(s)
  let new_position = b.position + len
  if new_position > b.length {
    resize(b, len)
  }
  Bytes.blit_string(s, 0, b.buffer, b.position, len)
  b.position = new_position
}

let add_bytes = (b, s) => add_string(b, Bytes.unsafe_to_string(s))

let add_buffer = (b, bs) => add_subbytes(b, bs.buffer, 0, bs.position)

let closing = param =>
  switch param {
  | '(' => ')'
  | '{' => '}'
  | _ => assert(false)
  }

/* opening and closing: open and close characters, typically ( and )
   k: balance of opening and closing chars
   s: the string where we are searching
   start: the index where we start the search. */
let advance_to_closing = (opening, closing, k, s, start) => {
  let rec advance = (k, i, lim) =>
    if i >= lim {
      raise(Not_found)
    } else if String.get(s, i) == opening {
      advance(k + 1, i + 1, lim)
    } else if String.get(s, i) == closing {
      if k == 0 {
        i
      } else {
        advance(k - 1, i + 1, lim)
      }
    } else {
      advance(k, i + 1, lim)
    }
  advance(k, start, String.length(s))
}

let advance_to_non_alpha = (s, start) => {
  let rec advance = (i, lim) =>
    if i >= lim {
      lim
    } else {
      switch String.get(s, i) {
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' => advance(i + 1, lim)
      | _ => i
      }
    }
  advance(start, String.length(s))
}

/* We are just at the beginning of an ident in s, starting at start. */
let find_ident = (s, start, lim) =>
  if start >= lim {
    raise(Not_found)
  } else {
    switch String.get(s, start) {
    /* Parenthesized ident ? */
    | ('(' | '{') as c =>
      let new_start = start + 1
      let stop = advance_to_closing(c, closing(c), 0, s, new_start)
      (String.sub(s, new_start, stop - start - 1), stop + 1)
    /* Regular ident */
    | _ =>
      let stop = advance_to_non_alpha(s, start + 1)
      (String.sub(s, start, stop - start), stop)
    }
  }

/* Substitute $ident, $(ident), or ${ident} in s,
 according to the function mapping f. */
let add_substitute = (b, f, s) => {
  let lim = String.length(s)
  let rec subst = (previous, i) =>
    if i < lim {
      switch String.get(s, i) {
      | '$' as current if previous == '\\' =>
        add_char(b, current)
        subst(' ', i + 1)
      | '$' =>
        let j = i + 1
        let (ident, next_i) = find_ident(s, j, lim)
        add_string(b, f(ident))
        subst(' ', next_i)
      | current if previous === '\\' =>
        add_char(b, '\\')
        add_char(b, current)
        subst(' ', i + 1)
      | '\\' as current => subst(current, i + 1)
      | current =>
        add_char(b, current)
        subst(current, i + 1)
      }
    } else if previous == '\\' {
      add_char(b, previous)
    }
  subst(' ', 0)
}

let truncate = (b, len) =>
  if len < 0 || len > length(b) {
    invalid_arg("Buffer.truncate")
  } else {
    b.position = len
  }
