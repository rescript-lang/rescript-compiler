/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

let lognot = n => lxor(n, -1l)
let cmn = (q, a, b, x, s, t) => {
  let a = a + q + x + t
  lor(lsl(a, s), lsr(a, 32 - s)) + b
}

let f = (a, b, c, d, x, s, t) => cmn(lor(land(b, c), land(lognot(b), d)), a, b, x, s, t)

let g = (a, b, c, d, x, s, t) => cmn(lor(land(b, d), land(c, lognot(d))), a, b, x, s, t)

let h = (a, b, c, d, x, s, t) => cmn(lxor(lxor(b, c), d), a, b, x, s, t)

let i = (a, b, c, d, x, s, t) => cmn(lxor(c, lor(b, lognot(d))), a, b, x, s, t)

let {unsafe_get, unsafe_set} = module(Caml_array_extern)

let cycle = (x: array<int>, k: array<int>) => {
  let a = ref(x->unsafe_get(0))
  let b = ref(x->unsafe_get(1))
  let c = ref(x->unsafe_get(2))
  let d = ref(x->unsafe_get(3))

  a.contents = f(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(0), 7, 0xd76aa478l)
  d.contents = f(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(1), 12, 0xe8c7b756l)
  c.contents = f(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(2), 17, 0x242070dbl)
  b.contents = f(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(3), 22, 0xc1bdceeel)

  a.contents = f(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(4), 7, 0xf57c0fafl)
  d.contents = f(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(5), 12, 0x4787c62al)
  c.contents = f(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(6), 17, 0xa8304613l)
  b.contents = f(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(7), 22, 0xfd469501l)

  a.contents = f(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(8), 7, 0x698098d8l)
  d.contents = f(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(9), 12, 0x8b44f7afl)
  c.contents = f(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(10), 17, 0xffff5bb1l)
  b.contents = f(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(11), 22, 0x895cd7bel)
  a.contents = f(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(12), 7, 0x6b901122l)
  d.contents = f(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(13), 12, 0xfd987193l)
  c.contents = f(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(14), 17, 0xa679438el)
  b.contents = f(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(15), 22, 0x49b40821l)

  a.contents = g(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(1), 5, 0xf61e2562l)
  d.contents = g(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(6), 9, 0xc040b340l)
  c.contents = g(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(11), 14, 0x265e5a51l)
  b.contents = g(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(0), 20, 0xe9b6c7aal)
  a.contents = g(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(5), 5, 0xd62f105dl)
  d.contents = g(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(10), 9, 0x2441453l)
  c.contents = g(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(15), 14, 0xd8a1e681l)
  b.contents = g(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(4), 20, 0xe7d3fbc8l)
  a.contents = g(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(9), 5, 0x21e1cde6l)
  d.contents = g(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(14), 9, 0xc33707d6l)
  c.contents = g(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(3), 14, 0xf4d50d87l)
  b.contents = g(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(8), 20, 0x455a14edl)
  a.contents = g(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(13), 5, 0xa9e3e905l)
  d.contents = g(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(2), 9, 0xfcefa3f8l)
  c.contents = g(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(7), 14, 0x676f02d9l)
  b.contents = g(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(12), 20, 0x8d2a4c8al)

  a.contents = h(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(5), 4, 0xfffa3942l)
  d.contents = h(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(8), 11, 0x8771f681l)
  c.contents = h(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(11), 16, 0x6d9d6122l)
  b.contents = h(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(14), 23, 0xfde5380cl)
  a.contents = h(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(1), 4, 0xa4beea44l)
  d.contents = h(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(4), 11, 0x4bdecfa9l)
  c.contents = h(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(7), 16, 0xf6bb4b60l)
  b.contents = h(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(10), 23, 0xbebfbc70l)
  a.contents = h(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(13), 4, 0x289b7ec6l)
  d.contents = h(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(0), 11, 0xeaa127fal)
  c.contents = h(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(3), 16, 0xd4ef3085l)
  b.contents = h(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(6), 23, 0x4881d05l)
  a.contents = h(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(9), 4, 0xd9d4d039l)
  d.contents = h(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(12), 11, 0xe6db99e5l)
  c.contents = h(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(15), 16, 0x1fa27cf8l)
  b.contents = h(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(2), 23, 0xc4ac5665l)

  a.contents = i(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(0), 6, 0xf4292244l)
  d.contents = i(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(7), 10, 0x432aff97l)
  c.contents = i(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(14), 15, 0xab9423a7l)
  b.contents = i(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(5), 21, 0xfc93a039l)
  a.contents = i(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(12), 6, 0x655b59c3l)
  d.contents = i(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(3), 10, 0x8f0ccc92l)
  c.contents = i(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(10), 15, 0xffeff47dl)
  b.contents = i(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(1), 21, 0x85845dd1l)
  a.contents = i(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(8), 6, 0x6fa87e4fl)
  d.contents = i(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(15), 10, 0xfe2ce6e0l)
  c.contents = i(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(6), 15, 0xa3014314l)
  b.contents = i(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(13), 21, 0x4e0811a1l)
  a.contents = i(a.contents, b.contents, c.contents, d.contents, k->unsafe_get(4), 6, 0xf7537e82l)
  d.contents = i(d.contents, a.contents, b.contents, c.contents, k->unsafe_get(11), 10, 0xbd3af235l)
  c.contents = i(c.contents, d.contents, a.contents, b.contents, k->unsafe_get(2), 15, 0x2ad7d2bbl)
  b.contents = i(b.contents, c.contents, d.contents, a.contents, k->unsafe_get(9), 21, 0xeb86d391l)

  unsafe_set(x, 0, a.contents + x->unsafe_get(0))
  unsafe_set(x, 1, b.contents + x->unsafe_get(1))
  unsafe_set(x, 2, c.contents + x->unsafe_get(2))
  unsafe_set(x, 3, d.contents + x->unsafe_get(3))
}

let seed_a = 0x67452301l
let seed_b = 0xefcdab89l
let seed_c = 0x98badcfel
let seed_d = 0x10325476l

let state = [seed_a, seed_b, seed_c, seed_d]

let md5blk = [0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l, 0l]

@send external charCodeAt: (string, int) => int = "charCodeAt"

let md5_string = (s: string, start: int, len: int): string => {
  let s = Caml_string_extern.slice(s, start, len)
  let n = Caml_string_extern.length(s)
  let () = {
    state->unsafe_set(0, seed_a)
    state->unsafe_set(1, seed_b)
    state->unsafe_set(2, seed_c)
    state->unsafe_set(3, seed_d)
    for i in 0 to 15 {
      md5blk->unsafe_set(i, 0l)
    }
  }

  let i_end = n / 64
  for i in 1 to i_end {
    for j in 0 to 16 - 1 {
      let k = i * 64 - 64 + j * 4
      md5blk->unsafe_set(
        j,
        charCodeAt(s, k) +
        lsl(s->charCodeAt(k + 1), 8) +
        lsl(s->charCodeAt(k + 2), 16) +
        lsl(s->charCodeAt(k + 3), 24),
      )
    }
    cycle(state, md5blk)
  }

  let s_tail = Caml_string_extern.slice_rest(s, i_end * 64)
  for kk in 0 to 15 {
    md5blk->unsafe_set(kk, 0l)
  }
  let i_end = Caml_string_extern.length(s_tail) - 1
  for i in 0 to i_end {
    md5blk->unsafe_set(
      i / 4,
      lor(unsafe_get(md5blk, i / 4), lsl(charCodeAt(s_tail, i), lsl(mod(i, 4), 3))),
    )
  }
  let i = i_end + 1
  md5blk->unsafe_set(i / 4, lor(unsafe_get(md5blk, i / 4), lsl(0x80l, lsl(mod(i, 4), 3))))
  if i > 55 {
    cycle(state, md5blk)
    for i in 0 to 15 {
      md5blk->unsafe_set(i, 0)
    }
  }
  unsafe_set(md5blk, 14, n * 8)
  cycle(state, md5blk)
  Caml_string_extern.of_small_int32_array([
    land(state->unsafe_get(0), 0xff),
    land(asr(state->unsafe_get(0), 8), 0xff),
    land(asr(state->unsafe_get(0), 16), 0xff),
    land(asr(state->unsafe_get(0), 24), 0xff),
    land(state->unsafe_get(1), 0xff),
    land(asr(state->unsafe_get(1), 8), 0xff),
    land(asr(state->unsafe_get(1), 16), 0xff),
    land(asr(state->unsafe_get(1), 24), 0xff),
    land(state->unsafe_get(2), 0xff),
    land(asr(state->unsafe_get(2), 8), 0xff),
    land(asr(state->unsafe_get(2), 16), 0xff),
    land(asr(state->unsafe_get(2), 24), 0xff),
    land(state->unsafe_get(3), 0xff),
    land(asr(state->unsafe_get(3), 8), 0xff),
    land(asr(state->unsafe_get(3), 16), 0xff),
    land(asr(state->unsafe_get(3), 24), 0xff),
  ])
}
