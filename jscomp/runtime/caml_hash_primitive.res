/* Copyright (C) 2018 Hongbo Zhang, Authors of ReScript
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

let rotl32 = (x: int, n) => lor(lsl(x, n), lsr(x, 32 - n))

@send external charCodeAt: (string, int) => int = "charCodeAt"

let hash_mix_int = (h, d) => {
  let d = ref(d)
  d.contents = d.contents * 0xcc9e2d51
  d.contents = rotl32(d.contents, 15)
  d.contents = d.contents * 0x1b873593
  let h = ref(lxor(h, d.contents))
  h.contents = rotl32(h.contents, 13)
  h.contents + lsl(h.contents, 2) + 0xe6546b64
}

let hash_final_mix = h => {
  let h = ref(lxor(h, lsr(h, 16)))
  h.contents = h.contents * 0x85ebca6b
  h.contents = lxor(h.contents, lsr(h.contents, 13))
  h.contents = h.contents * 0xc2b2ae35
  lxor(h.contents, lsr(h.contents, 16))
}
/* Caml_nativeint_extern.logand  (h.contents ^ (h.contents >>> 16)) 0x3FFFFFFFn */

let hash_mix_string = (h, s) => {
  let len = Caml_string_extern.length(s)
  let block = len / 4 - 1
  let hash = ref(h)
  for i in 0 to block {
    let j = 4 * i
    let w = lor(
      lor(lor(s->charCodeAt(j), lsl(s->charCodeAt(j + 1), 8)), lsl(s->charCodeAt(j + 2), 16)),
      lsl(s->charCodeAt(j + 3), 24),
    )

    hash.contents = hash_mix_int(hash.contents, w)
  }
  let modulo = land(len, 0b11)
  if modulo != 0 {
    let w = if modulo == 3 {
      lor(
        lor(lsl(s->charCodeAt(len - 1), 16), lsl(s->charCodeAt(len - 2), 8)),
        s->charCodeAt(len - 3),
      )
    } else if modulo == 2 {
      lor(lsl(s->charCodeAt(len - 1), 8), s->charCodeAt(len - 2))
    } else {
      s->charCodeAt(len - 1)
    }

    hash.contents = hash_mix_int(hash.contents, w)
  }
  hash.contents = lxor(hash.contents, len)
  hash.contents
}
