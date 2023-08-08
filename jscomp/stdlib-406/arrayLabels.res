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

/* Array operations */

external length: array<'a> => int = "%array_length"
external get: (array<'a>, int) => 'a = "%array_safe_get"
external set: (array<'a>, int, 'a) => unit = "%array_safe_set"
external unsafe_get: (array<'a>, int) => 'a = "%array_unsafe_get"
external unsafe_set: (array<'a>, int, 'a) => unit = "%array_unsafe_set"
external make: (int, 'a) => array<'a> = "?make_vect"
external create: (int, 'a) => array<'a> = "?make_vect"
external unsafe_sub: (array<'a>, int, int) => array<'a> = "?array_sub"

@send external append_prim: (array<'a>, array<'a>) => array<'a> = "concat"

external concat: list<array<'a>> => array<'a> = "?array_concat"
external unsafe_blit: (array<'a>, int, array<'a>, int, int) => unit = "?array_blit"
external create_float: int => array<float> = "?make_float_vect"
let make_float = create_float

module Floatarray = {
  external create: int => floatarray = "?floatarray_create"
  external length: floatarray => int = "%floatarray_length"
  external get: (floatarray, int) => float = "%floatarray_safe_get"
  external set: (floatarray, int, float) => unit = "%floatarray_safe_set"
  external unsafe_get: (floatarray, int) => float = "%floatarray_unsafe_get"
  external unsafe_set: (floatarray, int, float) => unit = "%floatarray_unsafe_set"
}

let init = (l, ~f) =>
  if l == 0 {
    []
  } else if l < 0 {
    invalid_arg("Array.init")
  } else {
    /* See #6575. We could also check for maximum array size, but this depends
     on whether we create a float array or a regular one... */

    let res = create(l, f(0))
    for i in 1 to pred(l) {
      unsafe_set(res, i, f(i))
    }
    res
  }

let make_matrix = (~dimx as sx, ~dimy as sy, init) => {
  let res = create(sx, [])
  for x in 0 to pred(sx) {
    unsafe_set(res, x, create(sy, init))
  }
  res
}

let create_matrix = make_matrix

let copy = a => {
  let l = length(a)
  if l == 0 {
    []
  } else {
    unsafe_sub(a, 0, l)
  }
}

let append = (a1, a2) => {
  let l1 = length(a1)
  if l1 == 0 {
    copy(a2)
  } else if length(a2) == 0 {
    unsafe_sub(a1, 0, l1)
  } else {
    append_prim(a1, a2)
  }
}

let sub = (a, ~pos as ofs, ~len) =>
  if ofs < 0 || (len < 0 || ofs > length(a) - len) {
    invalid_arg("Array.sub")
  } else {
    unsafe_sub(a, ofs, len)
  }

let fill = (a, ~pos as ofs, ~len, v) =>
  if ofs < 0 || (len < 0 || ofs > length(a) - len) {
    invalid_arg("Array.fill")
  } else {
    for i in ofs to ofs + len - 1 {
      unsafe_set(a, i, v)
    }
  }

let blit = (~src as a1, ~src_pos as ofs1, ~dst as a2, ~dst_pos as ofs2, ~len) =>
  if len < 0 || (ofs1 < 0 || (ofs1 > length(a1) - len || (ofs2 < 0 || ofs2 > length(a2) - len))) {
    invalid_arg("Array.blit")
  } else {
    unsafe_blit(a1, ofs1, a2, ofs2, len)
  }

let iter = (~f, a) =>
  for i in 0 to length(a) - 1 {
    f(unsafe_get(a, i))
  }

let iter2 = (~f, a, b) =>
  if length(a) != length(b) {
    invalid_arg("Array.iter2: arrays must have the same length")
  } else {
    for i in 0 to length(a) - 1 {
      f(unsafe_get(a, i), unsafe_get(b, i))
    }
  }

let map = (~f, a) => {
  let l = length(a)
  if l == 0 {
    []
  } else {
    let r = create(l, f(unsafe_get(a, 0)))
    for i in 1 to l - 1 {
      unsafe_set(r, i, f(unsafe_get(a, i)))
    }
    r
  }
}

let map2 = (~f, a, b) => {
  let la = length(a)
  let lb = length(b)
  if la != lb {
    invalid_arg("Array.map2: arrays must have the same length")
  } else if la == 0 {
    []
  } else {
    let r = create(la, f(unsafe_get(a, 0), unsafe_get(b, 0)))
    for i in 1 to la - 1 {
      unsafe_set(r, i, f(unsafe_get(a, i), unsafe_get(b, i)))
    }
    r
  }
}

let iteri = (~f, a) =>
  for i in 0 to length(a) - 1 {
    f(i, unsafe_get(a, i))
  }

let mapi = (~f, a) => {
  let l = length(a)
  if l == 0 {
    []
  } else {
    let r = create(l, f(0, unsafe_get(a, 0)))
    for i in 1 to l - 1 {
      unsafe_set(r, i, f(i, unsafe_get(a, i)))
    }
    r
  }
}

let to_list = a => {
  let rec tolist = (i, res) =>
    if i < 0 {
      res
    } else {
      tolist(i - 1, list{unsafe_get(a, i), ...res})
    }
  tolist(length(a) - 1, list{})
}

/* Cannot use List.length here because the List module depends on Array. */
let rec list_length = (accu, param) =>
  switch param {
  | list{} => accu
  | list{_, ...t} => list_length(succ(accu), t)
  }

let of_list = param =>
  switch param {
  | list{} => []
  | list{hd, ...tl} as l =>
    let a = create(list_length(0, l), hd)
    let rec fill = (i, param) =>
      switch param {
      | list{} => a
      | list{hd, ...tl} =>
        unsafe_set(a, i, hd)
        fill(i + 1, tl)
      }
    fill(1, tl)
  }

let fold_left = (~f, ~init as x, a) => {
  let r = ref(x)
  for i in 0 to length(a) - 1 {
    r := f(r.contents, unsafe_get(a, i))
  }
  r.contents
}

let fold_right = (~f, a, ~init as x) => {
  let r = ref(x)
  for i in length(a) - 1 downto 0 {
    r := f(unsafe_get(a, i), r.contents)
  }
  r.contents
}

let exists = (~f as p, a) => {
  let n = length(a)
  let rec loop = i =>
    if i == n {
      false
    } else if p(unsafe_get(a, i)) {
      true
    } else {
      loop(succ(i))
    }
  loop(0)
}

let for_all = (~f as p, a) => {
  let n = length(a)
  let rec loop = i =>
    if i == n {
      true
    } else if p(unsafe_get(a, i)) {
      loop(succ(i))
    } else {
      false
    }
  loop(0)
}

let mem = (x, ~set as a) => {
  let n = length(a)
  let rec loop = i =>
    if i == n {
      false
    } else if compare(unsafe_get(a, i), x) == 0 {
      true
    } else {
      loop(succ(i))
    }
  loop(0)
}

let memq = (x, ~set as a) => {
  let n = length(a)
  let rec loop = i =>
    if i == n {
      false
    } else if x === unsafe_get(a, i) {
      true
    } else {
      loop(succ(i))
    }
  loop(0)
}

exception Bottom(int)
let sort = (~cmp, a) => {
  let maxson = (l, i) => {
    let i31 = i + i + i + 1
    let x = ref(i31)
    if i31 + 2 < l {
      if cmp(get(a, i31), get(a, i31 + 1)) < 0 {
        x := i31 + 1
      }
      if cmp(get(a, x.contents), get(a, i31 + 2)) < 0 {
        x := i31 + 2
      }
      x.contents
    } else if i31 + 1 < l && cmp(get(a, i31), get(a, i31 + 1)) < 0 {
      i31 + 1
    } else if i31 < l {
      i31
    } else {
      raise(Bottom(i))
    }
  }

  let rec trickledown = (l, i, e) => {
    let j = maxson(l, i)
    if cmp(get(a, j), e) > 0 {
      set(a, i, get(a, j))
      trickledown(l, j, e)
    } else {
      set(a, i, e)
    }
  }

  let trickle = (l, i, e) =>
    try trickledown(l, i, e) catch {
    | Bottom(i) => set(a, i, e)
    }
  let rec bubbledown = (l, i) => {
    let j = maxson(l, i)
    set(a, i, get(a, j))
    bubbledown(l, j)
  }

  let bubble = (l, i) =>
    try bubbledown(l, i) catch {
    | Bottom(i) => i
    }
  let rec trickleup = (i, e) => {
    let father = (i - 1) / 3
    assert(i != father)
    if cmp(get(a, father), e) < 0 {
      set(a, i, get(a, father))
      if father > 0 {
        trickleup(father, e)
      } else {
        set(a, 0, e)
      }
    } else {
      set(a, i, e)
    }
  }

  let l = length(a)
  for i in (l + 1) / 3 - 1 downto 0 {
    trickle(l, i, get(a, i))
  }
  for i in l - 1 downto 2 {
    let e = get(a, i)
    set(a, i, get(a, 0))
    trickleup(bubble(i, 0), e)
  }
  if l > 1 {
    let e = get(a, 1)
    set(a, 1, get(a, 0))
    set(a, 0, e)
  }
}

let cutoff = 5
let stable_sort = (~cmp, a) => {
  let merge = (src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs) => {
    let src1r = src1ofs + src1len and src2r = src2ofs + src2len
    let rec loop = (i1, s1, i2, s2, d) =>
      if cmp(s1, s2) <= 0 {
        set(dst, d, s1)
        let i1 = i1 + 1
        if i1 < src1r {
          loop(i1, get(a, i1), i2, s2, d + 1)
        } else {
          blit(~src=src2, ~src_pos=i2, ~dst, ~dst_pos=d + 1, ~len=src2r - i2)
        }
      } else {
        set(dst, d, s2)
        let i2 = i2 + 1
        if i2 < src2r {
          loop(i1, s1, i2, get(src2, i2), d + 1)
        } else {
          blit(~src=a, ~src_pos=i1, ~dst, ~dst_pos=d + 1, ~len=src1r - i1)
        }
      }
    loop(src1ofs, get(a, src1ofs), src2ofs, get(src2, src2ofs), dstofs)
  }

  let isortto = (srcofs, dst, dstofs, len) =>
    for i in 0 to len - 1 {
      let e = get(a, srcofs + i)
      let j = ref(dstofs + i - 1)
      while j.contents >= dstofs && cmp(get(dst, j.contents), e) > 0 {
        set(dst, j.contents + 1, get(dst, j.contents))
        decr(j)
      }
      set(dst, j.contents + 1, e)
    }

  let rec sortto = (srcofs, dst, dstofs, len) =>
    if len <= cutoff {
      isortto(srcofs, dst, dstofs, len)
    } else {
      let l1 = len / 2
      let l2 = len - l1
      sortto(srcofs + l1, dst, dstofs + l1, l2)
      sortto(srcofs, a, srcofs + l2, l1)
      merge(srcofs + l2, l1, dst, dstofs + l1, l2, dst, dstofs)
    }

  let l = length(a)
  if l <= cutoff {
    isortto(0, a, 0, l)
  } else {
    let l1 = l / 2
    let l2 = l - l1
    let t = make(l2, get(a, 0))
    sortto(l1, t, 0, l2)
    sortto(0, a, l2, l1)
    merge(l2, l1, t, 0, l2, a, 0)
  }
}

let fast_sort = stable_sort
