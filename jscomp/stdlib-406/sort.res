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

/* Merging and sorting */

open Array

let rec merge = (order, l1, l2) =>
  switch l1 {
  | list{} => l2
  | list{h1, ...t1} =>
    switch l2 {
    | list{} => l1
    | list{h2, ...t2} =>
      if order(h1, h2) {
        list{h1, ...merge(order, t1, l2)}
      } else {
        list{h2, ...merge(order, l1, t2)}
      }
    }
  }

let list = (order, l) => {
  let rec initlist = param =>
    switch param {
    | list{} => list{}
    | list{e} => list{list{e}}
    | list{e1, e2, ...rest} =>
      list{
        if order(e1, e2) {
          list{e1, e2}
        } else {
          list{e2, e1}
        },
        ...initlist(rest),
      }
    }
  let rec merge2 = param =>
    switch param {
    | list{l1, l2, ...rest} => list{merge(order, l1, l2), ...merge2(rest)}
    | x => x
    }
  let rec mergeall = param =>
    switch param {
    | list{} => list{}
    | list{l} => l
    | llist => mergeall(merge2(llist))
    }
  mergeall(initlist(l))
}

let swap = (arr, i, j) => {
  let tmp = unsafe_get(arr, i)
  unsafe_set(arr, i, unsafe_get(arr, j))
  unsafe_set(arr, j, tmp)
}

/* There is a known performance bug in the code below.  If you find
   it, don't bother reporting it.  You're not supposed to use this
   module anyway. */
let array = (cmp, arr) => {
  let rec qsort = (lo, hi) =>
    if hi - lo >= 6 {
      let mid = lsr(lo + hi, 1)

      /* Select median value from among LO, MID, and HI. Rearrange
         LO and HI so the three values are sorted. This lowers the
         probability of picking a pathological pivot.  It also
         avoids extra comparisons on i and j in the two tight "while"
         loops below. */
      if cmp(unsafe_get(arr, mid), unsafe_get(arr, lo)) {
        swap(arr, mid, lo)
      }
      if cmp(unsafe_get(arr, hi), unsafe_get(arr, mid)) {
        swap(arr, mid, hi)
        if cmp(unsafe_get(arr, mid), unsafe_get(arr, lo)) {
          swap(arr, mid, lo)
        }
      }
      let pivot = unsafe_get(arr, mid)
      let i = ref(lo + 1) and j = ref(hi - 1)
      if !cmp(pivot, unsafe_get(arr, hi)) || !cmp(unsafe_get(arr, lo), pivot) {
        raise(Invalid_argument("Sort.array"))
      }
      while i.contents < j.contents {
        while !cmp(pivot, unsafe_get(arr, i.contents)) {
          incr(i)
        }
        while !cmp(unsafe_get(arr, j.contents), pivot) {
          decr(j)
        }
        if i.contents < j.contents {
          swap(arr, i.contents, j.contents)
        }
        incr(i)
        decr(j)
      }

      /* Recursion on smaller half, tail-call on larger half */
      if j.contents - lo <= hi - i.contents {
        qsort(lo, j.contents)
        qsort(i.contents, hi)
      } else {
        qsort(i.contents, hi)
        qsort(lo, j.contents)
      }
    }
  qsort(0, Array.length(arr) - 1)
  /* Finish sorting by insertion sort */
  for i in 1 to Array.length(arr) - 1 {
    let val_i = unsafe_get(arr, i)
    if !cmp(unsafe_get(arr, i - 1), val_i) {
      unsafe_set(arr, i, unsafe_get(arr, i - 1))
      let j = ref(i - 1)
      while j.contents >= 1 && !cmp(unsafe_get(arr, j.contents - 1), val_i) {
        unsafe_set(arr, j.contents, unsafe_get(arr, j.contents - 1))
        decr(j)
      }
      unsafe_set(arr, j.contents, val_i)
    }
  }
}
