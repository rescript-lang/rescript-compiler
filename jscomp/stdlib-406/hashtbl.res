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

/* Hash tables */

@noalloc external seeded_hash_param: (int, int, int, 'a) => int = "?hash"
/* external old_hash_param :
 int -> int -> 'a -> int = "caml_hash_univ_param" [@@noalloc] */

let hash = x => seeded_hash_param(10, 100, 0, x)
let hash_param = (n1, n2, x) => seeded_hash_param(n1, n2, 0, x)
let seeded_hash = (seed, x) => seeded_hash_param(10, 100, seed, x)

/* We do dynamic hashing, and resize the table and rehash the elements
 when buckets become too long. */

type rec t<'a, 'b> = {
  mutable size: int /* number of entries */,
  mutable data: array<bucketlist<'a, 'b>> /* the buckets */,
  mutable seed: int /* for randomization */,
  mutable initial_size: int /* initial array size */,
}

and bucketlist<'a, 'b> =
  | Empty
  | Cons({mutable key: 'a, mutable data: 'b, mutable next: bucketlist<'a, 'b>})

/* The sign of initial_size encodes the fact that a traversal is
   ongoing or not.

   This disables the efficient in place implementation of resizing.
*/

let ongoing_traversal = h => h.initial_size < 0

let flip_ongoing_traversal = h => h.initial_size = -h.initial_size

/* To pick random seeds if requested */

let randomized_default = false

let randomized = ref(randomized_default)

let randomize = () => randomized := true
let is_randomized = () => randomized.contents

let prng = lazy Random.State.make_self_init()

/* Creating a fresh, empty table */

let rec power_2_above = (x, n) =>
  if x >= n {
    x
  } else if x * 2 < x {
    x /* overflow */
  } else {
    power_2_above(x * 2, n)
  }

let create = (~random=randomized.contents, initial_size) => {
  let s = power_2_above(16, initial_size)
  let seed = if random {
    Random.State.bits(Lazy.force(prng))
  } else {
    0
  }
  {initial_size: s, size: 0, seed, data: Array.make(s, Empty)}
}

let clear = h => {
  h.size = 0
  let len = Array.length(h.data)
  for i in 0 to len - 1 {
    h.data[i] = Empty
  }
}

let reset = h => {
  let len = Array.length(h.data)
  if len == abs(h.initial_size) {
    clear(h)
  } else {
    h.size = 0
    h.data = Array.make(abs(h.initial_size), Empty)
  }
}

let copy_bucketlist = param =>
  switch param {
  | Empty => Empty
  | Cons({key, data, next}) =>
    let rec loop = (prec, param) =>
      switch param {
      | Empty => ()
      | Cons({key, data, next}) =>
        let r = Cons({key, data, next})
        switch prec {
        | Empty => assert(false)
        | Cons(prec) => prec.next = r
        }
        loop(r, next)
      }

    let r = Cons({key, data, next})
    loop(r, next)
    r
  }

let copy = h => {...h, data: Array.map(copy_bucketlist, h.data)}

let length = h => h.size

let resize = (indexfun, h) => {
  let odata = h.data
  let osize = Array.length(odata)
  let nsize = osize * 2
  if nsize >= osize {
    let ndata = Array.make(nsize, Empty)
    let ndata_tail = Array.make(nsize, Empty)
    let inplace = !ongoing_traversal(h)
    h.data = ndata /* so that indexfun sees the new bucket count */
    let rec insert_bucket = param =>
      switch param {
      | Empty => ()
      | Cons({key, data, next}) as cell =>
        let cell = if inplace {
          cell
        } else {
          Cons({key, data, next: Empty})
        }

        let nidx = indexfun(h, key)
        switch ndata_tail[nidx] {
        | Empty => ndata[nidx] = cell
        | Cons(tail) => tail.next = cell
        }
        ndata_tail[nidx] = cell
        insert_bucket(next)
      }

    for i in 0 to osize - 1 {
      insert_bucket(odata[i])
    }
    if inplace {
      for i in 0 to nsize - 1 {
        switch ndata_tail[i] {
        | Empty => ()
        | Cons(tail) => tail.next = Empty
        }
      }
    }
  }
}

let key_index = (h, key) =>
  /* compatibility with old hash tables */
  land(seeded_hash_param(10, 100, h.seed, key), Array.length(h.data) - 1)

let add = (h, key, data) => {
  let i = key_index(h, key)
  let bucket = Cons({key, data, next: h.data[i]})
  h.data[i] = bucket
  h.size = h.size + 1
  if h.size > lsl(Array.length(h.data), 1) {
    resize(key_index, h)
  }
}

let rec remove_bucket = (h, i, key, prec, param) =>
  switch param {
  | Empty => ()
  | Cons({key: k, next}) as c =>
    if compare(k, key) == 0 {
      h.size = h.size - 1
      switch prec {
      | Empty => h.data[i] = next
      | Cons(c) => c.next = next
      }
    } else {
      remove_bucket(h, i, key, c, next)
    }
  }

let remove = (h, key) => {
  let i = key_index(h, key)
  remove_bucket(h, i, key, Empty, h.data[i])
}

let rec find_rec = (key, param) =>
  switch param {
  | Empty => raise(Not_found)
  | Cons({key: k, data, next}) =>
    if compare(key, k) == 0 {
      data
    } else {
      find_rec(key, next)
    }
  }

let find = (h, key) =>
  switch h.data[key_index(h, key)] {
  | Empty => raise(Not_found)
  | Cons({key: k1, data: d1, next: next1}) =>
    if compare(key, k1) == 0 {
      d1
    } else {
      switch next1 {
      | Empty => raise(Not_found)
      | Cons({key: k2, data: d2, next: next2}) =>
        if compare(key, k2) == 0 {
          d2
        } else {
          switch next2 {
          | Empty => raise(Not_found)
          | Cons({key: k3, data: d3, next: next3}) =>
            if compare(key, k3) == 0 {
              d3
            } else {
              find_rec(key, next3)
            }
          }
        }
      }
    }
  }

let rec find_rec_opt = (key, param) =>
  switch param {
  | Empty => None
  | Cons({key: k, data, next}) =>
    if compare(key, k) == 0 {
      Some(data)
    } else {
      find_rec_opt(key, next)
    }
  }

let find_opt = (h, key) =>
  switch h.data[key_index(h, key)] {
  | Empty => None
  | Cons({key: k1, data: d1, next: next1}) =>
    if compare(key, k1) == 0 {
      Some(d1)
    } else {
      switch next1 {
      | Empty => None
      | Cons({key: k2, data: d2, next: next2}) =>
        if compare(key, k2) == 0 {
          Some(d2)
        } else {
          switch next2 {
          | Empty => None
          | Cons({key: k3, data: d3, next: next3}) =>
            if compare(key, k3) == 0 {
              Some(d3)
            } else {
              find_rec_opt(key, next3)
            }
          }
        }
      }
    }
  }

let find_all = (h, key) => {
  let rec find_in_bucket = param =>
    switch param {
    | Empty => list{}
    | Cons({key: k, data, next}) =>
      if compare(k, key) == 0 {
        list{data, ...find_in_bucket(next)}
      } else {
        find_in_bucket(next)
      }
    }
  find_in_bucket(h.data[key_index(h, key)])
}

let rec replace_bucket = (key, data, param) =>
  switch param {
  | Empty => true
  | Cons({key: k, next} as slot) =>
    if compare(k, key) == 0 {
      slot.key = key
      slot.data = data
      false
    } else {
      replace_bucket(key, data, next)
    }
  }

let replace = (h, key, data) => {
  let i = key_index(h, key)
  let l = h.data[i]
  if replace_bucket(key, data, l) {
    h.data[i] = Cons({key, data, next: l})
    h.size = h.size + 1
    if h.size > lsl(Array.length(h.data), 1) {
      resize(key_index, h)
    }
  }
}

let mem = (h, key) => {
  let rec mem_in_bucket = param =>
    switch param {
    | Empty => false
    | Cons({key: k, next}) => compare(k, key) == 0 || mem_in_bucket(next)
    }
  mem_in_bucket(h.data[key_index(h, key)])
}

let iter = (f, h) => {
  let rec do_bucket = param =>
    switch param {
    | Empty => ()
    | Cons({key, data, next}) =>
      f(key, data)
      do_bucket(next)
    }
  let old_trav = ongoing_traversal(h)
  if !old_trav {
    flip_ongoing_traversal(h)
  }
  try {
    let d = h.data
    for i in 0 to Array.length(d) - 1 {
      do_bucket(d[i])
    }
    if !old_trav {
      flip_ongoing_traversal(h)
    }
  } catch {
  | exn if !old_trav =>
    flip_ongoing_traversal(h)
    raise(exn)
  }
}

let rec filter_map_inplace_bucket = (f, h, i, prec, param) =>
  switch param {
  | Empty =>
    switch prec {
    | Empty => h.data[i] = Empty
    | Cons(c) => c.next = Empty
    }
  | Cons({key, data, next} as c) as slot =>
    switch f(key, data) {
    | None =>
      h.size = h.size - 1
      filter_map_inplace_bucket(f, h, i, prec, next)
    | Some(data) =>
      switch prec {
      | Empty => h.data[i] = slot
      | Cons(c) => c.next = slot
      }
      c.data = data
      filter_map_inplace_bucket(f, h, i, slot, next)
    }
  }

let filter_map_inplace = (f, h) => {
  let d = h.data
  let old_trav = ongoing_traversal(h)
  if !old_trav {
    flip_ongoing_traversal(h)
  }
  try for i in 0 to Array.length(d) - 1 {
    filter_map_inplace_bucket(f, h, i, Empty, h.data[i])
  } catch {
  | exn if !old_trav =>
    flip_ongoing_traversal(h)
    raise(exn)
  }
}

let fold = (f, h, init) => {
  let rec do_bucket = (b, accu) =>
    switch b {
    | Empty => accu
    | Cons({key, data, next}) => do_bucket(next, f(key, data, accu))
    }
  let old_trav = ongoing_traversal(h)
  if !old_trav {
    flip_ongoing_traversal(h)
  }
  try {
    let d = h.data
    let accu = ref(init)
    for i in 0 to Array.length(d) - 1 {
      accu := do_bucket(d[i], accu.contents)
    }
    if !old_trav {
      flip_ongoing_traversal(h)
    }
    accu.contents
  } catch {
  | exn if !old_trav =>
    flip_ongoing_traversal(h)
    raise(exn)
  }
}

type statistics = {
  num_bindings: int,
  num_buckets: int,
  max_bucket_length: int,
  bucket_histogram: array<int>,
}

let rec bucket_length = (accu, param) =>
  switch param {
  | Empty => accu
  | Cons({next}) => bucket_length(accu + 1, next)
  }

let stats = h => {
  let mbl = Array.fold_left((m, b) => max(m, bucket_length(0, b)), 0, h.data)
  let histo = Array.make(mbl + 1, 0)
  Array.iter(b => {
    let l = bucket_length(0, b)
    histo[l] = histo[l] + 1
  }, h.data)
  {
    num_bindings: h.size,
    num_buckets: Array.length(h.data),
    max_bucket_length: mbl,
    bucket_histogram: histo,
  }
}

/* Functorial interface */

module type HashedType = {
  type t
  let equal: (t, t) => bool
  let hash: t => int
}

module type SeededHashedType = {
  type t
  let equal: (t, t) => bool
  let hash: (int, t) => int
}

module type S = {
  type key
  type t<'a>
  let create: int => t<'a>
  let clear: t<'a> => unit
  let reset: t<'a> => unit
  let copy: t<'a> => t<'a>
  let add: (t<'a>, key, 'a) => unit
  let remove: (t<'a>, key) => unit
  let find: (t<'a>, key) => 'a
  let find_opt: (t<'a>, key) => option<'a>
  let find_all: (t<'a>, key) => list<'a>
  let replace: (t<'a>, key, 'a) => unit
  let mem: (t<'a>, key) => bool
  let iter: ((key, 'a) => unit, t<'a>) => unit
  let filter_map_inplace: ((key, 'a) => option<'a>, t<'a>) => unit
  let fold: ((key, 'a, 'b) => 'b, t<'a>, 'b) => 'b
  let length: t<'a> => int
  let stats: t<'a> => statistics
}

module type SeededS = {
  type key
  type t<'a>
  let create: (~random: bool=?, int) => t<'a>
  let clear: t<'a> => unit
  let reset: t<'a> => unit
  let copy: t<'a> => t<'a>
  let add: (t<'a>, key, 'a) => unit
  let remove: (t<'a>, key) => unit
  let find: (t<'a>, key) => 'a
  let find_opt: (t<'a>, key) => option<'a>
  let find_all: (t<'a>, key) => list<'a>
  let replace: (t<'a>, key, 'a) => unit
  let mem: (t<'a>, key) => bool
  let iter: ((key, 'a) => unit, t<'a>) => unit
  let filter_map_inplace: ((key, 'a) => option<'a>, t<'a>) => unit
  let fold: ((key, 'a, 'b) => 'b, t<'a>, 'b) => 'b
  let length: t<'a> => int
  let stats: t<'a> => statistics
}

module MakeSeeded = (H: SeededHashedType): (SeededS with type key = H.t) => {
  type key = H.t
  type hashtbl<'a> = t<key, 'a>
  type t<'a> = hashtbl<'a>
  let create = create
  let clear = clear
  let reset = reset
  let copy = copy

  let key_index = (h, key) => land(H.hash(h.seed, key), Array.length(h.data) - 1)

  let add = (h, key, data) => {
    let i = key_index(h, key)
    let bucket = Cons({key, data, next: h.data[i]})
    h.data[i] = bucket
    h.size = h.size + 1
    if h.size > lsl(Array.length(h.data), 1) {
      resize(key_index, h)
    }
  }

  let rec remove_bucket = (h, i, key, prec, param) =>
    switch param {
    | Empty => ()
    | Cons({key: k, next}) as c =>
      if H.equal(k, key) {
        h.size = h.size - 1
        switch prec {
        | Empty => h.data[i] = next
        | Cons(c) => c.next = next
        }
      } else {
        remove_bucket(h, i, key, c, next)
      }
    }

  let remove = (h, key) => {
    let i = key_index(h, key)
    remove_bucket(h, i, key, Empty, h.data[i])
  }

  let rec find_rec = (key, param) =>
    switch param {
    | Empty => raise(Not_found)
    | Cons({key: k, data, next}) =>
      if H.equal(key, k) {
        data
      } else {
        find_rec(key, next)
      }
    }

  let find = (h, key) =>
    switch h.data[key_index(h, key)] {
    | Empty => raise(Not_found)
    | Cons({key: k1, data: d1, next: next1}) =>
      if H.equal(key, k1) {
        d1
      } else {
        switch next1 {
        | Empty => raise(Not_found)
        | Cons({key: k2, data: d2, next: next2}) =>
          if H.equal(key, k2) {
            d2
          } else {
            switch next2 {
            | Empty => raise(Not_found)
            | Cons({key: k3, data: d3, next: next3}) =>
              if H.equal(key, k3) {
                d3
              } else {
                find_rec(key, next3)
              }
            }
          }
        }
      }
    }

  let rec find_rec_opt = (key, param) =>
    switch param {
    | Empty => None
    | Cons({key: k, data, next}) =>
      if H.equal(key, k) {
        Some(data)
      } else {
        find_rec_opt(key, next)
      }
    }

  let find_opt = (h, key) =>
    switch h.data[key_index(h, key)] {
    | Empty => None
    | Cons({key: k1, data: d1, next: next1}) =>
      if H.equal(key, k1) {
        Some(d1)
      } else {
        switch next1 {
        | Empty => None
        | Cons({key: k2, data: d2, next: next2}) =>
          if H.equal(key, k2) {
            Some(d2)
          } else {
            switch next2 {
            | Empty => None
            | Cons({key: k3, data: d3, next: next3}) =>
              if H.equal(key, k3) {
                Some(d3)
              } else {
                find_rec_opt(key, next3)
              }
            }
          }
        }
      }
    }

  let find_all = (h, key) => {
    let rec find_in_bucket = param =>
      switch param {
      | Empty => list{}
      | Cons({key: k, data: d, next}) =>
        if H.equal(k, key) {
          list{d, ...find_in_bucket(next)}
        } else {
          find_in_bucket(next)
        }
      }
    find_in_bucket(h.data[key_index(h, key)])
  }

  let rec replace_bucket = (key, data, param) =>
    switch param {
    | Empty => true
    | Cons({key: k, next} as slot) =>
      if H.equal(k, key) {
        slot.key = key
        slot.data = data
        false
      } else {
        replace_bucket(key, data, next)
      }
    }

  let replace = (h, key, data) => {
    let i = key_index(h, key)
    let l = h.data[i]
    if replace_bucket(key, data, l) {
      h.data[i] = Cons({key, data, next: l})
      h.size = h.size + 1
      if h.size > lsl(Array.length(h.data), 1) {
        resize(key_index, h)
      }
    }
  }

  let mem = (h, key) => {
    let rec mem_in_bucket = param =>
      switch param {
      | Empty => false
      | Cons({key: k, next}) => H.equal(k, key) || mem_in_bucket(next)
      }
    mem_in_bucket(h.data[key_index(h, key)])
  }

  let iter = iter
  let filter_map_inplace = filter_map_inplace
  let fold = fold
  let length = length
  let stats = stats
}

module Make = (H: HashedType): (S with type key = H.t) => {
  include MakeSeeded({
    type t = H.t
    let equal = H.equal
    let hash = (_seed: int, x) => H.hash(x)
  })
  let create = sz => create(~random=false, sz)
}
