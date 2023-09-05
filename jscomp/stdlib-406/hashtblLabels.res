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

type t<'a, 'b> = Hashtbl.t<'a, 'b>

let {
  create,
  clear,
  reset,
  copy,
  add,
  find,
  find_opt,
  find_all,
  mem,
  remove,
  replace,
  iter,
  filter_map_inplace,
  fold,
  length,
  randomize,
  is_randomized,
  stats,
  hash,
  seeded_hash,
  hash_param,
  seeded_hash_param,
} = module(Hashtbl)

let add = (tbl, ~key, ~data) => add(tbl, key, data)

let replace = (tbl, ~key, ~data) => replace(tbl, key, data)

let iter = (~f, tbl) => iter((key, data) => f(~key, ~data), tbl)

let filter_map_inplace = (~f, tbl) => filter_map_inplace((key, data) => f(~key, ~data), tbl)

let fold = (~f, tbl, ~init) => fold((key, data, acc) => f(~key, ~data, acc), tbl, init)

type statistics = Hashtbl.statistics = {
  num_bindings: int,
  num_buckets: int,
  max_bucket_length: int,
  bucket_histogram: array<int>,
}

/* Functorial interface */

module type HashedType = Hashtbl.HashedType

module type SeededHashedType = Hashtbl.SeededHashedType

module type S = {
  type rec key
  and t<'a>
  let create: int => t<'a>
  let clear: t<'a> => unit
  let reset: t<'a> => unit
  let copy: t<'a> => t<'a>
  let add: (t<'a>, ~key: key, ~data: 'a) => unit
  let remove: (t<'a>, key) => unit
  let find: (t<'a>, key) => 'a
  let find_opt: (t<'a>, key) => option<'a>
  let find_all: (t<'a>, key) => list<'a>
  let replace: (t<'a>, ~key: key, ~data: 'a) => unit
  let mem: (t<'a>, key) => bool
  let iter: (~f: (~key: key, ~data: 'a) => unit, t<'a>) => unit
  let filter_map_inplace: (~f: (~key: key, ~data: 'a) => option<'a>, t<'a>) => unit
  let fold: (~f: (~key: key, ~data: 'a, 'b) => 'b, t<'a>, ~init: 'b) => 'b
  let length: t<'a> => int
  let stats: t<'a> => statistics
}

module type SeededS = {
  type rec key
  and t<'a>
  let create: (~random: bool=?, int) => t<'a>
  let clear: t<'a> => unit
  let reset: t<'a> => unit
  let copy: t<'a> => t<'a>
  let add: (t<'a>, ~key: key, ~data: 'a) => unit
  let remove: (t<'a>, key) => unit
  let find: (t<'a>, key) => 'a
  let find_opt: (t<'a>, key) => option<'a>
  let find_all: (t<'a>, key) => list<'a>
  let replace: (t<'a>, ~key: key, ~data: 'a) => unit
  let mem: (t<'a>, key) => bool
  let iter: (~f: (~key: key, ~data: 'a) => unit, t<'a>) => unit
  let filter_map_inplace: (~f: (~key: key, ~data: 'a) => option<'a>, t<'a>) => unit
  let fold: (~f: (~key: key, ~data: 'a, 'b) => 'b, t<'a>, ~init: 'b) => 'b
  let length: t<'a> => int
  let stats: t<'a> => statistics
}

module MakeSeeded = (H: SeededHashedType): (SeededS with type key = H.t) => {
  include Hashtbl.MakeSeeded(H)
  let add = (tbl, ~key, ~data) => add(tbl, key, data)
  let replace = (tbl, ~key, ~data) => replace(tbl, key, data)

  let iter = (~f, tbl) => iter((key, data) => f(~key, ~data), tbl)

  let filter_map_inplace = (~f, tbl) => filter_map_inplace((key, data) => f(~key, ~data), tbl)

  let fold = (~f, tbl, ~init) => fold((key, data, acc) => f(~key, ~data, acc), tbl, init)
}

module Make = (H: HashedType): (S with type key = H.t) => {
  include MakeSeeded({
    type t = H.t
    let equal = H.equal
    let hash = (_seed: int, x) => H.hash(x)
  })
  let create = sz => create(~random=false, sz)
}
