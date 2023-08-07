/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Jacques Garrigue, Kyoto University RIMS */
/*  */
/* Copyright 2001 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */

/*** Extra labeled libraries.

   This meta-module provides labelized version of the {!Hashtbl},
   {!Map} and {!Set} modules.

   They only differ by their labels. They are provided to help
   porting from previous versions of OCaml.
   The contents of this module are subject to change.
*/

module Hashtbl: {
  type t<'a, 'b> = Hashtbl.t<'a, 'b>
  let create: (~random: bool=?, int) => t<'a, 'b>
  let clear: t<'a, 'b> => unit
  let reset: t<'a, 'b> => unit
  let copy: t<'a, 'b> => t<'a, 'b>
  let add: (t<'a, 'b>, ~key: 'a, ~data: 'b) => unit
  let find: (t<'a, 'b>, 'a) => 'b
  let find_opt: (t<'a, 'b>, 'a) => option<'b>
  let find_all: (t<'a, 'b>, 'a) => list<'b>
  let mem: (t<'a, 'b>, 'a) => bool
  let remove: (t<'a, 'b>, 'a) => unit
  let replace: (t<'a, 'b>, ~key: 'a, ~data: 'b) => unit
  let iter: (~f: (~key: 'a, ~data: 'b) => unit, t<'a, 'b>) => unit
  let filter_map_inplace: (~f: (~key: 'a, ~data: 'b) => option<'b>, t<'a, 'b>) => unit
  let fold: (~f: (~key: 'a, ~data: 'b, 'c) => 'c, t<'a, 'b>, ~init: 'c) => 'c
  let length: t<'a, 'b> => int
  let randomize: unit => unit
  let is_randomized: unit => bool
  type statistics = Hashtbl.statistics
  let stats: t<'a, 'b> => statistics
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
  module Make: (H: HashedType) => (S with type key = H.t)
  module MakeSeeded: (H: SeededHashedType) => (SeededS with type key = H.t)
  let hash: 'a => int
  let seeded_hash: (int, 'a) => int
  let hash_param: (int, int, 'a) => int
  let seeded_hash_param: (int, int, int, 'a) => int
}

module Map: {
  module type OrderedType = Map.OrderedType
  module type S = {
    type rec key
    and t<+'a>
    let empty: t<'a>
    let is_empty: t<'a> => bool
    let mem: (key, t<'a>) => bool
    let add: (~key: key, ~data: 'a, t<'a>) => t<'a>
    let update: (~key: key, ~f: option<'a> => option<'a>, t<'a>) => t<'a>
    let singleton: (key, 'a) => t<'a>
    let remove: (key, t<'a>) => t<'a>
    let merge: (~f: (key, option<'a>, option<'b>) => option<'c>, t<'a>, t<'b>) => t<'c>
    let union: (~f: (key, 'a, 'a) => option<'a>, t<'a>, t<'a>) => t<'a>
    let compare: (~cmp: ('a, 'a) => int, t<'a>, t<'a>) => int
    let equal: (~cmp: ('a, 'a) => bool, t<'a>, t<'a>) => bool
    let iter: (~f: (~key: key, ~data: 'a) => unit, t<'a>) => unit
    let fold: (~f: (~key: key, ~data: 'a, 'b) => 'b, t<'a>, ~init: 'b) => 'b
    let for_all: (~f: (key, 'a) => bool, t<'a>) => bool
    let exists: (~f: (key, 'a) => bool, t<'a>) => bool
    let filter: (~f: (key, 'a) => bool, t<'a>) => t<'a>
    let partition: (~f: (key, 'a) => bool, t<'a>) => (t<'a>, t<'a>)
    let cardinal: t<'a> => int
    let bindings: t<'a> => list<(key, 'a)>
    let min_binding: t<'a> => (key, 'a)
    let min_binding_opt: t<'a> => option<(key, 'a)>
    let max_binding: t<'a> => (key, 'a)
    let max_binding_opt: t<'a> => option<(key, 'a)>
    let choose: t<'a> => (key, 'a)
    let choose_opt: t<'a> => option<(key, 'a)>
    let split: (key, t<'a>) => (t<'a>, option<'a>, t<'a>)
    let find: (key, t<'a>) => 'a
    let find_opt: (key, t<'a>) => option<'a>
    let find_first: (~f: key => bool, t<'a>) => (key, 'a)
    let find_first_opt: (~f: key => bool, t<'a>) => option<(key, 'a)>
    let find_last: (~f: key => bool, t<'a>) => (key, 'a)
    let find_last_opt: (~f: key => bool, t<'a>) => option<(key, 'a)>
    let map: (~f: 'a => 'b, t<'a>) => t<'b>
    let mapi: (~f: (key, 'a) => 'b, t<'a>) => t<'b>
  }
  module Make: (Ord: OrderedType) => (S with type key = Ord.t)
}

module Set: {
  module type OrderedType = Set.OrderedType
  module type S = {
    type rec elt
    and t
    let empty: t
    let is_empty: t => bool
    let mem: (elt, t) => bool
    let add: (elt, t) => t
    let singleton: elt => t
    let remove: (elt, t) => t
    let union: (t, t) => t
    let inter: (t, t) => t
    let diff: (t, t) => t
    let compare: (t, t) => int
    let equal: (t, t) => bool
    let subset: (t, t) => bool
    let iter: (~f: elt => unit, t) => unit
    let map: (~f: elt => elt, t) => t
    let fold: (~f: (elt, 'a) => 'a, t, ~init: 'a) => 'a
    let for_all: (~f: elt => bool, t) => bool
    let exists: (~f: elt => bool, t) => bool
    let filter: (~f: elt => bool, t) => t
    let partition: (~f: elt => bool, t) => (t, t)
    let cardinal: t => int
    let elements: t => list<elt>
    let min_elt: t => elt
    let min_elt_opt: t => option<elt>
    let max_elt: t => elt
    let max_elt_opt: t => option<elt>
    let choose: t => elt
    let choose_opt: t => option<elt>
    let split: (elt, t) => (t, bool, t)
    let find: (elt, t) => elt
    let find_opt: (elt, t) => option<elt>
    let find_first: (~f: elt => bool, t) => elt
    let find_first_opt: (~f: elt => bool, t) => option<elt>
    let find_last: (~f: elt => bool, t) => elt
    let find_last_opt: (~f: elt => bool, t) => option<elt>
    let of_list: list<elt> => t
  }
  module Make: (Ord: OrderedType) => (S with type elt = Ord.t)
}
