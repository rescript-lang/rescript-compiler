/* ********************************************************************* */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique.  All rights reserved.  This file is distributed */
/* under the terms of the GNU Library General Public License, with */
/* the special exception on linking described in file ../LICENSE. */
/*  */
/* Adapted by authors of ReScript without using functors */
/* ********************************************************************* */

/** specalized when key type is `int`, more efficient
    than the generic type
*/
module Int = Belt_MapInt

/** specalized when key type is `string`, more efficient
    than the generic type */
module String = Belt_MapString

/** seprate function from data, a more verboe but slightly
    more efficient
*/
module Dict = Belt_MapDict

type id<'key, 'id> = Belt_Id.comparable<'key, 'id>
type cmp<'key, 'id> = Belt_Id.cmp<'key, 'id>

type t<'k, 'v, 'id> = {
  cmp: cmp<'k, 'id>,
  data: Dict.t<'k, 'v, 'id>,
}

let fromArray = (type k idx, data, ~id: id<k, idx>) => {
  module M = unpack(id)
  let cmp = M.cmp
  {cmp, data: Dict.fromArray(~cmp, data)}
}

let remove = (m, x) => {
  let {cmp, data: odata} = m
  let newData = Dict.remove(odata, x, ~cmp)
  if newData === odata {
    m
  } else {
    {cmp, data: newData}
  }
}

let removeMany = (m, x) => {
  let {cmp, data: odata} = m
  let newData = Dict.removeMany(odata, x, ~cmp)
  {cmp, data: newData}
}

let set = (m, key, d) => {
  let cmp = m.cmp
  {cmp, data: Dict.set(~cmp, m.data, key, d)}
}

let mergeMany = (m, e) => {
  let cmp = m.cmp
  {cmp, data: Dict.mergeMany(~cmp, m.data, e)}
}

let updateU = (m, key, f) => {
  let cmp = m.cmp
  {cmp, data: Dict.updateU(~cmp, m.data, key, f)}
}
let update = (m, key, f) => updateU(m, key, (. a) => f(a))
let split = (m, x) => {
  let cmp = m.cmp
  let ((l, r), b) = Dict.split(~cmp, m.data, x)
  (({cmp, data: l}, {cmp, data: r}), b)
}

let mergeU = (s1, s2, f) => {
  let cmp = s1.cmp
  {cmp, data: Dict.mergeU(~cmp, s1.data, s2.data, f)}
}

let merge = (s1, s2, f) => mergeU(s1, s2, (. a, b, c) => f(a, b, c))

let make = (type key idx, ~id: id<key, idx>) => {
  module M = unpack(id)
  {cmp: M.cmp, data: Dict.empty}
}

let isEmpty = map => Dict.isEmpty(map.data)

let findFirstByU = (m, f) => Dict.findFirstByU(m.data, f)
let findFirstBy = (m, f) => findFirstByU(m, (. a, b) => f(a, b))
let forEachU = (m, f) => Dict.forEachU(m.data, f)
let forEach = (m, f) => forEachU(m, (. a, b) => f(a, b))
let reduceU = (m, acc, f) => Dict.reduceU(m.data, acc, f)
let reduce = (m, acc, f) => reduceU(m, acc, (. a, b, c) => f(a, b, c))
let everyU = (m, f) => Dict.everyU(m.data, f)
let every = (m, f) => everyU(m, (. a, b) => f(a, b))
let someU = (m, f) => Dict.someU(m.data, f)
let some = (m, f) => someU(m, (. a, b) => f(a, b))
let keepU = (m, f) => {cmp: m.cmp, data: Dict.keepU(m.data, f)}
let keep = (m, f) => keepU(m, (. a, b) => f(a, b))

let partitionU = (m, p) => {
  let cmp = m.cmp
  let (l, r) = m.data->Dict.partitionU(p)
  ({cmp, data: l}, {cmp, data: r})
}
let partition = (m, p) => partitionU(m, (. a, b) => p(a, b))

let mapU = (m, f) => {cmp: m.cmp, data: Dict.mapU(m.data, f)}
let map = (m, f) => mapU(m, (. a) => f(a))
let mapWithKeyU = (m, f) => {cmp: m.cmp, data: Dict.mapWithKeyU(m.data, f)}
let mapWithKey = (m, f) => mapWithKeyU(m, (. a, b) => f(a, b))
let size = map => Dict.size(map.data)
let toList = map => Dict.toList(map.data)
let toArray = m => Dict.toArray(m.data)
let keysToArray = m => Dict.keysToArray(m.data)
let valuesToArray = m => Dict.valuesToArray(m.data)
let minKey = m => Dict.minKey(m.data)
let minKeyUndefined = m => Dict.minKeyUndefined(m.data)
let maxKey = m => Dict.maxKey(m.data)
let maxKeyUndefined = m => Dict.maxKeyUndefined(m.data)
let minimum = m => Dict.minimum(m.data)
let minUndefined = m => Dict.minUndefined(m.data)
let maximum = m => Dict.maximum(m.data)
let maxUndefined = m => Dict.maxUndefined(m.data)

let get = (map, x) => Dict.get(~cmp=map.cmp, map.data, x)

let getUndefined = (map, x) => Dict.getUndefined(~cmp=map.cmp, map.data, x)

let getWithDefault = (map, x, def) => Dict.getWithDefault(~cmp=map.cmp, map.data, x, def)

let getExn = (map, x) => Dict.getExn(~cmp=map.cmp, map.data, x)

let has = (map, x) => Dict.has(~cmp=map.cmp, map.data, x)

let checkInvariantInternal = m => Dict.checkInvariantInternal(m.data)

let eqU = (m1, m2, veq) => Dict.eqU(~kcmp=m1.cmp, ~veq, m1.data, m2.data)
let eq = (m1, m2, veq) => eqU(m1, m2, (. a, b) => veq(a, b))

let cmpU = (m1, m2, vcmp) => Dict.cmpU(~kcmp=m1.cmp, ~vcmp, m1.data, m2.data)
let cmp = (m1, m2, vcmp) => cmpU(m1, m2, (. a, b) => vcmp(a, b))

let getData = m => m.data

let getId = (type key identity, m: t<key, _, identity>): id<key, identity> => {
  module T = {
    type identity = identity
    type t = key
    let cmp = m.cmp
  }
  module(T)
}

let packIdData = (type key idx, ~id: id<key, idx>, ~data) => {
  module M = unpack(id)
  {cmp: M.cmp, data}
}
