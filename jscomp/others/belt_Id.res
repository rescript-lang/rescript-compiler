/* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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

type hash<'a, 'id> = (. 'a) => int
type eq<'a, 'id> = (. 'a, 'a) => bool
type cmp<'a, 'id> = (. 'a, 'a) => int

external getHashInternal: hash<'a, 'id> => (. 'a) => int = "%identity"
external getEqInternal: eq<'a, 'id> => (. 'a, 'a) => bool = "%identity"
external getCmpInternal: cmp<'a, 'id> => (. 'a, 'a) => int = "%identity"

module type Comparable = {
  type identity
  type t
  let cmp: cmp<t, identity>
}

type comparable<'key, 'id> = module(Comparable with type t = 'key and type identity = 'id)

module MakeComparableU = (
  M: {
    type t
    let cmp: (. t, t) => int
  },
) => {
  type identity
  include M
}

module MakeComparable = (
  M: {
    type t
    let cmp: (t, t) => int
  },
) => {
  type identity
  type t = M.t
  /* see https://github.com/rescript-lang/rescript-compiler/pull/2589/files/5ef875b7665ee08cfdc59af368fc52bac1fe9130#r173330825 */
  let cmp = {
    let cmp = M.cmp
    (. a, b) => cmp(a, b)
  }
}

let comparableU = (type key, ~cmp): module(Comparable with type t = key) =>
  module(
    MakeComparableU({
      type t = key
      let cmp = cmp
    })
  )

let comparable = (type key, ~cmp) => {
  module N = MakeComparable({
    type t = key
    let cmp = cmp
  })
  module(N: Comparable with type t = key)
}

module type Hashable = {
  type identity
  type t
  let hash: hash<t, identity>
  let eq: eq<t, identity>
}

type hashable<'key, 'id> = module(Hashable with type t = 'key and type identity = 'id)

module MakeHashableU = (
  M: {
    type t
    let hash: (. t) => int
    let eq: (. t, t) => bool
  },
) => {
  type identity
  include M
}

module MakeHashable = (
  M: {
    type t
    let hash: t => int
    let eq: (t, t) => bool
  },
) => {
  type identity
  type t = M.t
  let hash = {
    let hash = M.hash
    (. a) => hash(a)
  }
  let eq = {
    let eq = M.eq
    (. a, b) => eq(a, b)
  }
}

let hashableU = (type key, ~hash, ~eq): module(Hashable with type t = key) =>
  module(
    MakeHashableU({
      type t = key
      let hash = hash
      let eq = eq
    })
  )

let hashable = (type key, ~hash, ~eq) => {
  module N = MakeHashable({
    type t = key
    let hash = hash
    let eq = eq
  })
  module(N: Hashable with type t = key)
}
