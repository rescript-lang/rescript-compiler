
(* Copyright (C) 2017 Authors of BuckleScript
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)


type ('a, 'id) hash = ('a -> int [@bs])
type ('a, 'id) eq = ('a -> 'a -> bool [@bs])
type ('a, 'id) cmp = ('a -> 'a -> int [@bs])

external getHashInternal : ('a,'id) hash -> ('a -> int [@bs]) = "%identity"
external getEqInternal : ('a, 'id) eq -> ('a -> 'a -> bool [@bs]) = "%identity"
external getCmpInternal : ('a,'id) cmp -> ('a -> 'a -> int [@bs]) = "%identity"


module type Comparable = sig
  type identity
  type t
  val cmp: (t, identity) cmp
end

type ('key, 'id) comparable =
  (module Comparable with type t = 'key and type identity = 'id)


module MakeComparableU (M : sig
   type t
   val cmp: t -> t -> int [@bs]
  end) =
struct
  type identity
  type t = M.t
  let cmp = M.cmp
end

module MakeComparable (M : sig
   type t
   val cmp: t -> t -> int
  end) =
struct
  type identity
  type t = M.t
  (* see https://github.com/BuckleScript/bucklescript/pull/2589/files/5ef875b7665ee08cfdc59af368fc52bac1fe9130#r173330825 *)
  let cmp =
    let cmp = M.cmp in fun[@bs] a b -> cmp a b
end

let comparableU
  (type key)
  ~cmp
  =
  let module N = MakeComparableU(struct
      type t = key
      let cmp = cmp
    end) in
  (module N : Comparable with type t = key)

let comparable
  (type key)
  ~cmp
  =
  let module N = MakeComparable(struct
      type t = key
      let cmp = cmp
    end) in
  (module N : Comparable with type t = key)

module type Hashable = sig
  type identity
  type t
  val hash: (t,identity) hash
  val eq:  (t,identity) eq
end

type ('key, 'id) hashable = (module Hashable with type t = 'key and type identity = 'id)

module MakeHashableU (M : sig
   type t
   val hash : t -> int [@bs]
   val eq : t -> t -> bool [@bs]
  end) =
struct
  type identity
  type t = M.t
  let hash = M.hash
  let eq = M.eq
end

module MakeHashable (M : sig
   type t
   val hash : t -> int
   val eq : t -> t -> bool
  end) =
struct
  type identity
  type t = M.t
  let hash =
    let hash = M.hash in fun[@bs] a -> hash a
  let eq =
    let eq = M.eq in fun[@bs] a b -> eq a b
end

let hashableU (type key) ~hash ~eq =
  let module N = MakeHashableU(struct
    type t = key
    let hash = hash
    let eq = eq
  end) in
  (module N : Hashable with type t = key)

let hashable (type key) ~hash ~eq =
  let module N = MakeHashable(struct
    type t = key
    let hash = hash
    let eq = eq
  end) in
  (module N : Hashable with type t = key)
