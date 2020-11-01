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

type 'a t  =
  | NotAsked
  | Loading
  | Done 'a

let getExn = function
  | Done x -> x
  | NotAsked | Loading -> raise Not_found

let mapWithDefaultU opt default f = match opt with
  | Done x -> (f x [@bs])
  | Loading | NotAsked -> default

let mapWithDefault opt default f = mapWithDefaultU opt default (fun[@bs] x -> f x)

let mapU opt f = match opt with
  | Done x -> Done (f x [@bs])
  | Loading -> Loading
  | NotAsked -> NotAsked

let map opt f = mapU opt (fun[@bs] x -> f x)

let flatMapU opt f = match opt with
  | Done x -> (f x [@bs])
  | Loading -> Loading
  | NotAsked -> NotAsked

let flatMap opt f = flatMapU opt (fun[@bs] x -> f x)

let getWithDefault opt default = match opt with
  | Done x -> x
  | Loading | NotAsked -> default

let isLoading = function
  | Loading -> true
  | Done _ | NotAsked -> false

let isDone = function
  | Done _ -> true
  | NotAsked | Loading -> false

let isNotAsked = function
  | NotAsked -> true
  | Loading | Done _ -> false

let eqU a b f = match (a, b) with
  | (Done a, Done b) -> f a b [@bs]
  | (NotAsked, Done _)
  | (NotAsked, Loading)
  | (Loading, NotAsked)
  | (Loading, Done _)
  | (Done _, NotAsked)
  | (Done _, Loading) -> false
  | (Loading, Loading)
  | (NotAsked, NotAsked) -> true

let eq a b f = eqU a b (fun[@bs] x y -> f x y)

let cmpU a b f = match (a, b) with
  | (Done a, Done b) -> f a b [@bs]
  | (NotAsked, Done _) -> -1
  | (NotAsked, Loading) -> -1
  | (Loading, NotAsked) -> 1
  | (Loading, Done _) -> -1
  | (Done _, NotAsked) -> 1
  | (Done _, Loading) -> 1
  | (Loading, Loading)
  | (NotAsked, NotAsked) -> 0

let cmp a b f = cmpU a b (fun[@bs] x y -> f x y)
