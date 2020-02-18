(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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

type 'a t = 'a option

let some x = Some x 

let isSome = function
  | None -> false
  | Some _ -> true

let isSomeValue eq v x =
  match x with 
  | None -> false 
  | Some x -> eq v x [@bs]


let isNone = function
  | None -> true
  | Some _ -> false

let getExn x =
  match x with 
  | None -> Js_exn.raiseError "getExn"
  | Some x -> x 

let equal eq a b =
  match a  with 
  | None -> b = None 
  | Some x -> 
    begin match b with 
    | None -> false 
    | Some y -> eq x y [@bs]
    end

let andThen f x =
  match x with 
  | None -> None 
  | Some x -> f x [@bs]

let map f x =
  match x with
  | None -> None
  | Some x -> Some (f x [@bs])

let getWithDefault a x =
  match x with
  | None -> a
  | Some x -> x

let default = getWithDefault  

let filter f x =
  match x with
  | None -> None
  | Some x -> 
    if f x [@bs] then
      Some x
    else
      None

let firstSome a b =
  match (a, b) with
  | (Some _, _) -> a
  | (None, Some _) -> b
  | (None, None) -> None
