(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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


let keepU opt p = match opt with
  | Some x as some when (p x [@bs]) -> some
  | _ -> None

let keep opt p = keepU opt (fun[@bs] x -> p x)

let forEachU opt f = match opt with
  | Some x -> (f x [@bs])
  | None  -> ()

let forEach opt f = forEachU opt (fun[@bs] x -> f x)

let getExn = function
  | Some x -> x
  | None -> raise Not_found

external getUnsafe : 'a option -> 'a = "%identity"  

let mapWithDefaultU opt default f = match opt with
  | Some x -> (f x [@bs])
  | None -> default

let mapWithDefault opt default f = mapWithDefaultU opt default (fun[@bs] x -> f x)

let mapU opt f = match opt with
  | Some x -> Some (f x [@bs])
  | None -> None

let map opt f = mapU opt (fun[@bs] x -> f x)

let flatMapU opt f = match opt with
  | Some x -> (f x [@bs])
  | None -> None

let flatMap opt f = flatMapU opt (fun[@bs] x -> f x)

let getWithDefault opt default = match opt with
  | Some x -> x
  | None -> default

let isSome = function
  | Some _ -> true
  | None -> false

let isNone x = x = None

let eqU a b f = 
  match a with 
  | Some a -> 
    begin match b with 
    | None -> false 
    | Some b -> f a b [@bs]
    end 
  | None -> b = None
  
let eq a b f = eqU a b (fun[@bs] x y -> f x y)

let cmpU a b f = match (a, b) with
  | (Some a, Some b) -> f a b [@bs]
  | (None, Some _) -> -1
  | (Some _, None) -> 1
  | (None, None) -> 0

let cmp a b f = cmpU a b (fun[@bs] x y -> f x y)
