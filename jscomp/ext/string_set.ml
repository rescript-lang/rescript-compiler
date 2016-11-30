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



type elt = string
let compare_elt = String.compare 

type  t = elt Bal_set_common.t 
let empty = Bal_set_common.empty 
let is_empty = Bal_set_common.is_empty
let iter = Bal_set_common.iter
let fold = Bal_set_common.fold
let for_all = Bal_set_common.for_all 
let exists = Bal_set_common.exists 
let singleton = Bal_set_common.singleton 
let cardinal = Bal_set_common.cardinal
let elements = Bal_set_common.elements
let min_elt = Bal_set_common.min_elt
let max_elt = Bal_set_common.max_elt
let choose = Bal_set_common.choose 
let of_sorted_list = Bal_set_common.of_sorted_list
let of_sorted_array = Bal_set_common.of_sorted_array
let partition = Bal_set_common.partition 
let filter = Bal_set_common.filter 
let of_sorted_list = Bal_set_common.of_sorted_list
let of_sorted_array = Bal_set_common.of_sorted_array

let rec add x (tree : _ Bal_set_common.t) : _ Bal_set_common.t =
  match tree with  
  | Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
    let c = compare_elt x v in
    if c = 0 then t else
    if c < 0 then Bal_set_common.internal_bal (add x l) v r else Bal_set_common.internal_bal l v (add x r) 


let rec mem x (tree : _ Bal_set_common.t) = 
  match tree with 
  | Empty -> false
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    c = 0 || mem x (if c < 0 then l else r)

let of_list l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add x1 (singleton x0)
  | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
  | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
  | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
  | _ -> of_sorted_list (List.sort_uniq compare_elt l)

let rec find x (tree : t) = 
  match tree with 
  | Empty -> raise Not_found
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then v
    else find x (if c < 0 then l else r)






