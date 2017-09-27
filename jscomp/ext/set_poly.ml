# 1 "ext/set.cppo.ml"
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


# 59
type 'a t = 'a Set_gen.t
let compare_elt = Pervasives.compare
let print_elt = Ext_pervasives.pp_any


# 67
let empty = Set_gen.empty 
let is_empty = Set_gen.is_empty
let iter = Set_gen.iter
let fold = Set_gen.fold
let for_all = Set_gen.for_all 
let exists = Set_gen.exists 
let singleton = Set_gen.singleton 
let cardinal = Set_gen.cardinal
let elements = Set_gen.elements
let min_elt = Set_gen.min_elt
let max_elt = Set_gen.max_elt
let choose = Set_gen.choose 
let of_sorted_list = Set_gen.of_sorted_list
let of_sorted_array = Set_gen.of_sorted_array
let partition = Set_gen.partition 
let filter = Set_gen.filter 
let of_sorted_list = Set_gen.of_sorted_list
let of_sorted_array = Set_gen.of_sorted_array

let rec split x (tree : _ Set_gen.t) : _ Set_gen.t * bool * _ Set_gen.t =  match tree with 
  | Empty ->
    (Empty, false, Empty)
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then (l, true, r)
    else if c < 0 then
      let (ll, pres, rl) = split x l in (ll, pres, Set_gen.internal_join rl v r)
    else
      let (lr, pres, rr) = split x r in (Set_gen.internal_join l v lr, pres, rr)
let rec add x (tree : _ Set_gen.t) : _ Set_gen.t =  match tree with 
  | Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
    let c = compare_elt x v in
    if c = 0 then t else
    if c < 0 then Set_gen.internal_bal (add x l) v r else Set_gen.internal_bal l v (add x r)

let rec union (s1 : _ Set_gen.t) (s2 : _ Set_gen.t) : _ Set_gen.t  =
  match (s1, s2) with
  | (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
    if h1 >= h2 then
      if h2 = 1 then add v2 s1 else begin
        let (l2, _, r2) = split v1 s2 in
        Set_gen.internal_join (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add v1 s2 else begin
      let (l1, _, r1) = split v2 s1 in
      Set_gen.internal_join (union l1 l2) v2 (union r1 r2)
    end    

let rec inter (s1 : _ Set_gen.t)  (s2 : _ Set_gen.t) : _ Set_gen.t  =
  match (s1, s2) with
  | (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
    begin match split v1 t2 with
      | (l2, false, r2) ->
        Set_gen.internal_concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
        Set_gen.internal_join (inter l1 l2) v1 (inter r1 r2)
    end 

let rec diff (s1 : _ Set_gen.t) (s2 : _ Set_gen.t) : _ Set_gen.t  =
  match (s1, s2) with
  | (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
    begin match split v1 t2 with
      | (l2, false, r2) ->
        Set_gen.internal_join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
        Set_gen.internal_concat (diff l1 l2) (diff r1 r2)    
    end


let rec mem x (tree : _ Set_gen.t) =  match tree with 
  | Empty -> false
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    c = 0 || mem x (if c < 0 then l else r)

let rec remove x (tree : _ Set_gen.t) : _ Set_gen.t = match tree with 
  | Empty -> Empty
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then Set_gen.internal_merge l r else
    if c < 0 then Set_gen.internal_bal (remove x l) v r else Set_gen.internal_bal l v (remove x r)

let compare s1 s2 = Set_gen.compare compare_elt s1 s2 


let equal s1 s2 =
  compare s1 s2 = 0

let rec subset (s1 : _ Set_gen.t) (s2 : _ Set_gen.t) =
  match (s1, s2) with
  | Empty, _ ->
    true
  | _, Empty ->
    false
  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
    let c = compare_elt v1 v2 in
    if c = 0 then
      subset l1 l2 && subset r1 r2
    else if c < 0 then
      subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
    else
      subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2




let rec find x (tree : _ Set_gen.t) = match tree with
  | Empty -> raise Not_found
  | Node(l, v, r, _) ->
    let c = compare_elt x v in
    if c = 0 then v
    else find x (if c < 0 then l else r)



let of_list l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add x1 (singleton x0)
  | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
  | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
  | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
  | _ -> of_sorted_list (List.sort_uniq compare_elt l)

let of_array l = 
  Array.fold_left (fun  acc x -> add x acc) empty l

(* also check order *)
let invariant t =
  Set_gen.check t ;
  Set_gen.is_ordered compare_elt t          

let print fmt s = 
  Format.fprintf 
   fmt   "@[<v>{%a}@]@."
    (fun fmt s   -> 
       iter 
         (fun e -> Format.fprintf fmt "@[<v>%a@],@ " 
         print_elt e) s
    )
    s     




