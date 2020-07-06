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


#if defined TYPE_STRING 
type elt = string
let compare_elt = Ext_string.compare 
let [@inline] eq_elt (x : elt) y = x = y
let print_elt = Format.pp_print_string
#elif defined TYPE_IDENT
type elt = Ident.t
let compare_elt (x : elt) (y : elt) = 
  let a =  Pervasives.compare (x.stamp : int) y.stamp in 
  if a <> 0 then a 
  else 
    let b = Pervasives.compare (x.name : string) y.name in 
    if b <> 0 then b 
    else Pervasives.compare (x.flags : int) y.flags     
let [@inline] eq_elt (x : elt) y = Ident.same x y
let print_elt = Ident.print
#elif defined TYPE_INT
type elt = int 
let compare_elt = Ext_int.compare 
let print_elt = Format.pp_print_int
let [@inline] eq_elt (x : elt) y = x = y
#else 
[%error "unknown type" ]
#endif

type ('a ) t0 = 'a Set_gen.t 

type  t = elt t0

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

let choose = Set_gen.choose 

let of_sorted_array = Set_gen.of_sorted_array

let rec mem (tree : t) (x : elt) =  match tree with 
  | Empty -> false
  | Leaf v -> eq_elt x  v 
  | Node{l; v; r} ->
    let c = compare_elt x v in
    c = 0 || mem (if c < 0 then l else r) x

let rec split (tree : t) x : t * bool * t =  match tree with 
  | Empty ->
    (empty, false, empty)
  | Leaf v ->   
    let c = compare_elt x v in
    if c = 0 then (empty, true, empty)
    else if c < 0 then
      (empty, false, tree)
    else
      (tree, false, empty)
  | Node {l; v; r} ->
    let c = compare_elt x v in
    if c = 0 then (l, true, r)
    else if c < 0 then
      let (ll, pres, rl) = split l x in (ll, pres, Set_gen.internal_join rl v r)
    else
      let (lr, pres, rr) = split r x in (Set_gen.internal_join l v lr, pres, rr)

let rec add (tree : t) x : t =  match tree with 
  | Empty -> singleton x
  | Leaf v -> 
    let c = compare_elt x v in
    if c = 0 then tree else     
    if c < 0 then 
      Set_gen.unsafe_two_elements x v
    else 
      Set_gen.unsafe_two_elements v x 
  | Node {l; v; r} as t ->
    let c = compare_elt x v in
    if c = 0 then t else
    if c < 0 then Set_gen.internal_bal (add l x ) v r else Set_gen.internal_bal l v (add r x )

let rec union (s1 : t) (s2 : t) : t  =
  match (s1, s2) with
  | (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | Node _, Leaf v2 ->
    add s1 v2 
  | Leaf v1, Node _ -> 
    add s2 v1 
  | Leaf x, Leaf v -> 
    let c = compare_elt x v in
    if c = 0 then s1 else     
    if c < 0 then 
      Set_gen.unsafe_two_elements x v
    else 
      Set_gen.unsafe_two_elements v x
  | Node{l=l1; v=v1; r=r1; h=h1}, Node{l=l2; v=v2; r=r2; h=h2} ->
    if h1 >= h2 then
      if h2 = 1 then add s1 v2 else begin
        let (l2, _, r2) = split s2 v1 in
        Set_gen.internal_join (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add s2 v1 else begin
      let (l1, _, r1) = split s1 v2 in
      Set_gen.internal_join (union l1 l2) v2 (union r1 r2)
    end    

let rec inter (s1 : t)  (s2 : t) : t  =
  match (s1, s2) with
  | (Empty, _) -> empty
  | (_, Empty) -> empty  
  | Leaf v, t2 -> 
    if mem t2 v then s1 else empty
  | (Node{l=l1; v=v1; r=r1}, t2) ->
    begin match split t2 v1 with
      | (l2, false, r2) ->
        Set_gen.internal_concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
        Set_gen.internal_join (inter l1 l2) v1 (inter r1 r2)
    end 

let rec diff (s1 : t) (s2 : t) : t  =
  match (s1, s2) with
  | (Empty, _) -> empty
  | (t1, Empty) -> t1
  | Leaf v, t2 -> 
    if mem t2 v then empty else s1 
  | (Node{l=l1; v=v1; r=r1}, t2) ->
    begin match split t2 v1 with
      | (l2, false, r2) ->
        Set_gen.internal_join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
        Set_gen.internal_concat (diff l1 l2) (diff r1 r2)    
    end




let rec remove (tree : t)  (x : elt) : t = match tree with 
  | Empty -> empty (* This case actually would be never reached *)
  | Leaf v ->     
    if eq_elt x  v then empty else tree    
  | Node{l; v; r} ->
    let c = compare_elt x v in
    if c = 0 then Set_gen.internal_merge l r else
    if c < 0 then Set_gen.internal_bal (remove l x) v r else Set_gen.internal_bal l v (remove r x )

(* let compare s1 s2 = Set_gen.compare ~cmp:compare_elt s1 s2  *)



let of_list l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add (singleton x0) x1 
  | [x0; x1; x2] -> add (add (singleton x0)  x1) x2 
  | [x0; x1; x2; x3] -> add (add (add (singleton x0) x1 ) x2 ) x3 
  | [x0; x1; x2; x3; x4] -> add (add (add (add (singleton x0) x1) x2 ) x3 ) x4 
  | _ -> 
    let arrs = Array.of_list l in 
    Array.sort compare_elt arrs ; 
    of_sorted_array arrs



(* also check order *)
let invariant t =
  Set_gen.check t ;
  Set_gen.is_ordered ~cmp:compare_elt t          

let print fmt s = 
  Format.fprintf 
   fmt   "@[<v>{%a}@]@."
    (fun fmt s   -> 
       iter s
         (fun e -> Format.fprintf fmt "@[<v>%a@],@ " 
         print_elt e) 
    )
    s     





