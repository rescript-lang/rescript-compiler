(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)
(** balanced tree based on stdlib distribution *)

type 'a t  = 'a Bal_set_common.t 
open Bal_set_common


(* Splitting.  
    split x s returns a triple (l, present, r) where
        - l is the set of elements of s that are < x
        - r is the set of elements of s that are > x
        - present is false if s contains no element equal to x,
          or true if s contains an element equal to x.
*)

let rec split x (tree : _ t ) : _ t  *  bool * _ t =
  match tree with 
  | Empty ->
    (Empty, false, Empty)
  | Node(l, v, r, _) ->
    let c = Pervasives.compare x v in
    if c = 0 then (l, true, r)
    else if c < 0 then
      let (ll, pres, rl) = split x l in (ll, pres, internal_join rl v r)
    else
      let (lr, pres, rr) = split x r in (internal_join l v lr, pres, rr)

let rec add x (tree : _ t) : _ t  =
  match tree with 
  | Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
    let c = Pervasives.compare x v in
    if c = 0 then t else
    if c < 0 then internal_bal (add x l) v r else internal_bal l v (add x r)

let rec union (s1 : _ t) (s2 : _ t) =
  match (s1, s2) with
  | (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
    if h1 >= h2 then
      if h2 = 1 then add v2 s1 else begin
        let (l2, _, r2) = split v1 s2 in
        internal_join (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add v1 s2 else begin
      let (l1, _, r1) = split v2 s1 in
      internal_join (union l1 l2) v2 (union r1 r2)
    end    

let rec inter s1 s2 =
  match (s1, s2) with
  | (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
    begin match split v1 t2 with
      | (l2, false, r2) ->
        internal_concat (inter l1 l2) (inter r1 r2)
      | (l2, true, r2) ->
        internal_join (inter l1 l2) v1 (inter r1 r2)
    end 

let rec diff s1 s2 =
  match (s1, s2) with
  | (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
    begin match split v1 t2 with
      | (l2, false, r2) ->
        internal_join (diff l1 l2) v1 (diff r1 r2)
      | (l2, true, r2) ->
        internal_concat (diff l1 l2) (diff r1 r2)    
    end


let rec mem x (tree : _ t) =
  match tree with 
  | Empty -> false
  | Node(l, v, r, _) ->
    let c = Pervasives.compare x v in
    c = 0 || mem x (if c < 0 then l else r)

let rec remove x = function
    Empty -> Empty
  | Node(l, v, r, _) ->
    let c = Pervasives.compare x v in
    if c = 0 then internal_merge l r else
    if c < 0 then internal_bal (remove x l) v r else internal_bal l v (remove x r)


let rec subset (s1 : _ t) (s2 : _ t) =
  match (s1, s2) with
  | Empty, _ -> true
  | _, Empty -> false
  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
    let c = Pervasives.compare v1 v2 in
    if c = 0 then
      subset l1 l2 && subset r1 r2
    else if c < 0 then
      subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
    else
      subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2

let compare s1 s2 = Bal_set_common.compare Pervasives.compare s1 s2 

let equal s1 s2 = compare s1 s2 = 0

let rec find x = function
    Empty -> raise Not_found
  | Node(l, v, r, _) ->
    let c = Pervasives.compare x v in
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
  | _ -> of_sorted_list (List.sort_uniq Pervasives.compare l)

let of_array l = 
  Array.fold_left (fun  acc x -> add x acc) empty l




let invariant t =
  Bal_set_common.invariant Pervasives.compare t 

module Make ( S : Set.OrderedType) = struct
  type elt = S.t
  type nonrec t = elt t 
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
  let rec split x = function
    | Empty ->
      (Empty, false, Empty)
    | Node(l, v, r, _) ->
      let c = S.compare x v in
      if c = 0 then (l, true, r)
      else if c < 0 then
        let (ll, pres, rl) = split x l in (ll, pres, internal_join rl v r)
      else
        let (lr, pres, rr) = split x r in (internal_join l v lr, pres, rr)
  let rec add x = function
      Empty -> Node(Empty, x, Empty, 1)
    | Node(l, v, r, _) as t ->
      let c = S.compare x v in
      if c = 0 then t else
      if c < 0 then internal_bal (add x l) v r else internal_bal l v (add x r)

  let rec union s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      if h1 >= h2 then
        if h2 = 1 then add v2 s1 else begin
          let (l2, _, r2) = split v1 s2 in
          internal_join (union l1 l2) v1 (union r1 r2)
        end
      else
      if h1 = 1 then add v1 s2 else begin
        let (l1, _, r1) = split v2 s1 in
        internal_join (union l1 l2) v2 (union r1 r2)
      end    

  let rec inter s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> Empty
    | (t1, Empty) -> Empty
    | (Node(l1, v1, r1, _), t2) ->
      begin match split v1 t2 with
        | (l2, false, r2) ->
          internal_concat (inter l1 l2) (inter r1 r2)
        | (l2, true, r2) ->
          internal_join (inter l1 l2) v1 (inter r1 r2)
      end 

  let rec diff s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> Empty
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, _), t2) ->
      begin match split v1 t2 with
        | (l2, false, r2) ->
          internal_join (diff l1 l2) v1 (diff r1 r2)
        | (l2, true, r2) ->
          internal_concat (diff l1 l2) (diff r1 r2)    
      end


  let rec mem x = function
      Empty -> false
    | Node(l, v, r, _) ->
      let c = S.compare x v in
      c = 0 || mem x (if c < 0 then l else r)

  let rec remove x = function
      Empty -> Empty
    | Node(l, v, r, _) ->
      let c = S.compare x v in
      if c = 0 then internal_merge l r else
      if c < 0 then internal_bal (remove x l) v r else internal_bal l v (remove x r)

  let compare s1 s2 = Bal_set_common.compare Pervasives.compare s1 s2 


  let equal s1 s2 =
    compare s1 s2 = 0

  let rec subset s1 s2 =
    match (s1, s2) with
      Empty, _ ->
      true
    | _, Empty ->
      false
    | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
      let c = S.compare v1 v2 in
      if c = 0 then
        subset l1 l2 && subset r1 r2
      else if c < 0 then
        subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
      else
        subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2




  let rec find x = function
      Empty -> raise Not_found
    | Node(l, v, r, _) ->
      let c = S.compare x v in
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
    | _ -> of_sorted_list (List.sort_uniq S.compare l)

  let of_array l = 
    Array.fold_left (fun  acc x -> add x acc) empty l

  (* also check order *)
  let invariant t =
    check t ;
    is_ordered S.compare t          
end 
