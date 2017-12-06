# 2 "set.cppo.ml"
type elt = string
  

# 10
type ('elt, 'id) t0 = ('elt, 'id) Bs_internalAVLset.t0 =
| Empty 
| Node of ('elt, 'id) t0 * 'elt * ('elt, 'id) t0 * int

type ('elt, 'id) enumeration0 = 
  ('elt, 'id) Bs_internalAVLset.enumeration0 
  =
  End 
  | More of 'elt * ('elt, 'id) t0 * ('elt, 'id) enumeration0

 type t = (elt, unit) t0
 type enumeration = (elt,unit) enumeration0

 let empty = Bs_internalAVLset.empty0      
 let isEmpty = Bs_internalAVLset.isEmpty0
 let singleton = Bs_internalAVLset.singleton0
 let min = Bs_internalAVLset.min0
 let max = Bs_internalAVLset.max0
 let iter = Bs_internalAVLset.iter0      
 let fold = Bs_internalAVLset.fold0
 let forAll = Bs_internalAVLset.forAll0
 let exists = Bs_internalAVLset.exists0    
 let filter = Bs_internalAVLset.filter0
 let partition = Bs_internalAVLset.partition0
 let cardinal = Bs_internalAVLset.cardinal0
 let elements = Bs_internalAVLset.elements0 
  
(* Insertion of one element *)

let rec add (x : elt) = function
    Empty -> Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
    if x = v then t else
    if x < v then Bs_internalAVLset.bal (add x l) v r else Bs_internalAVLset.bal l v (add x r)



(* Splitting.  split x s returns a triple (l, present, r) where
    - l is the set of elements of s that are < x
    - r is the set of elements of s that are > x
    - present is false if s contains no element equal to x,
      or true if s contains an element equal to x. *)

let rec split (x : elt) = function
    Empty ->
    (Empty, false, Empty)
  | Node(l, v, r, _) ->    
    if x = v then (l, true, r)
    else if x < v then
      let (ll, pres, rl) = split x l in (ll, pres, Bs_internalAVLset.join rl v r)
    else
      let (lr, pres, rr) = split x r in (Bs_internalAVLset.join l v lr, pres, rr)


let rec mem (x : elt) = function
    Empty -> false
  | Node(l, v, r, _) ->    
    x = v || mem x (if x < v then l else r)

let rec remove (x : elt) = function
    Empty -> Empty
  | Node(l, v, r, _) ->
    if x = v then Bs_internalAVLset.merge l r else
    if x < v then Bs_internalAVLset.bal (remove x l) v r else Bs_internalAVLset.bal l v (remove x r)

let rec union s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
    if h1 >= h2 then
      if h2 = 1 then add v2 s1 else begin
        let (l2, _, r2) = split v1 s2 in
        Bs_internalAVLset.join (union l1 l2) v1 (union r1 r2)
      end
    else
    if h1 = 1 then add v1 s2 else begin
      let (l1, _, r1) = split v2 s1 in
      Bs_internalAVLset.join (union l1 l2) v2 (union r1 r2)
    end

let rec inter s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
    match split v1 t2 with
      (l2, false, r2) ->
      Bs_internalAVLset.concat (inter l1 l2) (inter r1 r2)
    | (l2, true, r2) ->
      Bs_internalAVLset.join (inter l1 l2) v1 (inter r1 r2)

let rec diff s1 s2 =
  match (s1, s2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
    match split v1 t2 with
      (l2, false, r2) ->
      Bs_internalAVLset.join (diff l1 l2) v1 (diff r1 r2)
    | (l2, true, r2) ->
      Bs_internalAVLset.concat (diff l1 l2) (diff r1 r2)


let rec compare_aux e1 e2 =
  match (e1, e2) with
    (End, End) -> 0
  | (End, _)  -> -1
  | (_, End) -> 1
  | (More(v1, r1, e1), More(v2, r2, e2)) ->
    if (v1 : elt) <> v2
    then if v1 < v2 then -1 else 1
    else compare_aux (Bs_internalAVLset.cons_enum r1 e1) (Bs_internalAVLset.cons_enum r2 e2)

let cmp s1 s2 =
  compare_aux (Bs_internalAVLset.cons_enum s1 End) (Bs_internalAVLset.cons_enum s2 End)

let eq s1 s2 =
  cmp s1 s2 = 0

let rec subset s1 s2 =
  match (s1, s2) with
    Empty, _ ->
    true
  | _, Empty ->
    false
  | Node (l1, v1, r1, _), (Node (l2, v2, r2, _) as t2) ->
    if (v1 : elt) = v2 then
      subset l1 l2 && subset r1 r2
    else if v1 < v2 then
      subset (Node (l1, v1, Empty, 0)) l2 && subset r1 t2
    else
      subset (Node (Empty, v1, r1, 0)) r2 && subset l1 t2


let rec find (x : elt) = function
    Empty -> None
  | Node(l, v, r, _) ->
    if x = v then Some v
    else find x (if x < v then l else r)
(*
let of_sorted_list l =
  let rec sub n l =
    match n, l with
    | 0, l -> Empty, l
    | 1, x0 :: l -> Node (Empty, x0, Empty, 1), l
    | 2, x0 :: x1 :: l -> Node (Node(Empty, x0, Empty, 1), x1, Empty, 2), l
    | 3, x0 :: x1 :: x2 :: l ->
      Node (Node(Empty, x0, Empty, 1), x1, Node(Empty, x2, Empty, 1), 2),l
    | n, l ->
      let nl = n / 2 in
      let left, l = sub nl l in
      match l with
      | [] -> assert false
      | mid :: l ->
        let right, l = sub (n - nl - 1) l in
        create left mid right, l
  in
  fst (sub (List.length l) l)
*)  
(*
let of_list l =
  match l with
  | [] -> empty
  | [x0] -> singleton x0
  | [x0; x1] -> add x1 (singleton x0)
  | [x0; x1; x2] -> add x2 (add x1 (singleton x0))
  | [x0; x1; x2; x3] -> add x3 (add x2 (add x1 (singleton x0)))
  | [x0; x1; x2; x3; x4] -> add x4 (add x3 (add x2 (add x1 (singleton x0))))
  | _ -> of_sorted_list (List.sort_uniq Pervasives.compare l)
*)
