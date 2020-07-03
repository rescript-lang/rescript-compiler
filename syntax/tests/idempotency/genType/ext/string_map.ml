
(* we don't create [map_poly], since some operations require raise an exception which carries [key] *)


  
  type key = string 
  let compare_key = Ext_string.compare

type 'a t = (key,'a) Map_gen.t
exception Duplicate_key of key 

let empty = Map_gen.empty 
let is_empty = Map_gen.is_empty
let iter = Map_gen.iter
let fold = Map_gen.fold
let for_all = Map_gen.for_all 
let exists = Map_gen.exists 
let singleton = Map_gen.singleton 
let cardinal = Map_gen.cardinal
let bindings = Map_gen.bindings
let keys = Map_gen.keys
let choose = Map_gen.choose 
let partition = Map_gen.partition 
let filter = Map_gen.filter 
let map = Map_gen.map 
let mapi = Map_gen.mapi
let bal = Map_gen.bal 
let height = Map_gen.height 
let max_binding_exn = Map_gen.max_binding_exn
let min_binding_exn = Map_gen.min_binding_exn


let rec add x data (tree : _ Map_gen.t as 'a) : 'a = match tree with 
  | Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, data, r, h)
    else if c < 0 then
      bal (add x data l) v d r
    else
      bal l v d (add x data r)


let rec adjust x data replace (tree : _ Map_gen.t as 'a) : 'a = 
  match tree with 
  | Empty ->
    Node(Empty, x, data (), Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = compare_key x v in
    if c = 0 then
      Node(l, x, replace  d , r, h)
    else if c < 0 then
      bal (adjust x data replace l) v d r
    else
      bal l v d (adjust x data replace r)


let rec find_exn x (tree : _ Map_gen.t )  = match tree with 
  | Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then d
    else find_exn x (if c < 0 then l else r)

let rec find_opt x (tree : _ Map_gen.t )  = match tree with 
  | Empty -> None 
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then Some d
    else find_opt x (if c < 0 then l else r)

let rec find_default x (tree : _ Map_gen.t ) default     = match tree with 
  | Empty -> default  
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then  d
    else find_default x   (if c < 0 then l else r) default

let rec mem x (tree : _ Map_gen.t )   = match tree with 
  | Empty ->
    false
  | Node(l, v, _d, r, _) ->
    let c = compare_key x v in
    c = 0 || mem x (if c < 0 then l else r)

let rec remove x (tree : _ Map_gen.t as 'a) : 'a = match tree with 
  | Empty ->
    Empty
  | Node(l, v, d, r, _h) ->
    let c = compare_key x v in
    if c = 0 then
      Map_gen.merge l r
    else if c < 0 then
      bal (remove x l) v d r
    else
      bal l v d (remove x r)


let rec split x (tree : _ Map_gen.t as 'a) : 'a * _ option * 'a  = match tree with 
  | Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    let c = compare_key x v in
    if c = 0 then (l, Some d, r)
    else if c < 0 then
      let (ll, pres, rl) = split x l in (ll, pres, Map_gen.join rl v d r)
    else
      let (lr, pres, rr) = split x r in (Map_gen.join l v d lr, pres, rr)

let rec merge f (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    let (l2, d2, r2) = split v1 s2 in
    Map_gen.concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
  | (_, Node (l2, v2, d2, r2, _h2)) ->
    let (l1, d1, r1) = split v2 s1 in
    Map_gen.concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
  | _ ->
    assert false

let rec disjoint_merge  (s1 : _ Map_gen.t) (s2  : _ Map_gen.t) : _ Map_gen.t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    begin match split v1 s2 with 
    | l2, None, r2 -> 
      Map_gen.join (disjoint_merge  l1 l2) v1 d1 (disjoint_merge r1 r2)
    | _, Some _, _ ->
      raise (Duplicate_key  v1)
    end        
  | (_, Node (l2, v2, d2, r2, _h2)) ->
    begin match  split v2 s1 with 
    | (l1, None, r1) -> 
      Map_gen.join (disjoint_merge  l1 l2) v2 d2 (disjoint_merge  r1 r2)
    | (_, Some _, _) -> 
      raise (Duplicate_key v2)
    end
  | _ ->
    assert false



let compare cmp m1 m2 = Map_gen.compare compare_key cmp m1 m2

let equal cmp m1 m2 = Map_gen.equal compare_key cmp m1 m2 

let add_list (xs : _ list ) init = 
  List.fold_left (fun acc (k,v) -> add k v acc) init xs 

let of_list xs = add_list xs empty

let of_array xs = 
  Array.fold_left (fun acc (k,v) -> add k v acc) empty xs
