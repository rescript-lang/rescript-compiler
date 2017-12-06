# 2 "map.cppo.ml"
type key = string
  
# 9
type ('key, 'a, 'id) t0 = ('key,'a,'id) Bs_internalAVLtree.t0 =
    Empty
  | Node of ('key, 'a, 'id) t0 * 'key * 'a * ('key, 'a, 'id) t0 * int

type 'a t = (key,'a, unit) Bs_internalAVLtree.t0 

type ('key, 'a, 'id) enumeration0 = ('key,'a,'id) Bs_internalAVLtree.enumeration0 =
    End 
   | More of 'key * 'a * ('key, 'a, 'id) t0 * ('key, 'a, 'id) enumeration0

type  'a enumeration = 
  (key,'a, unit) enumeration0



let empty = Bs_internalAVLtree.empty0      
let isEmpty = Bs_internalAVLtree.isEmpty0
let singleton = Bs_internalAVLtree.singleton0
let minBinding = Bs_internalAVLtree.minBinding0
let maxBinding = Bs_internalAVLtree.maxBinding0
let iter = Bs_internalAVLtree.iter0      
let map  = Bs_internalAVLtree.map0
let mapi = Bs_internalAVLtree.mapi0
let fold = Bs_internalAVLtree.fold0
let forAll = Bs_internalAVLtree.forAll0
let exists = Bs_internalAVLtree.exists0    
let filter = Bs_internalAVLtree.filter0
let partition = Bs_internalAVLtree.partition0
let cardinal = Bs_internalAVLtree.cardinal0
let bindings = Bs_internalAVLtree.bindings0  

let rec add (x : key) data = function
    Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    if x = v then
      Node(l, x, data, r, h)
    else if x < v then
      Bs_internalAVLtree.bal (add x data l) v d r
    else
      Bs_internalAVLtree.bal l v d (add x data r)

let rec findOpt (x : key) = function
    Empty ->
    None
  | Node(l, v, d, r, _) ->
    if x = v then Some d
    else findOpt x (if x < v then l else r)

let rec findAssert (x : key) = function
    Empty ->
    [%assert "Not_found"]
  | Node(l, v, d, r, _) ->
    if x = v then d
    else findAssert x (if x < v then l else r)

let rec findWithDefault ~def (x : key) = function
    Empty -> def    
  | Node(l, v, d, r, _) ->
    if x = v then d
    else findWithDefault ~def x (if x < v then l else r)

let rec mem (x : key) = function
    Empty ->
    false
  | Node(l, v, d, r, _) ->
    x = v || mem x (if x < v then l else r)

let rec remove (x : key) = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    if x = v then
      Bs_internalAVLtree.merge l r
    else if x < v then
      Bs_internalAVLtree.bal (remove x l) v d r
    else
      Bs_internalAVLtree.bal l v d (remove x r)



let rec split (x : key) = function
    Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    if x = v then (l, Some d, r)
    else if x < v then
      let (ll, pres, rl) = split x l in (ll, pres, Bs_internalAVLtree.join rl v d r)
    else
      let (lr, pres, rr) = split x r in (Bs_internalAVLtree.join l v d lr, pres, rr)

let rec merge f s1 s2 =
  match (s1, s2) with
    (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= Bs_internalAVLtree.height s2 ->
    let (l2, d2, r2) = split v1 s2 in
    Bs_internalAVLtree.concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2 [@bs]) (merge f r1 r2)
  | (_, Node (l2, v2, d2, r2, h2)) ->
    let (l1, d1, r1) = split v2 s1 in
    Bs_internalAVLtree.concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2) [@bs]) (merge f r1 r2)
  | _ ->
    assert false

let compare cmp m1 m2 =
  let rec compare_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      if (v1 : key) <> v2 then if v1 < v2 then -1 else 1 else
        let c = cmp d1 d2 [@bs] in
        if c <> 0 then c else
          compare_aux (Bs_internalAVLtree.cons_enum r1 e1) (Bs_internalAVLtree.cons_enum r2 e2)
  in compare_aux (Bs_internalAVLtree.cons_enum m1 End) (Bs_internalAVLtree.cons_enum m2 End)

let equal cmp m1 m2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      (v1 : key) = v2  && cmp d1 d2 [@bs] &&
      equal_aux (Bs_internalAVLtree.cons_enum r1 e1) (Bs_internalAVLtree.cons_enum r2 e2)
  in equal_aux (Bs_internalAVLtree.cons_enum m1 End) (Bs_internalAVLtree.cons_enum m2 End)






