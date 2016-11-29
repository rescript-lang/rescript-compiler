
type ('key,'value) t =  ('key,'value) Bal_map_common.t 
open Bal_map_common
(**********************************************************************)

let rec add x data (tree : _ t) : _ t =
  match tree with 
  | Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = Pervasives.compare x v in
    if c = 0 then
      Node(l, x, data, r, h)
    else if c < 0 then
      bal (add x data l) v d r
    else
      bal l v d (add x data r)

let rec find x (tree : _ t) =
  match tree with 
  | Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = Pervasives.compare x v in
    if c = 0 then d
    else find x (if c < 0 then l else r)

let rec mem x  (tree : _ t) =
  match tree with 
  | Empty ->
    false
  | Node(l, v, d, r, _) ->
    let c = Pervasives.compare x v in
    c = 0 || mem x (if c < 0 then l else r)

let rec remove x (tree : _ t) : _ t =
  match tree with 
  | Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let c = Pervasives.compare x v in
    if c = 0 then
      merge l r
    else if c < 0 then
      bal (remove x l) v d r
    else
      bal l v d (remove x r)

let rec split x (tree : _ t) : _ t * _ option * _ t  =
  match tree with 
  | Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    let c = Pervasives.compare x v in
    if c = 0 then (l, Some d, r)
    else if c < 0 then
      let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
    else
      let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

let rec merge f (s1 : _ t) (s2 : _ t) : _ t =
  match (s1, s2) with
  | (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    let (l2, d2, r2) = split v1 s2 in
    concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
  | (_, Node (l2, v2, d2, r2, h2)) ->
    let (l1, d1, r1) = split v2 s1 in
    concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
  | _ ->
    assert false


let compare cmp m1 m2 =
  let rec compare_aux e1 e2 =
    match (e1, e2) with
    | (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      let c = Pervasives.compare v1 v2 in
      if c <> 0 then c else
        let c = cmp d1 d2 in
        if c <> 0 then c else
          compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in compare_aux (cons_enum m1 End) (cons_enum m2 End)

let equal cmp m1 m2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      Pervasives.compare v1 v2 = 0 && cmp d1 d2 &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in equal_aux (cons_enum m1 End) (cons_enum m2 End)

