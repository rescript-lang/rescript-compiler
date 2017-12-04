
type ('key, 'a, 'id) t0 =
    Empty
  | Node of ('key, 'a, 'id) t0 * 'key * 'a * ('key, 'a, 'id) t0 * int

let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let singleton0 x d = Node(Empty, x, d, Empty, 1)

let bal l x d r =
  let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
  let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
  if hl > hr + 2 then begin
    match l with
      Empty -> invalid_arg "Map.bal"
    | Node(ll, lv, ld, lr, _) ->
      if height ll >= height lr then
        create ll lv ld (create lr x d r)
      else begin
        match lr with
          Empty -> invalid_arg "Map.bal"
        | Node(lrl, lrv, lrd, lrr, _)->
          create (create ll lv ld lrl) lrv lrd (create lrr x d r)
      end
  end else if hr > hl + 2 then begin
    match r with
      Empty -> invalid_arg "Map.bal"
    | Node(rl, rv, rd, rr, _) ->
      if height rr >= height rl then
        create (create l x d rl) rv rd rr
      else begin
        match rl with
          Empty -> invalid_arg "Map.bal"
        | Node(rll, rlv, rld, rlr, _) ->
          create (create l x d rll) rlv rld (create rlr rv rd rr)
      end
  end else
    Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let empty0 = Empty

let is_empty0 = function Empty -> true | _ -> false

let rec add0 ~cmp x data = function
    Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then
      Node(l, x, data, r, h)
    else if c < 0 then
      bal (add0 ~cmp x data l) v d r
    else
      bal l v d (add0 ~cmp x data r)

let rec find0 ~cmp x = function
    Empty ->
    raise Not_found
  | Node(l, v, d, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then d
    else find0 ~cmp x (if c < 0 then l else r)

let rec mem0 ~cmp x = function
    Empty ->
    false
  | Node(l, v, d, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp x (if c < 0 then l else r)

let rec min_binding0 = function
    Empty -> raise Not_found
  | Node(Empty, x, d, r, _) -> (x, d)
  | Node(l, x, d, r, _) -> min_binding0 l

let rec max_binding0 = function
    Empty -> raise Not_found
  | Node(l, x, d, Empty, _) -> (x, d)
  | Node(l, x, d, r, _) -> max_binding0 r

let rec remove_min_binding = function
    Empty -> invalid_arg "Map.remove_min_elt"
  | Node(Empty, x, d, r, _) -> r
  | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

let merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding0 t2 in
    bal t1 x d (remove_min_binding t2)

let rec remove0 ~cmp x = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then
      merge l r
    else if c < 0 then
      bal (remove0 ~cmp x l) v d r
    else
      bal l v d (remove0 ~cmp x r)

let rec iter0 f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
    iter0 f l; f v d [@bs]; iter0 f r

let rec map0 f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = map0 f l in
    let d' = f d [@bs] in
    let r' = map0 f r in
    Node(l', v, d', r', h)

let rec mapi0 f = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let l' = mapi0 f l in
    let d' = f v d [@bs] in
    let r' = mapi0 f r in
    Node(l', v, d', r', h)

let rec fold0 f m accu =
  match m with
    Empty -> accu
  | Node(l, v, d, r, _) ->
    fold0 f r (f v d (fold0 f l accu) [@bs])

let rec for_all0 p = function
    Empty -> true
  | Node(l, v, d, r, _) -> p v d [@bs] && for_all0 p l && for_all0 p r

let rec exists0 p = function
    Empty -> false
  | Node(l, v, d, r, _) -> p v d [@bs] || exists0 p l || exists0 p r

(* Beware: those two functions assume that the added k is *strictly*
   smaller (or bigger) than all the present keys in the tree; it
   does not test for equality with the current min (or max) key.

   Indeed, they are only used during the "join" operation which
   respects this precondition.
*)

let rec add_min_binding k v = function
  | Empty -> singleton0 k v
  | Node (l, x, d, r, h) ->
    bal (add_min_binding k v l) x d r

let rec add_max_binding k v = function
  | Empty -> singleton0 k v
  | Node (l, x, d, r, h) ->
    bal l x d (add_max_binding k v r)

(* Same as create and bal, but no assumptions are made on the
   relative heights of l and r. *)

let rec join l v d r =
  match (l, r) with
    (Empty, _) -> add_min_binding v d r
  | (_, Empty) -> add_max_binding v d l
  | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
    if lh > rh + 2 then bal ll lv ld (join lr v d r) else
    if rh > lh + 2 then bal (join l v d rl) rv rd rr else
      create l v d r

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   No assumption on the heights of l and r. *)

let concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (_, _) ->
    let (x, d) = min_binding0 t2 in
    join t1 x d (remove_min_binding t2)

let concat_or_join t1 v d t2 =
  match d with
  | Some d -> join t1 v d t2
  | None -> concat t1 t2

let rec split0 ~cmp x = function
    Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then (l, Some d, r)
    else if c < 0 then
      let (ll, pres, rl) = split0 ~cmp x l in (ll, pres, join rl v d r)
    else
      let (lr, pres, rr) = split0 ~cmp x r in (join l v d lr, pres, rr)

let rec merge0 ~cmp f s1 s2 =
  match (s1, s2) with
    (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
    let (l2, d2, r2) = split0 ~cmp v1 s2 in
    concat_or_join (merge0 ~cmp f l1 l2) v1 (f v1 (Some d1) d2 [@bs]) (merge0 ~cmp f r1 r2)
  | (_, Node (l2, v2, d2, r2, h2)) ->
    let (l1, d1, r1) = split0 ~cmp v2 s1 in
    concat_or_join (merge0 ~cmp f l1 l2) v2 (f v2 d1 (Some d2) [@bs]) (merge0 ~cmp f r1 r2)
  | _ ->
    assert false

let rec filter0 p = function
    Empty -> Empty
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let l' = filter0 p l in
    let pvd = p v d [@bs] in
    let r' = filter0 p r in
    if pvd then join l' v d r' else concat l' r'

let rec partition0 p = function
    Empty -> (Empty, Empty)
  | Node(l, v, d, r, _) ->
    (* call [p] in the expected left-to-right order *)
    let (lt, lf) = partition0 p l in
    let pvd = p v d [@bs] in
    let (rt, rf) = partition0 p r in
    if pvd
    then (join lt v d rt, concat lf rf)
    else (concat lt rt, join lf v d rf)

type ('key, 'a, 'id) enumeration = End | More of 'key * 'a * ('key, 'a, 'id) t0 * ('key, 'a, 'id) enumeration

let rec cons_enum m e =
  match m with
    Empty -> e
  | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

let compare0 ~cmp:keycmp cmp m1 m2 =
  let rec compare_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> 0
    | (End, _)  -> -1
    | (_, End) -> 1
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      let c = (Bs_Cmp.getCmp keycmp) v1 v2 [@bs] in
      if c <> 0 then c else
        let c = cmp d1 d2 [@bs] in
        if c <> 0 then c else
          compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in compare_aux (cons_enum m1 End) (cons_enum m2 End)

let equal0 ~cmp:keycmp cmp m1 m2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      (Bs_Cmp.getCmp keycmp) v1 v2 [@bs] = 0   && cmp d1 d2 [@bs] &&
      equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
  in equal_aux (cons_enum m1 End) (cons_enum m2 End)

let rec cardinal0 = function
    Empty -> 0
  | Node(l, _, _, r, _) -> cardinal0 l + 1 + cardinal0 r

let rec bindings_aux accu = function
    Empty -> accu
  | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

let bindings0 s =
  bindings_aux [] s

let choose = min_binding0


type ('k,'v,'id) t = {
  cmp : ('k,'id) Bs_Cmp.t ;
  data : ('k,'v, 'id) t0 
}


let empty cmp = 
  {
    cmp ;
    data = empty0
  }
let is_empty map = 
  is_empty0 map.data 

let singleton cmp k v = {
  cmp ; 
  data = singleton0 k v 
}

let iter f map = 
  iter0 f map.data
let fold f map = 
  fold0 f map.data   
let for_all f map = 
  for_all0 f map.data   
let exists f map =   
  exists0 f map.data 

let filter f map = 
  {data = filter0 f map.data  ;
   cmp = map.cmp
  } 

let partition p map = 
  let l,r = partition0 p map.data in 
  let map_cmp = map.cmp in 
  { data = l; cmp = map_cmp },
  { data = r; cmp = map_cmp}

let cardinal map = 
  cardinal0 map.data   

let bindings map = 
  bindings0 map.data 

let min_binding map = 
  min_binding0 map.data 
let max_binding map =
  max_binding0 map.data   

let map f m = 
  let m_cmp = m.cmp in 
  { cmp = m_cmp  ; data = map0 f m.data}

let mapi f m  = 
  let m_cmp = m.cmp in 
  { cmp = m_cmp  ; data = mapi0 f m.data}


let add (type k) (type v) (type id) key data (map : (k,v,id) t) = 
  let map_cmp = map.cmp in 
  let module X = (val map_cmp) in 
  { cmp = map_cmp ; 
    data = add0 ~cmp:X.cmp key data map.data
  }

let find (type k) (type v) (type id) x (map : (k,v,id) t) = 
  let module X = (val map.cmp) in 
  find0 ~cmp:X.cmp x map.data

let mem (type k) (type v) (type id) x (map : (k,v,id) t) = 
  let module X = (val map.cmp) in 
  mem0 ~cmp:X.cmp x map.data

let remove (type k) (type v) (type id) x (map : (k,v,id) t) =   
  let map_cmp = map.cmp in
  let module X = (val map_cmp) in 
  { data = remove0 ~cmp:X.cmp x map.data ;
    cmp = map_cmp
  }

let split (type k) (type v) (type id) x (map : (k,v,id) t) =   
  let map_cmp = map.cmp in 
  let module X = (val map_cmp) in 
  let l,v,r = split0 ~cmp:X.cmp x map.data in 
  { cmp = map_cmp ; 
    data = l
  }, 
  v ,
  { cmp = map_cmp ; 
    data = r
  }

let merge (type k) (type v) (type id) f (s1 : (k,v,id) t) 
    (s2 : (k,_,id) t) = 
  let s1_cmp = s1.cmp in 
  let module X = (val s1_cmp) in 
  { data = merge0 ~cmp:X.cmp f s1.data s2.data ;
    cmp = s1_cmp
  }

let compare (type k) (type v) (type id) cmp 
    (m1 : (k,v,id) t) (m2 : (k,v,id) t) = 
  let module X = (val m1.cmp) in 
  compare0 ~cmp:X.cmp cmp m1.data m2.data

let equal (type k) (type v) (type id) cmp (m1 : (k,v,id) t) (m2 : (k,v,id) t) = 
  let module X = (val m1.cmp) in   
  equal0 ~cmp:X.cmp cmp m1.data m2.data 