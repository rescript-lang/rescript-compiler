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
(** Adapted by authors of BuckleScript without using functors          *)
type ('key, 'a, 'id) t0 = ('key,'a,'id) Bs_internalAVLtree.t0 = 
    Empty
  | Node of ('key, 'a, 'id) t0 * 'key * 'a * ('key, 'a, 'id) t0 * int


type ('key, 'a, 'id) enumeration = ('key, 'a, 'id) Bs_internalAVLtree.enumeration0 =
    End 
  | More of 'key * 'a * ('key, 'a, 'id) t0 * ('key, 'a, 'id) enumeration

let empty0 = Bs_internalAVLtree.empty0      
let isEmpty0 = Bs_internalAVLtree.isEmpty0
let singleton0 = Bs_internalAVLtree.singleton0
let minBinding0 = Bs_internalAVLtree.minBinding0
let maxBinding0 = Bs_internalAVLtree.maxBinding0
let iter0 = Bs_internalAVLtree.iter0      
let map0  = Bs_internalAVLtree.map0
let mapi0 = Bs_internalAVLtree.mapi0
let fold0 = Bs_internalAVLtree.fold0
let forAll0 = Bs_internalAVLtree.forAll0
let exists0 = Bs_internalAVLtree.exists0    
let filter0 = Bs_internalAVLtree.filter0
let partition0 = Bs_internalAVLtree.partition0
let cardinal0 = Bs_internalAVLtree.cardinal0
let bindings0 = Bs_internalAVLtree.bindings0  

let rec add0 ~cmp x data = function
    Empty ->
    Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then
      Node(l, x, data, r, h)
    else if c < 0 then
      Bs_internalAVLtree.bal (add0 ~cmp x data l) v d r
    else
      Bs_internalAVLtree.bal l v d (add0 ~cmp x data r)

let rec findOpt0 ~cmp x = function
    Empty ->
    None
  | Node(l, v, d, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then Some d
    else findOpt0 ~cmp x (if c < 0 then l else r)

let rec findAssert0 ~cmp x = function
    Empty ->
    [%assert "Not_found"]
  | Node(l, v, d, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then d
    else findAssert0 ~cmp x (if c < 0 then l else r)
    
let rec findWithDefault0 ~cmp ~def x = function
    Empty ->
    def
  | Node(l, v, d, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then d
    else findWithDefault0 ~cmp ~def x (if c < 0 then l else r)
    

let rec mem0 ~cmp x = function
    Empty ->
    false
  | Node(l, v, d, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp x (if c < 0 then l else r)


let rec remove0 ~cmp x = function
    Empty ->
    Empty
  | Node(l, v, d, r, h) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then
      Bs_internalAVLtree.merge l r
    else if c < 0 then
      Bs_internalAVLtree.bal (remove0 ~cmp x l) v d r
    else
      Bs_internalAVLtree.bal l v d (remove0 ~cmp x r)



let rec split0 ~cmp x = function
    Empty ->
    (Empty, None, Empty)
  | Node(l, v, d, r, _) ->
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then (l, Some d, r)
    else if c < 0 then
      let (ll, pres, rl) = split0 ~cmp x l in (ll, pres, Bs_internalAVLtree.join rl v d r)
    else
      let (lr, pres, rr) = split0 ~cmp x r in (Bs_internalAVLtree.join l v d lr, pres, rr)

let rec merge0 ~cmp f s1 s2 =
  match (s1, s2) with
    (Empty, Empty) -> Empty
  | (Node (l1, v1, d1, r1, h1), _) when h1 >= Bs_internalAVLtree.height s2 ->
    let (l2, d2, r2) = split0 ~cmp v1 s2 in
    Bs_internalAVLtree.concat_or_join (merge0 ~cmp f l1 l2) v1 (f v1 (Some d1) d2 [@bs]) (merge0 ~cmp f r1 r2)
  | (_, Node (l2, v2, d2, r2, h2)) ->
    let (l1, d1, r1) = split0 ~cmp v2 s1 in
    Bs_internalAVLtree.concat_or_join (merge0 ~cmp f l1 l2) v2 (f v2 d1 (Some d2) [@bs]) (merge0 ~cmp f r1 r2)
  | _ ->
    assert false



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
          compare_aux (Bs_internalAVLtree.cons_enum r1 e1) (Bs_internalAVLtree.cons_enum r2 e2)
  in compare_aux (Bs_internalAVLtree.cons_enum m1 End) (Bs_internalAVLtree.cons_enum m2 End)

let equal0 ~cmp:keycmp cmp m1 m2 =
  let rec equal_aux e1 e2 =
    match (e1, e2) with
      (End, End) -> true
    | (End, _)  -> false
    | (_, End) -> false
    | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
      (Bs_Cmp.getCmp keycmp) v1 v2 [@bs] = 0   && cmp d1 d2 [@bs] &&
      equal_aux (Bs_internalAVLtree.cons_enum r1 e1) (Bs_internalAVLtree.cons_enum r2 e2)
  in equal_aux (Bs_internalAVLtree.cons_enum m1 End) (Bs_internalAVLtree.cons_enum m2 End)





type ('k,'v,'id) t = {
  cmp : ('k,'id) Bs_Cmp.t ;
  data : ('k,'v, 'id) t0 
}


let empty cmp = 
  {
    cmp ;
    data = empty0
  }
let isEmpty map = 
  isEmpty0 map.data 

let singleton cmp k v = {
  cmp ; 
  data = singleton0 k v 
}

let iter f map = 
  iter0 f map.data
let fold f map = 
  fold0 f map.data   
let forAll f map = 
  forAll0 f map.data   
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

let minBinding map = 
  minBinding0 map.data 
let maxBinding map =
  maxBinding0 map.data   

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

let findOpt (type k) (type v) (type id) x (map : (k,v,id) t) = 
  let module X = (val map.cmp) in 
  findOpt0 ~cmp:X.cmp x map.data

let findAssert (type k) (type v) (type id) x (map : (k,v,id) t) = 
  let module X = (val map.cmp) in 
  findAssert0 ~cmp:X.cmp x map.data
  
let findWithDefault (type k) (type v) (type id) ~def x (map : (k,v,id) t) = 
  let module X = (val map.cmp) in 
  findWithDefault0 ~cmp:X.cmp ~def x map.data
  

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