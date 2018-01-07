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

module N = Bs_internalAVLtree
module B = Bs_Bag 
type ('key, + 'a, 'id) t0 = ('key,'a,'id) N.t0 

type ('k,'v,'id) t = 
  (('k,'id) Bs_Cmp.t,
   ('k,'v, 'id) t0 ) B.bag 



let empty0 = N.empty0      
let isEmpty0 = N.isEmpty0
let singleton0 = N.singleton0
let minBinding0 = N.minBinding0
let maxBinding0 = N.maxBinding0
let iter0 = N.iter0      
let map0  = N.map0
let mapi0 = N.mapi0
let fold0 = N.fold0
let forAll0 = N.forAll0
let exists0 = N.exists0    
let filter0 = N.filter0
let partition0 = N.partition0
let length0 = N.length0
let bindings0 = N.bindings0  

let rec add0 ~cmp x data (t : _ t0) =
  match N.toOpt t with (* TODO: test case with the same key *)
    None ->
    N.(return @@ node ~left:empty ~key:x ~value:data ~right:empty ~h:1)
  | Some n  ->
    let l,k,v,r = N.(left n, key n, value n, right n) in 
    let c = (Bs_Cmp.getCmp cmp) x k [@bs] in
    if c = 0 then
      N.(return @@ node ~left:l ~key:x ~value:data ~right:r ~h:(h n))
    else if c < 0 then
      N.(bal (add0 ~cmp x data l) k v  r)
    else
      N.(bal l k v (add0 ~cmp x data r))

let rec findOpt0 ~cmp x n = 
  match N.toOpt n with 
    None -> None
  | Some n (* Node(l, v, d, r, _) *)  ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then Some (N.value n)
    else findOpt0 ~cmp x (if c < 0 then N.left n else N.right n)

let rec findAssert0 ~cmp x n =
  match N.toOpt n with 
  | None -> 
    [%assert "Not_found"]
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then N.value n 
    else findAssert0 ~cmp x (if c < 0 then N.left n else N.right n)

let rec findWithDefault0 ~cmp ~def x n = 
  match N.toOpt n with 
    None ->
    def
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then N.value n 
    else findWithDefault0 ~cmp ~def x (if c < 0 then N.left n else N.right n)


let rec mem0 ~cmp x n = 
  match N.toOpt n with 
    None ->
    false
  | Some n (* Node(l, v, d, r, _) *) ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp x (if c < 0 then N.left n else N.right n)


let rec remove0 ~cmp x n = 
  match N.toOpt n with 
    None ->
    n
  | Some n (* Node(l, v, d, r, h) *)  ->
    let l,v,r = N.(left n, key n, right n ) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then
      N.(merge l r)
    else if c < 0 then
      N.(bal (remove0 ~cmp x l) v (value n) r)
    else
      N.(bal l v (value n) (remove0 ~cmp x r))

let rec splitAux ~cmp x (n : _ N.node) : _ t0 * _ option  * _ t0 =  
  let l,v,d,r = N.(left n , key n, value n, right n) in  
  let c = (Bs_Cmp.getCmp cmp) x v [@bs] in 
  if c = 0 then (l, Some d, r)
  else     
  if c < 0 then
    match N.toOpt l with 
    | None -> 
      N.(empty , None, return n)
    | Some l -> 
      let (ll, pres, rl) = splitAux ~cmp x l in (ll, pres, N.join rl v d r)
  else
    match N.toOpt r with 
    | None ->
      N.(return n, None, empty)
    | Some r -> 
      let (lr, pres, rr) = splitAux ~cmp x r in (N.join l v d lr, pres, rr)


let split0 ~cmp x n = 
  match N.toOpt n with 
  | None ->     
    N.(empty, None, empty)
  | Some n (* Node(l, v, d, r, _) *) ->
    splitAux ~cmp x n 

let rec merge0 ~cmp f s1 s2 =
  match N.(toOpt s1, toOpt s2) with
    (None, None) -> N.empty
  | Some n (* (Node (l1, v1, d1, r1, h1), _) *), _ when N.h n  >= N.height s2 ->
    let l1, v1, d1, r1 = N.(left n, key n, value n, right n) in 
    let (l2, d2, r2) = split0 ~cmp v1 s2 in
    N.concat_or_join (merge0 ~cmp f l1 l2) v1 (f v1 (Some d1) d2 [@bs]) (merge0 ~cmp f r1 r2)
  | _, Some n (* Node (l2, v2, d2, r2, h2)*) ->
    let l2,v2,d2,r2 = N.(left n, key n, value n, right n) in 
    let (l1, d1, r1) = split0 ~cmp v2 s1 in
    N.concat_or_join (merge0 ~cmp f l1 l2) v2 (f v2 d1 (Some d2) [@bs]) (merge0 ~cmp f r1 r2)
  | _ ->
    assert false

let rec compareAux e1 e2 ~kcmp ~vcmp =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    let c = (Bs_Cmp.getCmp kcmp) (N.key h1) (N.key h2) [@bs] in 
    if c = 0 then 
      let cx = vcmp (N.value h1) (N.value h2) [@bs] in 
      if cx = 0 then
        compareAux ~kcmp ~vcmp 
          (N.stackAllLeft  (N.right h1) t1 )
          (N.stackAllLeft (N.right h2) t2)
      else  cx
    else c 
  | _, _ -> 0   

let rec eqAux e1 e2 ~kcmp ~vcmp =
  match e1,e2 with 
  | h1::t1, h2::t2 ->
    if (Bs_Cmp.getCmp kcmp) (N.key h1) (N.key h2) [@bs] = 0 && 
       vcmp (N.value h1) (N.value h2) [@bs] then
      eqAux ~kcmp ~vcmp (
        N.stackAllLeft  (N.right h1) t1 ) (N.stackAllLeft (N.right h2) t2)
    else  false    
  | _, _ -> true (*end *)


let cmp0 s1 s2 ~kcmp ~vcmp =
  let len1,len2 = N.length0 s1, N.length0 s2 in 
  if len1 = len2 then 
    compareAux (N.stackAllLeft s1 []) (N.stackAllLeft s2 []) ~kcmp ~vcmp 
  else  if len1 < len2 then -1 else 1 

let eq0  s1 s2 ~kcmp ~vcmp =
  let len1, len2 = N.length0 s1, N.length0 s2 in 
  if len1 = len2 then
    eqAux (N.stackAllLeft s1 []) (N.stackAllLeft s2 []) ~kcmp ~vcmp
  else false


let ofArray0 ~cmp (xs : _ array) : _ t0 =     
  let result = ref N.empty in 
  for i = 0 to Array.length xs - 1 do  
    let k, v = (Bs_Array.unsafe_get xs i) in 
    result := add0 ~cmp  k v !result
  done ;
  !result 



let empty dict = 
  B.bag 
    ~dict 
    ~data:empty0

let isEmpty map = 
  isEmpty0 (B.data map)

let singleton dict k v = 
  B.bag ~dict 
    ~data:(singleton0 k v)


let iter f map = 
  iter0 f (B.data map)
let fold f map acc = 
  fold0 f (B.data map) acc   
let forAll f map = 
  forAll0 f (B.data map)   
let exists f map =   
  exists0 f (B.data map) 

let filter f map = 
  let dict, map = B.(dict map, data map) in 
  B.bag ~dict ~data:(filter0 f map)

let partition p map =   
  let dict, map = B.(dict map, data map) in 
  let l,r = partition0 p map in 
  B.bag ~dict ~data:l, B.bag ~dict ~data:r 

let length map = 
  length0 (B.data map)   

let bindings map = 
  bindings0 (B.data map) 

let minBinding map = 
  minBinding0 (B.data map) 
let maxBinding map =
  maxBinding0 (B.data map)   

let map f map = 
  let dict, map = B.(dict map, data map) in 
  B.bag ~dict ~data:(map0 f map)


let mapi f map  = 
  let dict,map = B.(dict map, data map) in 
  B.bag ~dict ~data:(mapi0 f map )



let add (type k) (type v) (type id) key data (map : (k,v,id) t) = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  B.bag ~dict ~data:(add0 ~cmp:X.cmp key data map)


let ofArray (type k) (type v) (type id) (dict : (k,id) Bs_Cmp.t) data = 
  let module M = (val dict ) in 
  B.bag
    ~dict 
    ~data:(ofArray0 ~cmp:M.cmp data)



let findOpt (type k) (type v) (type id) x (map : (k,v,id) t) = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  findOpt0 ~cmp:X.cmp x map

let findAssert (type k) (type v) (type id) x (map : (k,v,id) t) = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  findAssert0 ~cmp:X.cmp x map

let findWithDefault (type k) (type v) (type id) ~def x (map : (k,v,id) t) = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  findWithDefault0 ~cmp:X.cmp ~def x map


let mem (type k) (type v) (type id) x (map : (k,v,id) t) = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  mem0 ~cmp:X.cmp x map

let remove (type k) (type v) (type id) x (map : (k,v,id) t) =   
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  B.bag ~dict ~data:(remove0 ~cmp:X.cmp x map)

let split (type k) (type v) (type id) x (map : (k,v,id) t) =   
  let dict,map = B.(dict map, data map) in 

  let module X = (val dict) in 
  let l,v,r = split0 ~cmp:X.cmp x map in 
  B.bag ~dict 
    ~data:l
  , 
  v ,
  B.bag ~dict
    ~data:r


let merge (type k) (type v) (type id) f (s1 : (k,v,id) t) 
    (s2 : (k,_,id) t) = 
  let dict, s1_data, s2_data = B.(dict s1, data s1, data s2) in 
  let module X = (val dict) in 
  B.bag ~data:(merge0 ~cmp:X.cmp f s1_data s2_data )
    ~dict


let cmp (type k) (type v) (type id)  
    (m1 : (k,v,id) t) (m2 : (k,v,id) t) 
    cmp
  = 
  let dict, m1_data, m2_data = B.(dict m1, data m1, data m2) in 
  let module X = (val dict) in 
  cmp0 ~kcmp:X.cmp ~vcmp:cmp m1_data m2_data

let eq (type k) (type v) (type id) 
    (m1 : (k,v,id) t) (m2 : (k,v,id) t) cmp = 
  let dict, m1_data, m2_data = B.(dict m1, data m1, data m2) in 
  let module X = (val dict) in 
  eq0 ~kcmp:X.cmp ~vcmp:cmp m1_data m2_data 