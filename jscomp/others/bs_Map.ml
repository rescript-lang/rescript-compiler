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
type ('key,  'a, 'id) t0 = ('key,'a) N.t0 

type ('k,'v,'id) t = 
  (('k,'id) Bs_Cmp.t,
   ('k,'v, 'id) t0 ) B.bag 



let empty0 = N.empty0      
let isEmpty0 = N.isEmpty0
let singleton0 = N.singleton0
let minBinding0 = N.minKVOpt0
let maxBinding0 = N.maxKVOpt0
let iter0 = N.iter0      
let map0  = N.map0
let mapi0 = N.mapi0
let fold0 = N.fold0
let forAll0 = N.forAll0
let exists0 = N.exists0    
let filter0 = N.filter0
let partition0 = N.partition0
let length0 = N.length0
let toList0 = N.toList0

let rec add0  (t : _ t0) x data  ~cmp =
  match N.toOpt t with (* TODO: test case with the same key *)
    None ->
    N.(return @@ node ~left:empty ~key:x ~value:data ~right:empty ~h:1)
  | Some n  ->
    let l,k,v,r = N.(left n, key n, value n, right n) in 
    let c = (Bs_Cmp.getCmp cmp) x k [@bs] in
    if c = 0 then
      N.(return @@ node ~left:l ~key:x ~value:data ~right:r ~h:(h n))
    else if c < 0 then
      N.(bal (add0 ~cmp l x data ) k v  r)
    else
      N.(bal l k v (add0 ~cmp r x data ))

let rec findOpt0  n x ~cmp = 
  match N.toOpt n with 
    None -> None
  | Some n (* Node(l, v, d, r, _) *)  ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then Some (N.value n)
    else findOpt0 ~cmp  (if c < 0 then N.left n else N.right n) x 

let rec findAssert0  n x ~cmp =
  match N.toOpt n with 
  | None -> 
    [%assert "Not_found"]
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then N.value n 
    else findAssert0 ~cmp  (if c < 0 then N.left n else N.right n) x 

let rec findWithDefault0   n x def ~cmp = 
  match N.toOpt n with 
    None ->
    def
  | Some n (* Node(l, v, d, r, _)*) ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then N.value n 
    else findWithDefault0 ~cmp  (if c < 0 then N.left n else N.right n) x def


let rec mem0  x n ~cmp = 
  match N.toOpt n with 
    None ->
    false
  | Some n (* Node(l, v, d, r, _) *) ->
    let v = N.key n in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    c = 0 || mem0 ~cmp x (if c < 0 then N.left n else N.right n)


let rec remove0  n x ~cmp = 
  match N.toOpt n with 
    None ->
    n
  | Some n (* Node(l, v, d, r, h) *)  ->
    let l,v,r = N.(left n, key n, right n ) in 
    let c = (Bs_Cmp.getCmp cmp) x v [@bs] in
    if c = 0 then
      match N.toOpt l, N.toOpt r with 
      | None, _ -> r 
      | _, None -> l 
      | _, Some rn -> 
        let kr, vr = ref (N.key rn), ref (N.value rn) in 
        let r = N.removeMinAuxWithRef rn kr vr in 
        N.bal l !kr !vr r
    else if c < 0 then
      N.(bal (remove0 ~cmp l x ) v (value n) r)
    else
      N.(bal l v (value n) (remove0 ~cmp r x ))

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

let rec merge0 s1 s2 f ~cmp =
  match N.(toOpt s1, toOpt s2) with
    (None, None) -> N.empty
  | Some n (* (Node (l1, v1, d1, r1, h1), _) *), _ 
    when N.h n  >= (match N.toOpt s2 with None -> 0 | Some n -> N.h n) ->
    let l1, v1, d1, r1 = N.(left n, key n, value n, right n) in 
    let (l2, d2, r2) = split0 ~cmp v1 s2 in
    N.concatOrJoin (merge0 ~cmp l1 l2 f) v1 (f v1 (Some d1) d2 [@bs]) (merge0 ~cmp r1 r2 f)
  | _, Some n (* Node (l2, v2, d2, r2, h2)*) ->
    let l2,v2,d2,r2 = N.(left n, key n, value n, right n) in 
    let (l1, d1, r1) = split0 ~cmp v2 s1 in
    N.concatOrJoin (merge0 ~cmp l1 l2 f) v2 (f v2 d1 (Some d2) [@bs]) (merge0 ~cmp r1 r2 f)
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
    result := add0 ~cmp !result k v 
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


let iter map f = 
  iter0 (B.data map) f
let fold map acc f = 
  fold0 (B.data map) acc f  
let forAll map f = 
  forAll0 (B.data map) f  
let exists map f =   
  exists0 (B.data map) f

let filter f map = 
  let dict, map = B.(dict map, data map) in 
  B.bag ~dict ~data:(filter0 f map)

let partition p map =   
  let dict, map = B.(dict map, data map) in 
  let l,r = partition0 p map in 
  B.bag ~dict ~data:l, B.bag ~dict ~data:r 

let length map = 
  length0 (B.data map)   

let toList map = 
  toList0 (B.data map) 

let minBinding map = 
  minBinding0 (B.data map) 
let maxBinding map =
  maxBinding0 (B.data map)   

let map m f = 
  let dict, map = B.(dict m, data m) in 
  B.bag ~dict ~data:(map0 map f)


let mapi map  f = 
  let dict,map = B.(dict map, data map) in 
  B.bag ~dict ~data:(mapi0 map f)



let add (type k) (type v) (type id) (map : (k,v,id) t) key data  = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  B.bag ~dict ~data:(add0 ~cmp:X.cmp map key data )


let ofArray (type k) (type v) (type id) (dict : (k,id) Bs_Cmp.t) data = 
  let module M = (val dict ) in 
  B.bag
    ~dict 
    ~data:(ofArray0 ~cmp:M.cmp data)



let findOpt (type k) (type v) (type id) (map : (k,v,id) t) x  = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  findOpt0 ~cmp:X.cmp  map x 

let findAssert (type k) (type v) (type id) (map : (k,v,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  findAssert0 ~cmp:X.cmp  map x

let findWithDefault (type k) (type v) (type id)  (map : (k,v,id) t) x def = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  findWithDefault0 ~cmp:X.cmp map x  def


let mem (type k) (type v) (type id)  (map : (k,v,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  mem0 ~cmp:X.cmp x map

let remove (type k) (type v) (type id) (map : (k,v,id) t) x  =   
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  B.bag ~dict ~data:(remove0 ~cmp:X.cmp map x )

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


let merge (type k) (type v) (type id)  (s1 : (k,v,id) t) 
    (s2 : (k,_,id) t) f = 
  let dict, s1_data, s2_data = B.(dict s1, data s1, data s2) in 
  let module X = (val dict) in 
  B.bag ~data:(merge0 ~cmp:X.cmp  s1_data s2_data f)
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