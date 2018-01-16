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

type ('key,  'a, 'id) t0 = ('key, 'a) N.t0 

type ('k,'v,'id) t = 
  (('k,'id) Bs_Cmp.t,
   ('k,'v, 'id) t0 ) B.bag 

(** [addCheck0 m k v cmp]
    return m reference unchanged when [k] already existed
*)   
let rec addCheck0  (t : _ t0) newK newD ~cmp : _ t0 =
  match N.toOpt t with 
  | None -> N.singleton0 newK newD
  | Some nt ->
    let k = N.key nt in 
    let c = (Bs_Cmp.getCmp cmp) newK k [@bs] in
    if c = 0 then t 
    else
      let l,r,v = N.left nt, N.right nt, N.value nt in 
      if c < 0 then 
        let ll = addCheck0 ~cmp l newK newD in 
        if ll == l then t 
        else N.bal ll k v r 
      else 
        let rr = addCheck0 ~cmp r newK newD in 
        if rr == r then t 
        else N.bal l k v rr    

let rec add0  (t : _ t0) newK newD  ~cmp =
  match N.toOpt t with 
  | None -> N.singleton0 newK newD 
  | Some n  ->
    let k= N.key n in 
    let c = (Bs_Cmp.getCmp cmp) newK k [@bs] in
    if c = 0 then
      N.updateKV n newK newD 
    else 
      let l,r,v = N.left n, N.right n, N.value n in 
      if c < 0 then
        N.bal (add0 ~cmp l newK newD ) k v  r
      else
        N.bal l k v (add0 ~cmp r newK newD )



let rec remove0  n x ~cmp = 
  match N.toOpt n with 
  | None ->
    n
  | Some n  ->
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

let rec splitAuxPivot ~cmp n x pres  =  
  let l,v,d,r = N.(left n , key n, value n, right n) in  
  let c = (Bs_Cmp.getCmp cmp) x v [@bs] in 
  if c = 0 then begin 
    pres := Some d; 
    (l,  r)
  end
  else     
  if c < 0 then
    match N.toOpt l with 
    | None -> 
      N.(empty, return n)
    | Some l -> 
      let (ll,rl) = splitAuxPivot ~cmp l x pres in
      (ll,  N.join rl v d r)
  else
    match N.toOpt r with 
    | None ->
      N.(return n, empty)
    | Some r -> 
      let (lr,  rr) = splitAuxPivot ~cmp r x pres in
      (N.join l v d lr,  rr)


let split0 ~cmp n x = 
  match N.toOpt n with 
  | None ->     
    N.(empty, empty), None
  | Some n  ->
    let pres = ref None in
    let v = splitAuxPivot ~cmp n x pres in 
    v, !pres

let rec merge0 s1 s2 f ~cmp =
  match N.(toOpt s1, toOpt s2) with
    (None, None) -> N.empty
  | Some _, None -> 
    N.filterMap0 s1 (fun[@bs] k v -> 
        f k (Some v) None [@bs]
      )
  | None, Some _ -> 
    N.filterMap0 s2 (fun[@bs] k v -> 
        f k None (Some v) [@bs]
      )
  | Some s1n , Some s2n -> 
    if N.h s1n  >= N.h s2n  then
      let l1, v1, d1, r1 = N.(left s1n, key s1n, value s1n, right s1n) in 
      let d2 = ref None in 
      let (l2, r2) = splitAuxPivot ~cmp s2n v1 d2 in
      let d2 = !d2 in 
      let newLeft = merge0 ~cmp l1 l2 f in 
      let newD = f v1 (Some d1) d2 [@bs] in 
      let newRight = merge0 ~cmp r1 r2 f in 
      N.concatOrJoin newLeft v1 newD  newRight
    else
      let l2,v2,d2,r2 = N.(left s2n, key s2n, value s2n, right s2n) in 
      let d1 = ref None in 
      let (l1,  r1) = splitAuxPivot ~cmp s1n v2 d1 in
      let d1 = !d1 in 
      let newLeft = merge0 ~cmp l1 l2 f in 
      let newD = (f v2 d1 (Some d2) [@bs]) in 
      let newRight = (merge0 ~cmp r1 r2 f) in 
      N.concatOrJoin newLeft v2 newD newRight







let empty dict = 
  B.bag 
    ~dict 
    ~data:N.empty0

let isEmpty map = 
  N.isEmpty0 (B.data map)

let singleton dict k v = 
  B.bag ~dict 
    ~data:(N.singleton0 k v)


let iter map f = 
  N.iter0 (B.data map) f
let fold map acc f = 
  N.fold0 (B.data map) acc f  
let forAll map f = 
  N.forAll0 (B.data map) f  
let exists map f =   
  N.exists0 (B.data map) f

let filter f map = 
  let dict, map = B.(dict map, data map) in 
  B.bag ~dict ~data:(N.filterShared0 f map)

let partition p map =   
  let dict, map = B.(dict map, data map) in 
  let l,r = N.partitionShared0 p map in 
  B.bag ~dict ~data:l, B.bag ~dict ~data:r 

let length map = 
  N.length0 (B.data map)   

let toList map = 
  N.toList0 (B.data map) 
let toArray m = 
  N.toArray0 (B.data m)
let keysToArray m =   
  N.keysToArray0 (B.data m)
let valuesToArray m =   
  N.valuesToArray0 (B.data m)

let minKVOpt m = N.minKVOpt0 (B.data m)
let minKVNull m = N.minKVNull0 (B.data m) 
let maxKVOpt m = N.maxKVOpt0 (B.data m)
let maxKVNull m = N.maxKVNull0 (B.data m)


let map m f = 
  let dict, map = B.(dict m, data m) in 
  B.bag ~dict ~data:(N.map0 map f)


let mapi map  f = 
  let dict,map = B.(dict map, data map) in 
  B.bag ~dict ~data:(N.mapi0 map f)



let add (type k) (type id) (map : (k,_,id) t) key data  = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  B.bag ~dict ~data:(add0 ~cmp:X.cmp map key data )

let ofArray (type k) (type id) (dict : (k,id) Bs_Cmp.t) data = 
  let module M = (val dict ) in 
  B.bag
    ~dict 
    ~data:(N.ofArray0 ~cmp:M.cmp data)


let findOpt (type k) (type id) (map : (k,_,id) t) x  = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.findOpt0 ~cmp:X.cmp  map x 

let findNull (type k) (type id) (map : (k,_,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.findNull0 ~cmp:X.cmp  map x

let findWithDefault (type k) (type id)  (map : (k,_,id) t) x def = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.findWithDefault0 ~cmp:X.cmp map x  def

let findExn (type k) (type id)  (map : (k,_,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.findExn0 ~cmp:X.cmp map x 
  
let mem (type k) (type id)  (map : (k,_,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.mem0 ~cmp:X.cmp map x

let remove (type k) (type id) (map : (k,_,id) t) x  =   
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  B.bag ~dict ~data:(remove0 ~cmp:X.cmp map x )

let split (type k)  (type id) (map : (k,_,id) t) x =   
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  let (l,r),v = split0 ~cmp:X.cmp map x in 
  (B.bag ~dict 
     ~data:l
   ,   
   B.bag ~dict
     ~data:r), v


let merge (type k) (type id)  (s1 : (k,_,id) t) 
    (s2 : (k,_,id) t) f = 
  let dict, s1_data, s2_data = B.(dict s1, data s1, data s2) in 
  let module X = (val dict) in 
  B.bag ~data:(merge0 ~cmp:X.cmp  s1_data s2_data f)
    ~dict


let cmp (type k)  (type id)  
    (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) 
    cmp
  = 
  let dict, m1_data, m2_data = B.(dict m1, data m1, data m2) in 
  let module X = (val dict) in 
  N.cmp0 ~kcmp:X.cmp ~vcmp:cmp m1_data m2_data

let eq (type k) (type id) 
    (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) cmp = 
  let dict, m1_data, m2_data = B.(dict m1, data m1, data m2) in 
  let module X = (val dict) in 
  N.eq0 ~kcmp:X.cmp ~vcmp:cmp m1_data m2_data 

let empty0 = N.empty0      
let isEmpty0 = N.isEmpty0
let singleton0 = N.singleton0
let minKVOpt0 = N.minKVOpt0
let maxKVOpt0 = N.maxKVOpt0
let iter0 = N.iter0      
let map0  = N.map0
let mapi0 = N.mapi0
let fold0 = N.fold0
let forAll0 = N.forAll0
let exists0 = N.exists0    


let length0 = N.length0
let toList0 = N.toList0
let ofArray0 = N.ofArray0
let findOpt0 = N.findOpt0
let findNull0 = N.findNull0
let findWithDefault0 = N.findWithDefault0
let findExn0 = N.findExn0
let mem0 = N.mem0
let cmp0 = N.cmp0
let eq0 = N.eq0   
let keysToArray0 = N.keysToArray0
let valuesToArray0 = N.valuesToArray0
let filter0 = N.filterShared0
let partition0 = N.partitionShared0