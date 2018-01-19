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
module A = Bs_Array
type ('key,  'a, 'id) t0 = ('key, 'a) N.t0 

type ('k,'v,'id) t = 
  (('k,'id) Bs_Cmp.t,
   ('k,'v, 'id) t0 ) B.bag 

let rec set0  (t : _ t0) newK newD  ~cmp =
  match N.toOpt t with 
  | None -> N.singleton0 newK newD 
  | Some n  ->
    let k= N.key n in 
    let c = (Bs_Cmp.getCmp cmp) newK k [@bs] in
    if c = 0 then
      N.return (N.updateValue n newD) 
    else 
      let l,r,v = N.left n, N.right n, N.value n in 
      if c < 0 then (* Worth optimize for reference equality? *)
        N.bal (set0 ~cmp l newK newD ) k v  r
      else
        N.bal l k v (set0 ~cmp r newK newD )

let rec update0  (t : _ t0) newK f  ~cmp =
  match N.toOpt t with 
  | None ->
    begin match f None [@bs] with 
      | None -> t 
      | Some newD -> N.singleton0 newK newD 
    end 
  | Some n  ->
    let k= N.key n in 
    let c = (Bs_Cmp.getCmp cmp) newK k [@bs] in
    if c = 0 then
      match f (Some (N.value n)) [@bs] with 
      | None ->
        let l, r = N.left n , N.right n in  
        begin match N.toOpt l, N.toOpt r with
        | None, _ -> r
        | _, None -> l
        | _, Some rn ->
          let kr, vr = ref (N.key rn), ref (N.value rn) in
          let r = N.removeMinAuxWithRef rn kr vr in
          N.bal l !kr !vr r 
        end
      | Some newD -> N.return (N.updateValue n newD)
    else 
      let l,r,v = N.left n, N.right n, N.value n in 
      if c < 0 then
        let ll = (update0 ~cmp l newK f ) in
        if l == ll then
          t
        else 
          N.bal ll k v  r            
      else
        let rr = (update0 ~cmp r newK f) in
        if r == rr then t 
        else N.bal l k v rr

(*  unboxing API was not exported
    since the correct API is really awkard
    [bool -> 'k Js.null -> ('a Js.null * bool)]
    even for specialized [k] the first [bool] can 
    be erased, maybe the perf boost does not justify the inclusion of such API

    [updateWithNull m x f]
    the callback to [f exist v] 
    when [v] is non-null,
    [exist] is guaranteed to be true
    [v] is guranteed to be [null],
    when [exist] is [true], [v] could be [null], 
    since ['a] is polymorphic
*)


let rec removeAux0  n x ~cmp = 
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
    match N.toOpt l with 
    | None -> N.return n (* Nothing to remove *)
    | Some left ->
      let ll = removeAux0 left x ~cmp in 
      if ll == l then (N.return n)
      else N.bal ll v (N.value n) r
  else
    match N.toOpt r with 
    | None -> N.return n (* Nothing to remove *)
    | Some right -> 
      let rr = removeAux0 ~cmp right x in
      if rr == r then N.return n
      else N.bal l v (N.value n)  rr

let remove0 n x ~cmp = 
  match N.toOpt n with        
  | None -> N.empty0
  | Some n -> removeAux0 n x ~cmp 
  
let mergeArray0   h arr ~cmp =   
  let len = A.length arr in 
  let v = ref h in  
  for i = 0 to len - 1 do 
    let key,value = A.unsafe_get arr i in 
    v := set0 !v  ~cmp key value
  done ;
  !v 

let rec splitAuxPivot n x pres  ~cmp =  
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

let rec removeArrayAux t xs i len ~cmp =
  if i < len then
    let ele = A.unsafe_get xs i in
    let u =  removeAux0 t ele ~cmp in
    match N.toOpt u with
    | None -> u
    | Some t -> removeArrayAux t xs (i + 1) len ~cmp 
  else
    N.return t
      
let removeArray0 t keys ~cmp =
  let len = A.length keys in
  match N.toOpt t with
  | None -> N.empty0
  | Some t ->  removeArrayAux t keys 0 len ~cmp 

let ofArray (type k) (type id) data ~(dict : (k,id) Bs_Cmp.t)  = 
  let module M = (val dict ) in 
  B.bag ~dict ~data:(N.ofArray0 ~cmp:M.cmp data)

let remove (type k) (type id) (m : (k,_,id) t) x  =   
  let odata = B.data m in 
  match N.toOpt odata with 
  | None ->  m 
  | Some data ->  
    let dict = B.dict m in   
    let module M = (val dict) in 
    let newData = removeAux0 ~cmp:M.cmp data x in  
    if newData == odata then m
    else B.bag ~dict ~data:newData

let removeArray (type k) (type id) (m : (k,_,id) t) xs =     
  let odata = B.data m in 
  match N.toOpt odata with 
  | None -> m 
  | Some data -> 
    let dict = B.dict m in 
    let module M = (val dict) in 
    let len = A.length xs in 
    let newData = removeArrayAux data xs 0 len ~cmp:M.cmp in 
    if newData == odata then m 
    else B.bag ~dict ~data:newData

let set (type k) (type id) (map : (k,_,id) t) key data  = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  B.bag ~dict ~data:(set0 ~cmp:X.cmp map key data )

let mergeArray (type elt) (type id) (m : (elt,_,id) t) e = 
  let dict, data = B.(dict m, data m) in 
  let module M = (val dict) in 
  let newData = mergeArray0 ~cmp:M.cmp data e in 
  B.bag ~dict ~data:newData  

let update (type k) (type id) (map : (k,_,id) t) key f  = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  B.bag ~dict ~data:(update0 ~cmp:X.cmp map key f )

let split (type k)  (type id) (map : (k,_,id) t) x =   
  let dict,map = B.(dict map, data map) in 
  let module M = (val dict) in 
  let (l,r),b = split0 ~cmp:M.cmp map x in 
  (B.bag ~dict ~data:l, B.bag ~dict ~data:r), b  

let merge (type k) (type id)  (s1 : (k,_,id) t) 
    (s2 : (k,_,id) t) f = 
  let dict, s1_data, s2_data = B.(dict s1, data s1, data s2) in 
  let module X = (val dict) in 
  B.bag ~data:(merge0 ~cmp:X.cmp  s1_data s2_data f)
    ~dict

let empty ~dict = 
  B.bag  ~dict  ~data:N.empty0

let isEmpty map = 
  N.isEmpty0 (B.data map)

let singleton k v ~dict = 
  B.bag ~dict ~data:(N.singleton0 k v)     

let cmp (type k)  (type id)  (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) cmp
  = 
  let dict, m1_data, m2_data = B.(dict m1, data m1, data m2) in 
  let module X = (val dict) in 
  N.cmp0 ~kcmp:X.cmp ~vcmp:cmp m1_data m2_data

let eq (type k) (type id) 
    (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) cmp = 
  let dict, m1_data, m2_data = B.(dict m1, data m1, data m2) in 
  let module X = (val dict) in 
  N.eq0 ~kcmp:X.cmp ~vcmp:cmp m1_data m2_data 

let forEach m f = N.iter0 (B.data m) f

let fold m acc f = N.fold0 (B.data m) acc f  

let forAll m f = N.forAll0 (B.data m) f  

let exists m f = N.exists0 (B.data m) f

let filter m f = 
  let dict, map = B.(dict m, data m) in 
  B.bag ~dict ~data:(N.filterShared0 map f)

let partition m p =   
  let dict, map = B.(dict m, data m) in 
  let l,r = N.partitionShared0 map p in 
  B.bag ~dict ~data:l, B.bag ~dict ~data:r 

let map m f = 
  let dict, map = B.(dict m, data m) in 
  B.bag ~dict ~data:(N.map0 map f)
let mapi m  f = 
  let dict,map = B.(dict m, data m) in 
  B.bag ~dict ~data:(N.mapi0 map f)

let size map = 
  N.length0 (B.data map)   

let toList map = 
  N.toList0 (B.data map) 
let toArray m = 
  N.toArray0 (B.data m)
let keysToArray m =   
  N.keysToArray0 (B.data m)
let valuesToArray m =   
  N.valuesToArray0 (B.data m)

let minKeyOpt m = N.minKeyOpt0 (B.data m)
let minKeyNull m = N.minKeyNull0 (B.data m)
let maxKeyOpt m = N.maxKeyOpt0 (B.data m)
let maxKeyNull m = N.maxKeyNull0 (B.data m)    
let minimum m = N.minKVOpt0 (B.data m)
let minNull m = N.minKVNull0 (B.data m) 
let maximum m = N.maxKVOpt0 (B.data m)
let maxNull m = N.maxKVNull0 (B.data m)

let get (type k) (type id) (map : (k,_,id) t) x  = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.findOpt0 ~cmp:X.cmp  map x 

let getNull (type k) (type id) (map : (k,_,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.findNull0 ~cmp:X.cmp  map x

let getWithDefault (type k) (type id)  (map : (k,_,id) t) x def = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.findWithDefault0 ~cmp:X.cmp map x  def

let getExn (type k) (type id)  (map : (k,_,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.findExn0 ~cmp:X.cmp map x 

let has (type k) (type id)  (map : (k,_,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.mem0 ~cmp:X.cmp map x

let checkInvariant m  =
  N.checkInvariant (B.data m)
let empty0 = N.empty0      
let ofArray0 = N.ofArray0
let isEmpty0 = N.isEmpty0
let singleton0 = N.singleton0

let cmp0 = N.cmp0
let eq0 = N.eq0   
let mem0 = N.mem0
let iter0 = N.iter0      
let fold0 = N.fold0
let forAll0 = N.forAll0
let exists0 = N.exists0    
let length0 = N.length0
let toList0 = N.toList0
let toArray0 = N.toArray0

let keysToArray0 = N.keysToArray0
let valuesToArray0 = N.valuesToArray0

let minKVOpt0 = N.minKVOpt0
let maxKVOpt0 = N.maxKVOpt0
let findOpt0 = N.findOpt0
let findNull0 = N.findNull0
let findWithDefault0 = N.findWithDefault0
let findExn0 = N.findExn0

let mapi0 = N.mapi0
let map0  = N.map0

let filter0 = N.filterShared0
let partition0 = N.partitionShared0
let getData = B.data
let getDict = B.dict
let packDictData = B.bag 
