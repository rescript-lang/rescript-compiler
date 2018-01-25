
(* Copyright (C) 2017 Authors of BuckleScript
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

    
module N = Bs_internalAVLtree
module B = Bs_BagM
module A = Bs_Array
module S = Bs_Sort 

type ('k, 'v, 'id) t = 
    (('k,'id) Bs_Cmp.t, ('k,'v) N.t0 ) B.bag 

let rec removeMutateAux nt x ~cmp = 
  let k = N.key nt in 
  let c = (Bs_Cmp.getCmp cmp) x k [@bs] in 
  if c = 0 then 
    let l,r = N.(left nt, right nt) in       
    match N.(toOpt l, toOpt r) with 
    | Some _,  Some nr ->  
      N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
      N.return (N.balMutate nt)
    | None, Some _ ->
      r  
    | (Some _ | None ), None ->  l 
  else 
    begin 
      if c < 0 then 
        match N.toOpt (N.left nt) with         
        | None -> N.return nt 
        | Some l ->
          N.leftSet nt (removeMutateAux ~cmp l x );
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.right nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (removeMutateAux ~cmp r x);
          N.return (N.balMutate nt)
    end    

let removeDone (type elt) (type id) (d : (elt,_,id) t) k =  
  let oldRoot = B.data d in   
  match N.toOpt oldRoot with 
  | None -> ()
  | Some oldRoot2 ->
    let module M = (val B.dict d) in 
    let newRoot = removeMutateAux ~cmp:M.cmp oldRoot2 k in 
    if newRoot != oldRoot then 
      B.dataSet d newRoot    

let remove d v =     
  removeDone d v; 
  d 

let rec removeArrayMutateAux t xs i len ~cmp  =  
  if i < len then 
    let ele = A.getUnsafe xs i in 
    let u = removeMutateAux t ele ~cmp in 
    match N.toOpt u with 
    | None -> N.empty0
    | Some t -> removeArrayMutateAux t xs (i+1) len ~cmp 
  else N.return t    

let removeArrayDone (type elt) (type id) (d : (elt,_,id) t) xs =  
  let oldRoot = B.data d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    let module M = (val B.dict d)  in 
    let newRoot = removeArrayMutateAux nt xs 0 len ~cmp:M.cmp in 
    if newRoot != oldRoot then 
      B.dataSet d newRoot

let removeArray d xs =      
  removeArrayDone d xs; 
  d

let rec updateDone0 t x   f  ~cmp =   
  match N.toOpt t with 
  | None ->
    (match f None [@bs] with
    | Some data -> N.singleton0 x data
    | None -> t)
  | Some nt -> 
    let k = N.key nt in 
    let  c = (Bs_Cmp.getCmp cmp) x k [@bs] in  
    if c = 0 then begin     
      match f (Some (N.value nt)) [@bs] with
      | None ->
        let l,r = N.left nt, N.right nt in
        begin match N.toOpt l, N.toOpt r with
        | Some _, Some nr ->
          N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
          N.return (N.balMutate nt)
        | None, Some _ ->
          r
        | (Some _ | None), None -> l
        end
      | Some data -> 
        N.valueSet nt data;
        N.return nt
    end      
    else
      let l, r = N.(left nt, right nt) in 
      (if c < 0 then                   
         let ll = updateDone0  l x f ~cmp in
         N.leftSet nt ll
       else   
         N.rightSet nt (updateDone0  r x f ~cmp);
      );
      N.return (N.balMutate nt)  
      
let updateDone (type k) (type id) (t : (k,_,id) t)  x f =       
  let oldRoot = B.data t in 
  let module M = (val B.dict t) in 
  let newRoot = updateDone0 oldRoot x f ~cmp:M.cmp in 
  if  newRoot != oldRoot then 
    B.dataSet t newRoot 

let update t x f = 
  updateDone t x f ; 
  t
  
let empty ~dict = 
  B.bag ~dict ~data:N.empty0  
let isEmpty d = 
  N.isEmpty0 (B.data d)
let singleton dict x v= 
  B.bag ~data:(N.singleton0 x v) ~dict 

let minKeyOpt m = N.minKeyOpt0 (B.data m)
let minKeyNull m = N.minKeyNull0 (B.data m)
let maxKeyOpt m = N.maxKeyOpt0 (B.data m)
let maxKeyNull m = N.maxKeyNull0 (B.data m)
let minimum m = N.minKVOpt0 (B.data m)
let minNull m = N.minKVNull0 (B.data m) 
let maximum m = N.maxKVOpt0 (B.data m)
let maxNull m = N.maxKVNull0 (B.data m)

let forEach d f =
  N.iter0 (B.data d) f     
let reduce d acc cb = 
  N.fold0 (B.data d) acc cb 
let every d p = 
  N.every0 (B.data d) p 
let some d  p = 
  N.some0 (B.data d) p       
let size d = 
  N.length0 (B.data d)
let toList d =
  N.toList0 (B.data d)
let toArray d = 
  N.toArray0 (B.data d)
let keysToArray d =   
  N.keysToArray0 (B.data d)
let valuesToArray d =   
  N.valuesToArray0 (B.data d)
let ofSortedArrayUnsafe ~dict xs : _ t =
  B.bag ~data:(N.ofSortedArrayUnsafe0 xs) ~dict   
let checkInvariant d = 
  N.checkInvariant (B.data d)  
let cmp (type k)  (type id) (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) cmp = 
  let dict, m1_data, m2_data = B.(dict m1, data m1, data m2) in 
  let module X = (val dict) in   
  N.cmp0 ~kcmp:X.cmp ~vcmp:cmp m1_data m2_data  
let eq (type k) (type id) (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) cmp = 
  let dict, m1_data, m2_data = B.(dict m1, data m1, data m2) in 
  let module M = (val dict) in 
  N.eq0 ~kcmp:M.cmp ~vcmp:cmp m1_data m2_data   
let map m f = 
  let dict, map = B.(dict m, data m) in 
  B.bag ~dict ~data:(N.map0 map f)
let mapi map  f = 
  let dict,map = B.(dict map, data map) in 
  B.bag ~dict ~data:(N.mapi0 map f)
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
let getExn (type k)  (type id)  (map : (k,_,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.findExn0 ~cmp:X.cmp map x 
let has (type k) (type id)  (map : (k,_,id) t) x = 
  let dict,map = B.(dict map, data map) in 
  let module X = (val dict) in 
  N.mem0 ~cmp:X.cmp map x
let ofArray (type k) (type id) data ~(dict : (k,id) Bs_Cmp.t)= 
  let module M = (val dict ) in 
  B.bag ~dict  ~data:(N.ofArray0 ~cmp:M.cmp data)  
let setDone (type elt) (type id) (m : (elt,_,id) t) e v = 
  let dict, oldRoot = B.(dict m, data m) in 
  let module M = (val dict) in 
  let newRoot = N.updateMutate ~cmp:M.cmp oldRoot e v in 
  if newRoot != oldRoot then 
    B.dataSet m newRoot
let set m e v = 
  setDone m e v;
  m
let mergeArrayAux t  xs ~cmp =     
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    let key,value = A.getUnsafe xs i in 
    v := N.updateMutate !v key value ~cmp
  done; 
  !v 
let mergeArrayDone (type elt) (type id) (d : (elt,_,id) t ) xs =   
  let dict = B.dict d in 
  let oldRoot = B.data d in 
  let module M = (val dict) in 
  let newRoot = mergeArrayAux oldRoot xs ~cmp:M.cmp in 
  if newRoot != oldRoot then 
    B.dataSet d newRoot 
let mergeArray d xs = 
  mergeArrayDone d xs ; 
  d   

