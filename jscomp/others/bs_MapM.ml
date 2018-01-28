
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
module A = Bs_Array
module S = Bs_Sort 

type ('key, 'id) dict = ('key, 'id) Bs_Cmp.t
    
type ('k, 'v, 'id) t = {
  dict: ('k, 'id) dict;
  mutable data: ('k, 'v) N.t
} [@@bs.deriving abstract]

let rec removeMutateAux nt x ~cmp = 
  let k = N.key nt in 
  let c = (Bs_Cmp.getCmpIntenral cmp) x k [@bs] in 
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

let remove (type elt) (type id) (d : (elt,_,id) t) k =  
  let oldRoot = data d in   
  match N.toOpt oldRoot with 
  | None -> ()
  | Some oldRoot2 ->
    let module M = (val dict d) in 
    let newRoot = removeMutateAux ~cmp:M.cmp oldRoot2 k in 
    if newRoot != oldRoot then 
      dataSet d newRoot    


let rec removeArrayMutateAux t xs i len ~cmp  =  
  if i < len then 
    let ele = A.getUnsafe xs i in 
    let u = removeMutateAux t ele ~cmp in 
    match N.toOpt u with 
    | None -> N.empty
    | Some t -> removeArrayMutateAux t xs (i+1) len ~cmp 
  else N.return t    

let removeMany (type elt) (type id) (d : (elt,_,id) t) xs =  
  let oldRoot = data d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    let module M = (val dict d)  in 
    let newRoot = removeArrayMutateAux nt xs 0 len ~cmp:M.cmp in 
    if newRoot != oldRoot then 
      dataSet d newRoot


let rec updateDone t x   f  ~cmp =   
  match N.toOpt t with 
  | None ->
    (match f None [@bs] with
    | Some data -> N.singleton x data
    | None -> t)
  | Some nt -> 
    let k = N.key nt in 
    let  c = (Bs_Cmp.getCmpIntenral cmp) x k [@bs] in  
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
         let ll = updateDone  l x f ~cmp in
         N.leftSet nt ll
       else   
         N.rightSet nt (updateDone  r x f ~cmp);
      );
      N.return (N.balMutate nt)  
      
let update (type k) (type id) (t : (k,_,id) t)  x f =       
  let oldRoot = data t in 
  let module M = (val dict t) in 
  let newRoot = updateDone oldRoot x f ~cmp:M.cmp in 
  if  newRoot != oldRoot then 
    dataSet t newRoot 

let empty ~dict = 
  t ~dict ~data:N.empty  
let isEmpty d = 
  N.isEmpty (data d)
let singleton dict x v= 
  t ~data:(N.singleton x v) ~dict 

let minKey m = N.minKey (data m)
let minKeyUndefined m = N.minKeyUndefined (data m)
let maxKey m = N.maxKey (data m)
let maxKeyUndefined m = N.maxKeyUndefined (data m)
let minimum m = N.minimum (data m)
let minUndefined m = N.minUndefined (data m) 
let maximum m = N.maximum (data m)
let maxUndefined m = N.maxUndefined (data m)

let forEach d f =
  N.forEach (data d) f     
let reduce d acc cb = 
  N.reduce (data d) acc cb 
let every d p = 
  N.every (data d) p 
let some d  p = 
  N.some (data d) p       
let size d = 
  N.size (data d)
let toList d =
  N.toList (data d)
let toArray d = 
  N.toArray (data d)
let keysToArray d =   
  N.keysToArray (data d)
let valuesToArray d =   
  N.valuesToArray (data d)
let ofSortedArrayUnsafe ~dict xs : _ t =
  t ~data:(N.ofSortedArrayUnsafe xs) ~dict   
let checkInvariantInternal d = 
  N.checkInvariantInternal (data d)  
let cmp (type k)  (type id) (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) cmp = 
  let module X = (val dict m1) in   
  N.cmp ~kcmp:X.cmp ~vcmp:cmp (data m1) (data m2)
let eq (type k) (type id) (m1 : (k,'v,id) t) (m2 : (k,'v,id) t) cmp = 
  let module M = (val dict m1) in 
  N.eq ~kcmp:M.cmp ~veq:cmp (data m1) (data m2)
let map m f = 
  t ~dict:(dict m) ~data:(N.map (data m) f)
let mapWithKey m  f = 
  t ~dict:(dict m) ~data:(N.mapWithKey (data m) f)
let get (type k) (type id) (m : (k,_,id) t) x  = 
  let module X = (val dict m) in 
  N.get ~cmp:X.cmp  (data m) x 
let getUndefined (type k) (type id) (m : (k,_,id) t) x = 
  let module X = (val dict m) in 
  N.getUndefined ~cmp:X.cmp  (data m) x
let getWithDefault (type k) (type id)  (m : (k,_,id) t) x def = 
  let module X = (val dict m) in 
  N.getWithDefault ~cmp:X.cmp (data m) x  def  
let getExn (type k)  (type id)  (m : (k,_,id) t) x = 
  let module X = (val dict m) in 
  N.getExn ~cmp:X.cmp (data m) x 
let has (type k) (type id)  (m : (k,_,id) t) x = 
  let module X = (val dict m) in 
  N.has ~cmp:X.cmp (data m) x
let ofArray (type k) (type id) data ~(dict : (k,id) Bs_Cmp.t)= 
  let module M = (val dict ) in 
  t ~dict  ~data:(N.ofArray ~cmp:M.cmp data)  
let set (type elt) (type id) (m : (elt,_,id) t) e v = 
  let module M = (val dict m) in
  let oldRoot = data m in 
  let newRoot = N.updateMutate ~cmp:M.cmp oldRoot e v in 
  if newRoot != oldRoot then 
    dataSet m newRoot

let mergeArrayAux t  xs ~cmp =     
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    let key,value = A.getUnsafe xs i in 
    v := N.updateMutate !v key value ~cmp
  done; 
  !v 
let mergeMany (type elt) (type id) (d : (elt,_,id) t ) xs =   
  let module M = (val dict d) in
  let oldRoot = data d in 
  let newRoot = mergeArrayAux oldRoot xs ~cmp:M.cmp in 
  if newRoot != oldRoot then 
    dataSet d newRoot 

