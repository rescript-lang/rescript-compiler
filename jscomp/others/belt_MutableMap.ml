
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

module Int = Belt_MutableMapInt
module String = Belt_MutableMapString
    
module N = Belt_internalAVLtree
module A = Belt_Array


type ('key, 'id) id = ('key, 'id) Belt_Id.comparable
type ('key, 'id ) cmp = ('key, 'id) Belt_Id.cmp
    
module S = struct    
  type ('k, 'v, 'id) t = {
    cmp: ('k, 'id) cmp;
    mutable data: ('k, 'v) N.t
  } [@@bs.deriving abstract]
end

type ('k, 'v, 'id) t = ('k, 'v, 'id) S.t

let rec removeMutateAux nt x ~cmp = 
  let k = N.key nt in 
  let c = (Belt_Id.getCmpInternal cmp) x k [@bs] in 
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

let remove d k =  
  let oldRoot = S.data d in   
  match N.toOpt oldRoot with 
  | None -> ()
  | Some oldRoot2 ->
    let newRoot = removeMutateAux ~cmp:(S.cmp d) oldRoot2 k in 
    if newRoot != oldRoot then 
      S.dataSet d newRoot    


let rec removeArrayMutateAux t xs i len ~cmp  =  
  if i < len then 
    let ele = A.getUnsafe xs i in 
    let u = removeMutateAux t ele ~cmp in 
    match N.toOpt u with 
    | None -> N.empty
    | Some t -> removeArrayMutateAux t xs (i+1) len ~cmp 
  else N.return t    

let removeMany d xs =  
  let oldRoot = S.data d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    let newRoot = removeArrayMutateAux nt xs 0 len ~cmp:(S.cmp d) in 
    if newRoot != oldRoot then 
      S.dataSet d newRoot


let rec updateDone t x   f  ~cmp =   
  match N.toOpt t with 
  | None ->
    (match f None [@bs] with
    | Some data -> N.singleton x data
    | None -> t)
  | Some nt -> 
    let k = N.key nt in 
    let  c = (Belt_Id.getCmpInternal cmp) x k [@bs] in  
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
      
let updateU t  x f =       
  let oldRoot = S.data t in 
  let newRoot = updateDone oldRoot x f ~cmp:(S.cmp t) in 
  if  newRoot != oldRoot then 
    S.dataSet t newRoot 
let update t x f = updateU t x (fun [@bs] a -> f a)
    
let make (type key) (type identity) ~(id : (key,identity) id) =
  let module M = (val id) in 
  S.t ~cmp:M.cmp ~data:N.empty

let clear m = S.dataSet m N.empty
    
let isEmpty d = 
  N.isEmpty (S.data d)
    

let minKey m = N.minKey (S.data m)
let minKeyUndefined m = N.minKeyUndefined (S.data m)
let maxKey m = N.maxKey (S.data m)
let maxKeyUndefined m = N.maxKeyUndefined (S.data m)
let minimum m = N.minimum (S.data m)
let minUndefined m = N.minUndefined (S.data m) 
let maximum m = N.maximum (S.data m)
let maxUndefined m = N.maxUndefined (S.data m)

let forEachU d f = N.forEachU (S.data d) f
let forEach d f = forEachU d (fun [@bs] a b -> f a b)
let reduceU d acc cb = N.reduceU (S.data d) acc cb
let reduce d acc cb = reduceU d acc (fun[@bs] a b c -> cb a b c)
let everyU d p = N.everyU (S.data d) p
let every d p = everyU d (fun[@bs] a b -> p a b)
let someU d p = N.someU (S.data d) p       
let some d p = someU d (fun [@bs] a b -> p a b)
let size d = 
  N.size (S.data d)
let toList d =
  N.toList (S.data d)
let toArray d = 
  N.toArray (S.data d)
let keysToArray d =   
  N.keysToArray (S.data d)
let valuesToArray d =   
  N.valuesToArray (S.data d)
    
let fromSortedArrayUnsafe (type key) (type identity) ~(id : (key,identity) id) xs : _ t =
  let module M = (val id) in 
  S.t ~data:(N.fromSortedArrayUnsafe xs) ~cmp:M.cmp
    
let checkInvariantInternal d = 
  N.checkInvariantInternal (S.data d)  

let cmpU m1 m2 cmp = 
  N.cmpU ~kcmp:(S.cmp m1) ~vcmp:cmp (S.data m1) (S.data m2)
let cmp m1 m2 cmp = cmpU m1 m2 (fun[@bs] a b -> cmp a b)
    
let eqU m1 m2 cmp = 
  N.eqU ~kcmp:(S.cmp m1) ~veq:cmp (S.data m1) (S.data m2)
let eq m1 m2 cmp = eqU m1 m2 (fun[@bs] a b -> cmp a b)

let mapU m f = 
  S.t ~cmp:(S.cmp m) ~data:(N.mapU (S.data m) f)
let map m f = mapU m (fun[@bs] a  -> f a )    
let mapWithKeyU m f = 
  S.t ~cmp:(S.cmp m) ~data:(N.mapWithKeyU (S.data m) f)
let mapWithKey m f = mapWithKeyU m (fun [@bs] a b -> f a b)     
let get m x  = 
  N.get ~cmp:(S.cmp m)  (S.data m) x
    
let getUndefined m x = 
  N.getUndefined ~cmp:(S.cmp m)  (S.data m) x
    
let getWithDefault m x def = 
  N.getWithDefault ~cmp:(S.cmp m) (S.data m) x  def
    
let getExn m x = 
  N.getExn ~cmp:(S.cmp m) (S.data m) x
    
let has m x = 
  N.has ~cmp:(S.cmp m) (S.data m) x
    
let fromArray (type k) (type identity) data ~(id : (k,identity) id)= 
  let module M = (val id ) in
  let cmp = M.cmp in 
  S.t ~cmp  ~data:(N.fromArray ~cmp data)
  
let set  m e v = 
  let oldRoot = S.data m in 
  let newRoot = N.updateMutate ~cmp:(S.cmp m) oldRoot e v in 
  if newRoot != oldRoot then 
    S.dataSet m newRoot

let mergeManyAux t  xs ~cmp =     
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    let key,value = A.getUnsafe xs i in 
    v := N.updateMutate !v key value ~cmp
  done; 
  !v 

let mergeMany d xs =   
  let oldRoot = S.data d in 
  let newRoot = mergeManyAux oldRoot xs ~cmp:(S.cmp d) in 
  if newRoot != oldRoot then 
    S.dataSet d newRoot


