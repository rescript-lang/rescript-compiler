
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
  let k = N.keyGet nt in 
  let c = (Belt_Id.getCmpInternal cmp) x k [@bs] in 
  if c = 0 then 
    let l,r = N.(leftGet nt, rightGet nt) in       
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
        match N.toOpt (N.leftGet nt) with         
        | None -> N.return nt 
        | Some l ->
          N.leftSet nt (removeMutateAux ~cmp l x );
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.rightGet nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (removeMutateAux ~cmp r x);
          N.return (N.balMutate nt)
    end    

let remove d k =  
  let oldRoot = S.dataGet d in   
  match N.toOpt oldRoot with 
  | None -> ()
  | Some oldRoot2 ->
    let newRoot = removeMutateAux ~cmp:(S.cmpGet d) oldRoot2 k in 
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
  let oldRoot = S.dataGet d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    let newRoot = removeArrayMutateAux nt xs 0 len ~cmp:(S.cmpGet d) in 
    if newRoot != oldRoot then 
      S.dataSet d newRoot


let rec updateDone t x   f  ~cmp =   
  match N.toOpt t with 
  | None ->
    (match f None [@bs] with
    | Some data -> N.singleton x data
    | None -> t)
  | Some nt -> 
    let k = N.keyGet nt in 
    let  c = (Belt_Id.getCmpInternal cmp) x k [@bs] in  
    if c = 0 then begin     
      match f (Some (N.valueGet nt)) [@bs] with
      | None ->
        let l,r = N.leftGet nt, N.rightGet nt in
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
      let l, r = N.(leftGet nt, rightGet nt) in 
      (if c < 0 then                   
         let ll = updateDone  l x f ~cmp in
         N.leftSet nt ll
       else   
         N.rightSet nt (updateDone  r x f ~cmp);
      );
      N.return (N.balMutate nt)  
      
let updateU t  x f =       
  let oldRoot = S.dataGet t in 
  let newRoot = updateDone oldRoot x f ~cmp:(S.cmpGet t) in 
  if  newRoot != oldRoot then 
    S.dataSet t newRoot 
let update t x f = updateU t x (fun [@bs] a -> f a)
    
let make (type key) (type identity) ~(id : (key,identity) id) =
  let module M = (val id) in 
  S.t ~cmp:M.cmp ~data:N.empty

let clear m = S.dataSet m N.empty
    
let isEmpty d = 
  N.isEmpty (S.dataGet d)
    

let minKey m = N.minKey (S.dataGet m)
let minKeyUndefined m = N.minKeyUndefined (S.dataGet m)
let maxKey m = N.maxKey (S.dataGet m)
let maxKeyUndefined m = N.maxKeyUndefined (S.dataGet m)
let minimum m = N.minimum (S.dataGet m)
let minUndefined m = N.minUndefined (S.dataGet m) 
let maximum m = N.maximum (S.dataGet m)
let maxUndefined m = N.maxUndefined (S.dataGet m)

let forEachU d f = N.forEachU (S.dataGet d) f
let forEach d f = forEachU d (fun [@bs] a b -> f a b)
let reduceU d acc cb = N.reduceU (S.dataGet d) acc cb
let reduce d acc cb = reduceU d acc (fun[@bs] a b c -> cb a b c)
let everyU d p = N.everyU (S.dataGet d) p
let every d p = everyU d (fun[@bs] a b -> p a b)
let someU d p = N.someU (S.dataGet d) p       
let some d p = someU d (fun [@bs] a b -> p a b)
let size d = 
  N.size (S.dataGet d)
let toList d =
  N.toList (S.dataGet d)
let toArray d = 
  N.toArray (S.dataGet d)
let keysToArray d =   
  N.keysToArray (S.dataGet d)
let valuesToArray d =   
  N.valuesToArray (S.dataGet d)
    
let fromSortedArrayUnsafe (type key) (type identity) ~(id : (key,identity) id) xs : _ t =
  let module M = (val id) in 
  S.t ~data:(N.fromSortedArrayUnsafe xs) ~cmp:M.cmp
    
let checkInvariantInternal d = 
  N.checkInvariantInternal (S.dataGet d)  

let cmpU m1 m2 cmp = 
  N.cmpU ~kcmp:(S.cmpGet m1) ~vcmp:cmp (S.dataGet m1) (S.dataGet m2)
let cmp m1 m2 cmp = cmpU m1 m2 (fun[@bs] a b -> cmp a b)
    
let eqU m1 m2 cmp = 
  N.eqU ~kcmp:(S.cmpGet m1) ~veq:cmp (S.dataGet m1) (S.dataGet m2)
let eq m1 m2 cmp = eqU m1 m2 (fun[@bs] a b -> cmp a b)

let mapU m f = 
  S.t ~cmp:(S.cmpGet m) ~data:(N.mapU (S.dataGet m) f)
let map m f = mapU m (fun[@bs] a  -> f a )    
let mapWithKeyU m f = 
  S.t ~cmp:(S.cmpGet m) ~data:(N.mapWithKeyU (S.dataGet m) f)
let mapWithKey m f = mapWithKeyU m (fun [@bs] a b -> f a b)     
let get m x  = 
  N.get ~cmp:(S.cmpGet m)  (S.dataGet m) x
    
let getUndefined m x = 
  N.getUndefined ~cmp:(S.cmpGet m)  (S.dataGet m) x
    
let getWithDefault m x def = 
  N.getWithDefault ~cmp:(S.cmpGet m) (S.dataGet m) x  def
    
let getExn m x = 
  N.getExn ~cmp:(S.cmpGet m) (S.dataGet m) x
    
let has m x = 
  N.has ~cmp:(S.cmpGet m) (S.dataGet m) x
    
let fromArray (type k) (type identity) data ~(id : (k,identity) id)= 
  let module M = (val id ) in
  let cmp = M.cmp in 
  S.t ~cmp  ~data:(N.fromArray ~cmp data)
  
let set  m e v = 
  let oldRoot = S.dataGet m in 
  let newRoot = N.updateMutate ~cmp:(S.cmpGet m) oldRoot e v in 
  if newRoot != oldRoot then 
    S.dataSet m newRoot

let mergeManyAux t  xs ~cmp =     
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    let key,value = A.getUnsafe xs i in 
    v .contents<- N.updateMutate v.contents key value ~cmp
  done; 
  v.contents 

let mergeMany d xs =   
  let oldRoot = S.dataGet d in 
  let newRoot = mergeManyAux oldRoot xs ~cmp:(S.cmpGet d) in 
  if newRoot != oldRoot then 
    S.dataSet d newRoot


