
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


module N = Bs_internalAVLset
module A = Bs_Array
module S = Bs_Sort 

type ('k,'id) t0 = 'k  N.t

type ('k, 'id) dict = ('k, 'id) Bs_Cmp.t 

type ('elt,'id) t =
  {
    dict: ('elt, 'id) dict;
    mutable data: 'elt N.t
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

let remove (type elt) (type id) (d : (elt,id) t) v =  
  let oldRoot = data d in 
  let module M = (val dict d) in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some oldRoot2 ->
    let newRoot = removeMutateAux ~cmp:M.cmp oldRoot2 v in 
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

let removeMany (type elt) (type id) (d : (elt,id) t) xs =  
  let oldRoot = data d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    let module M = (val dict d) in 
    let newRoot = removeArrayMutateAux nt xs 0 len ~cmp:M.cmp in 
    if newRoot != oldRoot then 
      dataSet d newRoot


let rec removeMutateCheckAux  nt x removed ~cmp= 
  let k = N.key nt in 
  let c = (Bs_Cmp.getCmpIntenral cmp) x k [@bs] in 
  if c = 0 then 
    let () = removed := true in  
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
          N.leftSet nt (removeMutateCheckAux ~cmp l x removed);
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.right nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (removeMutateCheckAux ~cmp r x removed);
          N.return (N.balMutate nt)
    end



let removeCheck (type elt) (type id) (d : (elt,id) t) v =  
  let oldRoot = data d in 
  match N.toOpt oldRoot with 
  | None -> false 
  | Some oldRoot2 ->
    let module M = (val dict d) in
    let removed = ref false in 
    let newRoot = removeMutateCheckAux ~cmp:M.cmp oldRoot2 v removed in 
    if newRoot != oldRoot then  
      dataSet d newRoot ;   
    !removed



let rec addMutateCheckAux  (t : _ t0) x added ~cmp  =   
  match N.toOpt t with 
  | None -> 
    added := true;
    N.singleton x 
  | Some nt -> 
    let k = N.key nt in 
    let  c = (Bs_Cmp.getCmpIntenral cmp) x k [@bs] in  
    if c = 0 then t 
    else
      let l, r = N.(left nt, right nt) in 
      (if c < 0 then                   
         let ll = addMutateCheckAux ~cmp l x added in
         N.leftSet nt ll
       else   
         N.rightSet nt (addMutateCheckAux ~cmp r x added );
      );
      N.return (N.balMutate nt)

let addCheck (type elt) (type id) (m : (elt,id) t) e = 
  let dict, oldRoot = (dict m, data m) in 
  let module M = (val dict) in 
  let added = ref false in 
  let newRoot = addMutateCheckAux ~cmp:M.cmp oldRoot e added in 
  if newRoot != oldRoot then 
    dataSet m newRoot;
  !added    


let split (type elt) (type id) (d : (elt,id) t)  key  =     
  let module M = (val dict d ) in
  let arr = N.toArray (data d) in 
  let i = S.binarySearchBy arr key (Bs_Cmp.getCmpIntenral M.cmp)  in   
  let len = A.length arr in 
  if i < 0 then 
    let next = - i -1 in 
    (t 
       ~data:(N.ofSortedArrayAux arr 0 next)
       ~dict:(module M)
     , 
     t 
       ~data:(N.ofSortedArrayAux arr next (len - next))
       ~dict:(module M)
    ), false
  else 
    (t 
       ~data:(N.ofSortedArrayAux arr 0 i)
       ~dict:(module M),
     t 
       ~data:(N.ofSortedArrayAux arr (i+1) (len - i - 1))
       ~dict:(module M)
    ), true       

let keepBy d p = 
  t ~data:(N.filterCopy (data d) p ) ~dict:(dict d) 
let partition d p = 
  let dict = dict d in 
  let a, b = N.partitionCopy (data d) p in 
  t ~data:a ~dict, t ~data:b ~dict      

let empty ~dict = 
  t ~dict ~data:N.empty
    
let isEmpty d = 
  N.isEmpty (data d)
    
let singleton x ~dict = 
  t ~data:(N.singleton x) ~dict 
let minimum d = 
  N.minimum (data d)
let minUndefined d =
  N.minUndefined (data d)
let maximum d = 
  N.maximum (data d)
let maxUndefined d =
  N.maxUndefined (data d)
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
let ofSortedArrayUnsafe xs ~dict : _ t =
  t ~data:(N.ofSortedArrayUnsafe xs) ~dict   
let checkInvariantInternal d = 
  N.checkInvariantInternal (data d)
let cmp (type elt) (type id) (d0 : (elt,id) t) d1 = 
  let module M = (val dict d0) in 
  N.cmp ~cmp:M.cmp (data d0) (data d1)
let eq (type elt) (type id) (d0 : (elt,id) t)  d1 = 
  let module M = (val dict d0) in 
  N.eq ~cmp:M.cmp (data d0) (data d1)
let get (type elt) (type id) (d : (elt,id) t) x = 
  let module M = (val dict d) in 
  N.get ~cmp:M.cmp (data d) x 
let getUndefined (type elt) (type id) (d : (elt,id) t) x = 
  let module M = (val dict d) in 
  N.getUndefined ~cmp:M.cmp (data d) x
let getExn (type elt) (type id) (d : (elt,id) t) x = 
  let module M = (val dict d) in 
  N.getExn ~cmp:M.cmp (data d) x     
let has (type elt) (type id) (d : (elt,id) t) x =
  let module M = (val dict d) in 
  N.has ~cmp:M.cmp (data d) x   
let ofArray (type elt) (type id) data ~(dict : (elt,id) dict) =  
  let module M = (val dict) in 
  t ~dict ~data:(N.ofArray ~cmp:M.cmp data)
let add (type elt) (type id) (m : (elt,id) t) e = 
  let module M = (val dict m) in
  let oldRoot = (data m) in 
  let newRoot = N.addMutate ~cmp:M.cmp oldRoot e  in 
  if newRoot != oldRoot then 
    dataSet m newRoot

let addArrayMutate (t : _ t0) xs ~cmp =     
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    v := N.addMutate !v (A.getUnsafe xs i)  ~cmp
  done; 
  !v
    
let mergeMany (type elt) (type id) (d : (elt,id) t ) xs =   
  let module M = (val dict d) in
  dataSet d (addArrayMutate (data d) xs ~cmp:M.cmp)







let subset (type elt) (type id) (a : (elt,id) t) b = 
  let module M = (val dict a) in 
  N.subset  ~cmp:M.cmp (data a) (data b)

let intersect (type elt) (type id) (a : (elt,id) t) b  : _ t = 
  (* let dict, dataa, datab = dict a, data a, data b in  *)
  let module M = (val dict a) in 
  match N.toOpt (data a), N.toOpt (data b) with 
  | None, _ -> empty (module M)
  | _, None -> empty (module M)
  | Some dataa0, Some datab0 ->  
    let sizea, sizeb = 
      N.lengthNode dataa0, N.lengthNode datab0 in          
    let totalSize = sizea + sizeb in 
    let tmp = A.makeUninitializedUnsafe totalSize in 
    ignore @@ N.fillArray dataa0 0 tmp ; 
    ignore @@ N.fillArray datab0 sizea tmp;
    let p = Bs_Cmp.getCmpIntenral M.cmp in 
    if (p (A.getUnsafe tmp (sizea - 1))
          (A.getUnsafe tmp sizea) [@bs] < 0)
       || 
       (p 
          (A.getUnsafe tmp (totalSize - 1))
          (A.getUnsafe tmp 0) [@bs] < 0 
       )
    then empty (module M)
    else 
      let tmp2 = A.makeUninitializedUnsafe (min sizea sizeb) in 
      let k = S.intersect tmp 0 sizea tmp sizea sizeb tmp2 0 p in 
      t ~data:(N.ofSortedArrayAux tmp2 0 k)
        ~dict:(module M)
        
let diff (type elt) (type id) (a : (elt,id) t) b : _ t = 
  let module M = (val dict a) in
  let dataa = data a in 
  match N.toOpt dataa, N.toOpt (data b) with 
  | None, _ -> empty (module M)
  | _, None -> 
    t ~data:(N.copy dataa) ~dict:(module M) 
  | Some dataa0, Some datab0
    -> 
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in  
    let totalSize = sizea + sizeb in 
    let tmp = A.makeUninitializedUnsafe totalSize in 
    ignore @@ N.fillArray dataa0 0 tmp ; 
    ignore @@ N.fillArray datab0 sizea tmp;
    let p = Bs_Cmp.getCmpIntenral M.cmp in 
    if (p (A.getUnsafe tmp (sizea - 1))
          (A.getUnsafe tmp sizea) [@bs] < 0)
       || 
       (p 
          (A.getUnsafe tmp (totalSize - 1))
          (A.getUnsafe tmp 0) [@bs] < 0 
       )
    then t ~data:(N.copy dataa) ~dict:(module M) 
    else 
      let tmp2 = A.makeUninitializedUnsafe sizea in 
      let k = S.diff tmp 0 sizea tmp sizea sizeb tmp2 0 p in 
      t ~data:(N.ofSortedArrayAux tmp2 0 k) ~dict:(module M) 

let union (type elt) (type id) (a : (elt,id) t) b = 
  let module M = (val dict a) in
  let dataa, datab =  data a, data b  in 
  match N.toOpt dataa, N.toOpt datab with 
  | None, _ -> t ~data:(N.copy datab) ~dict:(module M) 
  | _, None -> t ~data:(N.copy dataa) ~dict:(module M) 
  | Some dataa0, Some datab0 
    -> 
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in 
    let totalSize = sizea + sizeb in 
    let tmp = A.makeUninitializedUnsafe totalSize in 
    ignore @@ N.fillArray dataa0 0 tmp ;
    ignore @@ N.fillArray datab0 sizea tmp ;
    let p = (Bs_Cmp.getCmpIntenral M.cmp)  in 
    if p
        (A.getUnsafe tmp (sizea - 1))
        (A.getUnsafe tmp sizea) [@bs] < 0 then 
      t ~data:(N.ofSortedArrayAux tmp 0 totalSize) ~dict:(module M) 
    else   
      let tmp2 = A.makeUninitializedUnsafe totalSize in 
      let k = S.union tmp 0 sizea tmp sizea sizeb tmp2 0 p in 
      t ~data:(N.ofSortedArrayAux tmp2 0 k) ~dict:(module M) 

let copy d = t ~data:(N.copy (data d)) ~dict:(dict d)
