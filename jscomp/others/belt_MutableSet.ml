
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

module Int = Belt_MutableSetInt
module String = Belt_MutableSetString

module N = Belt_internalAVLset
module A = Belt_Array
module Sort = Belt_SortArray


type ('k, 'id) id = ('k, 'id) Belt_Id.comparable
type ('key, 'id ) cmp = ('key, 'id) Belt_Id.cmp

module S = struct
  type ('value,'id) t =
    {
      cmp: ('value, 'id) cmp;
      mutable data: 'value N.t
    } [@@bs.deriving abstract]
end

type ('k, 'id) t = ('k, 'id) S.t


let rec remove0 nt x ~cmp = 
  let k = N.value nt in 
  let c = cmp x k [@bs] in 
  if c = 0 then 
    let l,r = N.(left nt, right nt) in       
    match N.(toOpt l, toOpt r) with 
    | None, _ -> r 
    | _, None -> l 
    | Some _,  Some nr ->  
      N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
      N.return (N.balMutate nt)
   else 
    begin 
      if c < 0 then 
        match N.toOpt (N.left nt) with         
        | None -> N.return nt 
        | Some l ->
          N.leftSet nt (remove0 ~cmp l x );
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.right nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (remove0 ~cmp r x);
          N.return (N.balMutate nt)
    end

let remove  d  v =  
  let oldRoot = S.data d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some oldRoot2 ->
    let newRoot = remove0 ~cmp:(Belt_Id.getCmpInternal (S.cmp d)) oldRoot2 v in 
    if newRoot != oldRoot then 
      S.dataSet d newRoot    


let rec removeMany0 t xs i len ~cmp  =  
  if i < len then 
    let ele = A.getUnsafe xs i in 
    let u = remove0 t ele ~cmp in 
    match N.toOpt u with 
    | None -> N.empty
    | Some t -> removeMany0 t xs (i+1) len ~cmp 
  else N.return t    

let removeMany d xs =  
  let oldRoot = S.data d in 
  match N.toOpt oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    S.dataSet d 
      (removeMany0 nt xs 0 len 
        ~cmp:(Belt_Id.getCmpInternal (S.cmp d)))


let rec removeCheck0  nt x removed ~cmp= 
  let k = N.value nt in 
  let c = (Belt_Id.getCmpInternal cmp) x k [@bs] in 
  if c = 0 then 
    let () = removed := true in  
    let l,r = N.(left nt, right nt) in       
    match N.(toOpt l, toOpt r) with 
    | None, _ -> r 
    | _, None -> l  
    | Some _,  Some nr ->  
      N.rightSet nt (N.removeMinAuxWithRootMutate nt nr);
      N.return (N.balMutate nt)
  else 
    begin 
      if c < 0 then 
        match N.toOpt (N.left nt) with         
        | None -> N.return nt 
        | Some l ->
          N.leftSet nt (removeCheck0 ~cmp l x removed);
          N.return (N.balMutate nt)
      else 
        match N.toOpt (N.right nt) with 
        | None -> N.return nt 
        | Some r -> 
          N.rightSet nt (removeCheck0 ~cmp r x removed);
          N.return (N.balMutate nt)
    end



let removeCheck d v =  
  let oldRoot = S.data d in 
  match N.toOpt oldRoot with 
  | None -> false 
  | Some oldRoot2 ->
    let removed = ref false in 
    let newRoot = removeCheck0 ~cmp:(S.cmp d) oldRoot2 v removed in 
    if newRoot != oldRoot then  
      S.dataSet d newRoot ;   
    !removed



let rec addCheck0  t x added ~cmp  =   
  match N.toOpt t with 
  | None -> 
    added := true;
    N.singleton x 
  | Some nt -> 
    let k = N.value nt in 
    let c = cmp x k [@bs] in  
    if c = 0 then t 
    else
      let l, r = N.(left nt, right nt) in 
      (if c < 0 then                   
         let ll = addCheck0 ~cmp l x added in
         N.leftSet nt ll
       else   
         N.rightSet nt (addCheck0 ~cmp r x added );
      );
      N.return (N.balMutate nt)

let addCheck m e = 
  let oldRoot = S.data m in 
  let added = ref false in 
  let newRoot = addCheck0 ~cmp:(Belt_Id.getCmpInternal (S.cmp m)) oldRoot e added in 
  if newRoot != oldRoot then 
    S.dataSet m newRoot;
  !added    

let add m e = 
  let oldRoot = S.data m in 
  let newRoot = N.addMutate ~cmp:(S.cmp m) oldRoot e  in 
  if newRoot != oldRoot then 
    S.dataSet m newRoot

let addArrayMutate t xs ~cmp =     
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    v := N.addMutate !v (A.getUnsafe xs i)  ~cmp
  done; 
  !v
    
let mergeMany d xs =   
  S.dataSet d (addArrayMutate (S.data d) xs ~cmp:(S.cmp d))


let make (type value) (type identity) ~(id : (value, identity) id) =
  let module M = (val id) in 
  S.t ~cmp:M.cmp ~data:N.empty
    
let isEmpty d = 
  N.isEmpty (S.data d)
    
let minimum d = 
  N.minimum (S.data d)    
let minUndefined d =
  N.minUndefined (S.data d)
let maximum d = 
  N.maximum (S.data d)
let maxUndefined d =
  N.maxUndefined (S.data d)

let forEachU d f = N.forEachU (S.data d) f
let forEach d f = forEachU d (fun[@bs] a -> f a)
let reduceU d acc cb = N.reduceU (S.data d) acc cb
let reduce d acc cb = reduceU d acc (fun[@bs] a b -> cb a b)    
let everyU d p = N.everyU (S.data d) p
let every d p = everyU d (fun[@bs] a -> p a)    
let someU d p = N.someU (S.data d) p
let some d p = someU d (fun[@bs] a -> p a)    
let size d = 
  N.size (S.data d)
let toList d =
  N.toList (S.data d)
let toArray d = 
  N.toArray (S.data d)

let fromSortedArrayUnsafe (type value) (type identity) xs ~(id : (value,identity) id) : _ t =
  let module M = (val id) in 
  S.t ~data:(N.fromSortedArrayUnsafe xs) ~cmp:M.cmp

let checkInvariantInternal d = 
  N.checkInvariantInternal (S.data d)
    
    
let fromArray (type value) (type identity)  data ~(id : (value,identity) id) =
  let module M = (val id) in
  let cmp = M.cmp in 
  S.t ~cmp ~data:(N.fromArray ~cmp data)


let cmp d0 d1 = 
  N.cmp ~cmp:(S.cmp d0) (S.data d0) (S.data d1)

let eq d0  d1 = 
  N.eq ~cmp:(S.cmp d0) (S.data d0) (S.data d1)
    
let get d x = 
  N.get ~cmp:(S.cmp d) (S.data d) x
    
let getUndefined  d x = 
  N.getUndefined ~cmp:(S.cmp d) (S.data d) x
    
let getExn d x = 
  N.getExn ~cmp:(S.cmp d) (S.data d) x
    

let split d  key  =     
  let arr = N.toArray (S.data d) in
  let cmp = S.cmp d in 
  let i = Sort.binarySearchByU arr key (Belt_Id.getCmpInternal cmp)  in   
  let len = A.length arr in 
  if i < 0 then 
    let next = - i -1 in 
    (S.t 
       ~data:(N.fromSortedArrayAux arr 0 next)
       ~cmp
     , 
     S.t 
       ~data:(N.fromSortedArrayAux arr next (len - next))
       ~cmp
    ), false
  else 
    (S.t 
       ~data:(N.fromSortedArrayAux arr 0 i)
       ~cmp,
     S.t 
       ~data:(N.fromSortedArrayAux arr (i+1) (len - i - 1))
       ~cmp
    ), true       

let keepU d p = 
  S.t ~data:(N.keepCopyU (S.data d) p ) ~cmp:(S.cmp d)

let keep d p = keepU d (fun[@bs] a -> p a)
    
let partitionU d p = 
  let cmp = S.cmp d in 
  let a, b = N.partitionCopyU (S.data d) p in 
  S.t ~data:a ~cmp, S.t ~data:b ~cmp  

let partition d p = partitionU d (fun[@bs] a -> p a)

let subset a b = 
  N.subset  ~cmp:(S.cmp a) (S.data a) (S.data b)

let intersect a b  : _ t = 
  let cmp = S.cmp a  in 
  match N.toOpt (S.data a), N.toOpt (S.data b) with 
  | None, _ -> S.t ~cmp ~data:N.empty
  | _, None -> S.t ~cmp ~data:N.empty
  | Some dataa0, Some datab0 ->  
    let sizea, sizeb = 
      N.lengthNode dataa0, N.lengthNode datab0 in          
    let totalSize = sizea + sizeb in 
    let tmp = A.makeUninitializedUnsafe totalSize in 
    ignore @@ N.fillArray dataa0 0 tmp ; 
    ignore @@ N.fillArray datab0 sizea tmp;
    let p = Belt_Id.getCmpInternal cmp in 
    if (p (A.getUnsafe tmp (sizea - 1))
          (A.getUnsafe tmp sizea) [@bs] < 0)
       || 
       (p 
          (A.getUnsafe tmp (totalSize - 1))
          (A.getUnsafe tmp 0) [@bs] < 0 
       )
    then S.t ~cmp ~data:N.empty
    else 
      let tmp2 = A.makeUninitializedUnsafe (min sizea sizeb) in 
      let k = Sort.intersectU tmp 0 sizea tmp sizea sizeb tmp2 0 p in 
      S.t ~data:(N.fromSortedArrayAux tmp2 0 k)
        ~cmp
        
let diff a b : _ t = 
  let cmp = S.cmp a in 
  let dataa = S.data a in 
  match N.toOpt dataa, N.toOpt (S.data b) with 
  | None, _ -> S.t ~cmp ~data:N.empty
  | _, None -> 
    S.t ~data:(N.copy dataa) ~cmp
  | Some dataa0, Some datab0
    -> 
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in  
    let totalSize = sizea + sizeb in 
    let tmp = A.makeUninitializedUnsafe totalSize in 
    ignore @@ N.fillArray dataa0 0 tmp ; 
    ignore @@ N.fillArray datab0 sizea tmp;
    let p = Belt_Id.getCmpInternal cmp in 
    if (p (A.getUnsafe tmp (sizea - 1))
          (A.getUnsafe tmp sizea) [@bs] < 0)
       || 
       (p 
          (A.getUnsafe tmp (totalSize - 1))
          (A.getUnsafe tmp 0) [@bs] < 0 
       )
    then S.t ~data:(N.copy dataa) ~cmp
    else 
      let tmp2 = A.makeUninitializedUnsafe sizea in 
      let k = Sort.diffU tmp 0 sizea tmp sizea sizeb tmp2 0 p in 
      S.t ~data:(N.fromSortedArrayAux tmp2 0 k) ~cmp

let union a b = 
  let cmp = S.cmp a in 
  let dataa, datab =  S.data a, S.data b  in 
  match N.toOpt dataa, N.toOpt datab with 
  | None, _ -> S.t ~data:(N.copy datab) ~cmp
  | _, None -> S.t ~data:(N.copy dataa) ~cmp
  | Some dataa0, Some datab0 
    -> 
    let sizea, sizeb = N.lengthNode dataa0, N.lengthNode datab0 in 
    let totalSize = sizea + sizeb in 
    let tmp = A.makeUninitializedUnsafe totalSize in 
    ignore @@ N.fillArray dataa0 0 tmp ;
    ignore @@ N.fillArray datab0 sizea tmp ;
    let p = (Belt_Id.getCmpInternal cmp)  in 
    if p
        (A.getUnsafe tmp (sizea - 1))
        (A.getUnsafe tmp sizea) [@bs] < 0 then 
      S.t ~data:(N.fromSortedArrayAux tmp 0 totalSize) ~cmp
    else   
      let tmp2 = A.makeUninitializedUnsafe totalSize in 
      let k = Sort.unionU tmp 0 sizea tmp sizea sizeb tmp2 0 p in 
      S.t ~data:(N.fromSortedArrayAux tmp2 0 k) ~cmp
      
let has d x =
  N.has ~cmp:(S.cmp d) (S.data d) x

let copy d = S.t ~data:(N.copy (S.data d)) ~cmp:(S.cmp d)


