
(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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


type ('value,'id) t =
  {
    cmp: ('value, 'id) cmp;
    mutable data: 'value N.t
  } 



let rec remove0 nt x ~cmp = 
  let k = nt.N.value in 
  let c = cmp x k [@bs] in 
  if c = 0 then 
    let {N.left = l; right = r} = nt in       
    match l, r with 
    | None, _ -> r 
    | _, None -> l 
    | Some _,  Some nr ->  
      nt.right <- (N.removeMinAuxWithRootMutate nt nr);
      Some (N.balMutate nt)
   else 
    begin 
      if c < 0 then 
        match nt.left with         
        | None -> Some nt 
        | Some l ->
          nt.left <- (remove0 ~cmp l x );
          Some (N.balMutate nt)
      else 
        match nt.right with 
        | None -> Some nt 
        | Some r -> 
          nt.right <- (remove0 ~cmp r x);
          Some (N.balMutate nt)
    end

let remove  d  v =  
  let oldRoot = d.data in 
  match oldRoot with 
  | None -> ()
  | Some oldRoot2 ->
    let newRoot = remove0 ~cmp:(Belt_Id.getCmpInternal d.cmp) oldRoot2 v in 
    if newRoot != oldRoot then 
      d.data <- newRoot    


let rec removeMany0 t xs i len ~cmp  =  
  if i < len then 
    let ele = A.getUnsafe xs i in 
    let u = remove0 t ele ~cmp in 
    match u with 
    | None -> None
    | Some t -> removeMany0 t xs (i+1) len ~cmp 
  else Some t    

let removeMany d xs =  
  let oldRoot = d.data in 
  match oldRoot with 
  | None -> ()
  | Some nt -> 
    let len = A.length xs in 
    d.data <- 
      (removeMany0 nt xs 0 len 
        ~cmp:(Belt_Id.getCmpInternal d.cmp))


let rec removeCheck0  nt x removed ~cmp= 
  let k = nt.N.value in 
  let c = (Belt_Id.getCmpInternal cmp) x k [@bs] in 
  if c = 0 then 
    let () = removed .contents<- true in  
    let {N.left = l; right = r} = nt in       
    match l, r with 
    | None, _ -> r 
    | _, None -> l  
    | Some _,  Some nr ->  
      nt.right <- (N.removeMinAuxWithRootMutate nt nr);
      Some (N.balMutate nt)
  else 
    begin 
      if c < 0 then 
        match nt.left with         
        | None -> Some nt 
        | Some l ->
          nt.left <- (removeCheck0 ~cmp l x removed);
          Some (N.balMutate nt)
      else 
        match nt.right with 
        | None -> Some nt 
        | Some r -> 
          nt.right <- (removeCheck0 ~cmp r x removed);
          Some (N.balMutate nt)
    end



let removeCheck d v =  
  let oldRoot = d.data in 
  match oldRoot with 
  | None -> false 
  | Some oldRoot2 ->
    let removed = ref false in 
    let newRoot = removeCheck0 ~cmp:d.cmp oldRoot2 v removed in 
    if newRoot != oldRoot then  
      d.data <- newRoot ;   
    removed.contents



let rec addCheck0  t x added ~cmp  =   
  match t with 
  | None -> 
    added .contents<- true;
    N.singleton x 
  | Some nt -> 
    let k = nt.N.value in 
    let c = cmp x k [@bs] in  
    if c = 0 then t 
    else
      let {N.left = l; right = r} = nt in 
      (if c < 0 then                   
         let ll = addCheck0 ~cmp l x added in
         nt.left <- ll
       else   
         nt.right <- (addCheck0 ~cmp r x added );
      );
      Some (N.balMutate nt)

let addCheck m e = 
  let oldRoot = m.data in 
  let added = ref false in 
  let newRoot = addCheck0 ~cmp:(Belt_Id.getCmpInternal m.cmp) oldRoot e added in 
  if newRoot != oldRoot then 
    m.data <- newRoot;
  added.contents

let add m e = 
  let oldRoot = m.data  in 
  let newRoot = N.addMutate ~cmp:m.cmp oldRoot e  in 
  if newRoot != oldRoot then 
    m.data <- newRoot

let addArrayMutate t xs ~cmp =     
  let v = ref t in 
  for i = 0 to A.length xs - 1 do 
    v .contents<- N.addMutate v.contents (A.getUnsafe xs i)  ~cmp
  done; 
  v.contents
    
let mergeMany d xs =   
  d.data <- (addArrayMutate (d.data) xs ~cmp:d.cmp)


let make (type value) (type identity) ~(id : (value, identity) id) =
  let module M = (val id) in 
  {cmp = M.cmp ; data = None}
    
let isEmpty d = 
  N.isEmpty (d.data)
    
let minimum d = 
  N.minimum (d.data)    
let minUndefined d =
  N.minUndefined (d.data)
let maximum d = 
  N.maximum (d.data)
let maxUndefined d =
  N.maxUndefined (d.data)

let forEachU d f = N.forEachU (d.data) f
let forEach d f = forEachU d (fun[@bs] a -> f a)
let reduceU d acc cb = N.reduceU (d.data) acc cb
let reduce d acc cb = reduceU d acc (fun[@bs] a b -> cb a b)    
let everyU d p = N.everyU (d.data) p
let every d p = everyU d (fun[@bs] a -> p a)    
let someU d p = N.someU (d.data) p
let some d p = someU d (fun[@bs] a -> p a)    
let size d = 
  N.size (d.data)
let toList d =
  N.toList (d.data)
let toArray d = 
  N.toArray (d.data)

let fromSortedArrayUnsafe (type value) (type identity) xs ~(id : (value,identity) id) : _ t =
  let module M = (val id) in 
  {data = (N.fromSortedArrayUnsafe xs); cmp = M.cmp}

let checkInvariantInternal d = 
  N.checkInvariantInternal (d.data)
    
    
let fromArray (type value) (type identity)  data ~(id : (value,identity) id) =
  let module M = (val id) in
  let cmp = M.cmp in 
  {cmp; data = (N.fromArray ~cmp data)}


let cmp d0 d1 = 
  N.cmp ~cmp:(d0.cmp) (d0.data) (d1.data)

let eq d0  d1 = 
  N.eq ~cmp:(d0.cmp) (d0.data) (d1.data)
    
let get d x = 
  N.get ~cmp:d.cmp d.data x
    
let getUndefined  d x = 
  N.getUndefined ~cmp:d.cmp d.data x
    
let getExn d x = 
  N.getExn ~cmp:d.cmp d.data x
    

let split d  key  =     
  let arr = N.toArray d.data in
  let cmp = d.cmp in 
  let i = Sort.binarySearchByU arr key (Belt_Id.getCmpInternal cmp)  in   
  let len = A.length arr in 
  if i < 0 then 
    let next = - i -1 in 
    ({
       data = (N.fromSortedArrayAux arr 0 next);
       cmp}
     ,      
       {data = (N.fromSortedArrayAux arr next (len - next));
        cmp}
    ), false
  else 
    (
       {data = (N.fromSortedArrayAux arr 0 i);
        cmp},
       { data = (N.fromSortedArrayAux arr (i+1) (len - i - 1));
         cmp}
     ), true       

let keepU d p = 
  {data = (N.keepCopyU d.data p );  cmp = d.cmp}

let keep d p = keepU d (fun[@bs] a -> p a)
    
let partitionU d p = 
  let cmp = d.cmp in 
  let a, b = N.partitionCopyU d.data p in 
  {data = a ; cmp}, { data = b; cmp}  

let partition d p = partitionU d (fun[@bs] a -> p a)

let subset a b = 
  N.subset  ~cmp:a.cmp a.data b.data

let intersect a b  : _ t = 
  let cmp = a.cmp in 
  match a.data, b.data with 
  | None, _ -> { cmp; data = None}
  | _, None -> { cmp; data = None}
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
    then {cmp; data = None}
    else 
      let tmp2 = A.makeUninitializedUnsafe (Pervasives.min sizea sizeb) in 
      let k = Sort.intersectU tmp 0 sizea tmp sizea sizeb tmp2 0 p in 
      {data = (N.fromSortedArrayAux tmp2 0 k);
       cmp}
        
let diff a b : _ t = 
  let cmp = a.cmp in 
  let dataa = a.data in 
  match dataa, b.data with 
  | None, _ -> {cmp; data = None}
  | _, None -> 
    {data = (N.copy dataa); cmp}
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
    then {data = (N.copy dataa); cmp}
    else 
      let tmp2 = A.makeUninitializedUnsafe sizea in 
      let k = Sort.diffU tmp 0 sizea tmp sizea sizeb tmp2 0 p in 
      {data = (N.fromSortedArrayAux tmp2 0 k); cmp}

let union a b = 
  let cmp = a.cmp in 
  let dataa, datab =  a.data, b.data  in 
  match dataa, datab with 
  | None, _ -> {data = (N.copy datab); cmp}
  | _, None -> {data = (N.copy dataa); cmp}
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
        { data = (N.fromSortedArrayAux tmp 0 totalSize); cmp}
    else   
      let tmp2 = A.makeUninitializedUnsafe totalSize in 
      let k = Sort.unionU tmp 0 sizea tmp sizea sizeb tmp2 0 p in 
      {data = (N.fromSortedArrayAux tmp2 0 k); cmp}
      
let has d x =
  N.has ~cmp:d.cmp d.data x

let copy d = {data = (N.copy d.data); cmp = d.cmp}


