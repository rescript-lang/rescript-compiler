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
(*  Adapted by Authors of BuckleScript 2017                            *)
(***********************************************************************)


module N = Belt_internalBuckets 
module C = Belt_internalBucketsType
module A = Belt_Array


type ('a, 'id) eq = ('a, 'id) Belt_Id.eq
type ('a, 'id) hash = ('a, 'id) Belt_Id.hash
type ('a, 'id) id = ('a, 'id) Belt_Id.hashable
type ('a,'b,'id) t =
  ( ('a, 'id) hash, ('a, 'id) eq, 'a, 'b) N.t


let clear = C.clear
let size = C.size
let forEach = N.forEach
let forEachU = N.forEachU 
let reduce = N.reduce 
let reduceU = N.reduceU
let logStats = N.logStats
let keepMapInPlaceU  = N.keepMapInPlaceU 
let keepMapInPlace  = N.keepMapInPlace
let toArray = N.toArray
let copy  = N.copy
let keysToArray = N.keysToArray
let valuesToArray = N.valuesToArray 
let getBucketHistogram  = N.getBucketHistogram 
let isEmpty = C.isEmpty


let rec copyBucketReHash ~hash ~h_buckets ~ndata_tail old_bucket = 
  match C.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = hash (N.key cell) [@bs] land (A.length h_buckets - 1) in 
    let v = C.return cell in 
    begin match C.toOpt (A.getUnsafe ndata_tail nidx) with
      | None -> 
        A.setUnsafe h_buckets nidx  v
      | Some tail ->
        N.nextSet tail v  (* cell put at the end *)            
    end;          
    A.setUnsafe ndata_tail nidx  v;
    copyBucketReHash ~hash ~h_buckets ~ndata_tail  (N.next cell)


let resize ~hash h =
  let odata = C.buckets h in
  let osize = A.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    C.bucketsSet h  h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      copyBucketReHash ~hash ~h_buckets ~ndata_tail  (A.getUnsafe odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (A.getUnsafe ndata_tail i) with
      | None -> ()
      | Some tail -> N.nextSet tail C.emptyOpt
    done
  end

let rec replaceInBucket ~eq  key info cell = 
  if eq (N.key cell) key [@bs]
  then
    begin
      N.valueSet cell info;
      false
    end
  else
    match C.toOpt (N.next cell) with 
    | None -> true 
    | Some cell -> 
      replaceInBucket ~eq key info cell

let set0 h key value ~eq ~hash = 
  let h_buckets = C.buckets h in 
  let buckets_len = A.length h_buckets in 
  let i = hash key [@bs] land (buckets_len - 1) in 
  let l = A.getUnsafe h_buckets i in  
  (match C.toOpt l with  
  | None -> 
    A.setUnsafe h_buckets i (C.return (N.bucket ~key ~value ~next:C.emptyOpt));
    C.sizeSet h (C.size  h + 1);
  | Some bucket -> 
      if replaceInBucket ~eq key value bucket then begin
        A.setUnsafe h_buckets i (C.return (N.bucket ~key ~value ~next:l));
        C.sizeSet h (C.size  h + 1);
      end 
    );
    if C.size h > buckets_len lsl 1 then resize ~hash h
      
(* if [key] already exists, replace it, otherwise add it 
   Here we add it to the head, it could be tail
*)      
let set  h key value =
  set0 h key value
   ~eq:(Belt_Id.getEqInternal (C.eq h))
   ~hash:(Belt_Id.getHashInternal (C.hash h))

let rec removeInBucket  h h_buckets  i key prec bucket ~eq =
  match C.toOpt bucket with
  | None -> ()
  | Some cell ->
    let cell_next = N.next cell in 
    if eq (N.key cell) key [@bs]
    then 
      begin        
        N.nextSet prec cell_next ; 
        C.sizeSet h (C.size h - 1);        
      end
    else removeInBucket ~eq h h_buckets i key cell cell_next


let remove h key = 
  let h_buckets = C.buckets h in 
  let i = (Belt_Id.getHashInternal (C.hash h)) key [@bs] land (A.length h_buckets - 1) in  
  let bucket = A.getUnsafe h_buckets i in 
  match C.toOpt bucket with 
  | None -> ()
  | Some cell -> 
    let eq = (Belt_Id.getEqInternal (C.eq h)) in 
    if eq (N.key cell ) key [@bs] then 
    begin 
      A.setUnsafe h_buckets i (N.next cell);
      C.sizeSet h (C.size h - 1)
    end 
    else  
      removeInBucket ~eq h h_buckets i key  cell (N.next cell)


let rec getAux ~eq key buckets = 
  match C.toOpt buckets with 
  | None ->
    None
  | Some cell ->
    if eq key (N.key cell) [@bs] then Some (N.value cell)
    else getAux ~eq key  (N.next cell)

let get h key =
  let h_buckets = C.buckets h in 
  let nid = (Belt_Id.getHashInternal (C.hash h)) key [@bs] land (A.length h_buckets - 1) in 
  match C.toOpt @@ A.getUnsafe h_buckets nid with
  | None -> None
  | Some cell1  ->
    let eq = Belt_Id.getEqInternal (C.eq h) in
    if eq key (N.key cell1) [@bs] then 
      Some  (N.value cell1)
    else
      match C.toOpt (N.next  cell1) with
      | None -> None
      | Some cell2 ->
        if eq key (N.key cell2) [@bs] then 
          Some (N.value cell2) else
          match C.toOpt (N.next cell2) with
          | None -> None
          | Some cell3 ->
            if eq key (N.key cell3) [@bs] then 
              Some (N.value cell3)
            else 
              getAux ~eq key (N.next cell3)


let rec memInBucket key cell ~eq = 
  eq (N.key cell) key [@bs] || 
  (match C.toOpt (N.next cell) with 
   | None -> false 
   | Some nextCell -> 
     memInBucket ~eq key nextCell)

let has h key =
  let h_buckets = C.buckets h in 
  let nid = (Belt_Id.getHashInternal (C.hash h)) key [@bs] land (A.length h_buckets - 1) in 
  let bucket = A.getUnsafe h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    memInBucket ~eq:(Belt_Id.getEqInternal (C.eq h)) key bucket




let make (type key) (type identity) ~hintSize ~(id : (key,identity) id) =
  let module M = (val id) in 
  C.make ~hash:M.hash ~eq:M.eq  ~hintSize

  
let fromArray (type a) (type identity) arr ~id:(id:(a,identity) id) =     
  let module M = (val id) in
  let hash, eq = M.hash, M.eq in  
  let len = A.length arr in 
  let v = C.make ~hash ~eq ~hintSize:len in 
  let eq, hash = Belt_Id.getEqInternal eq, Belt_Id.getHashInternal hash in   
  for i = 0 to len - 1 do 
    let key,value = (A.getUnsafe arr i) in 
    set0 ~eq ~hash v key value
  done ;
  v

let mergeMany h arr = 
  let hash, eq = Belt_Id.getHashInternal ( C.hash h) , Belt_Id.getEqInternal (C.eq h) in 
  let len = A.length arr in 
  for i = 0 to len - 1 do 
    let key,value = (A.getUnsafe arr i) in 
    set0 h  ~eq ~hash key value
  done
    

module Int = Belt_HashMapInt
module String = Belt_HashMapString  
