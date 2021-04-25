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
(*  Adapted by Hongbo Zhang, Authors of ReScript 2017                            *)
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
let size h = h.C.size
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
    let nidx = hash cell.N.key [@bs] land (A.length h_buckets - 1) in 
    let v = C.return cell in 
    begin match C.toOpt (A.getUnsafe ndata_tail nidx) with
      | None -> 
        A.setUnsafe h_buckets nidx  v
      | Some tail ->
        tail.N.next <- v  (* cell put at the end *)            
    end;          
    A.setUnsafe ndata_tail nidx  v;
    copyBucketReHash ~hash ~h_buckets ~ndata_tail  cell.N.next


let resize ~hash h =
  let odata = h.C.buckets  in
  let osize = A.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    h.C.buckets <- h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      copyBucketReHash ~hash ~h_buckets ~ndata_tail  (A.getUnsafe odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (A.getUnsafe ndata_tail i) with
      | None -> ()
      | Some tail -> tail.N.next <- C.emptyOpt
    done
  end

let rec replaceInBucket ~eq  key info cell = 
  if eq cell.N.key key [@bs]
  then
    begin
      cell.N.value <- info;
      false
    end
  else
    match C.toOpt cell.N.next with 
    | None -> true 
    | Some cell -> 
      replaceInBucket ~eq key info cell

let set0 h key value ~eq ~hash = 
  let h_buckets = h.C.buckets  in 
  let buckets_len = A.length h_buckets in 
  let i = hash key [@bs] land (buckets_len - 1) in 
  let l = A.getUnsafe h_buckets i in  
  (match C.toOpt l with  
  | None -> 
    A.setUnsafe h_buckets i (C.return {N.key; value; next = C.emptyOpt});
    h.C.size <- (h.C.size + 1);
  | Some bucket -> 
      if replaceInBucket ~eq key value bucket then begin
        A.setUnsafe h_buckets i (C.return {N.key; value; next = l});
        h.C.size <- (h.C.size + 1);
      end 
    );
    if h.C.size  > buckets_len lsl 1 then resize ~hash h
      
(* if `key` already exists, replace it, otherwise add it 
   Here we add it to the head, it could be tail
*)      
let set  h key value =
  set0 h key value
   ~eq:(Belt_Id.getEqInternal h.C.eq)
   ~hash:(Belt_Id.getHashInternal h.C.hash)

let rec removeInBucket  h h_buckets  i key prec bucket ~eq =
  match C.toOpt bucket with
  | None -> ()
  | Some cell ->
    let cell_next = cell.N.next in 
    if eq cell.N.key key [@bs]
    then 
      begin        
        prec.N.next <- cell_next ; 
        h.C.size <- (h.C.size  - 1);        
      end
    else removeInBucket ~eq h h_buckets i key cell cell_next


let remove h key = 
  let h_buckets = h.C.buckets  in 
  let i = (Belt_Id.getHashInternal h.C.hash) key [@bs] land (A.length h_buckets - 1) in  
  let bucket = A.getUnsafe h_buckets i in 
  match C.toOpt bucket with 
  | None -> ()
  | Some cell -> 
    let eq = (Belt_Id.getEqInternal h.C.eq) in 
    if eq cell.N.key key [@bs] then 
    begin 
      A.setUnsafe h_buckets i cell.N.next;
      h.C.size <- (h.C.size  - 1)
    end 
    else  
      removeInBucket ~eq h h_buckets i key  cell cell.N.next


let rec getAux ~eq key buckets = 
  match C.toOpt buckets with 
  | None ->
    None
  | Some cell ->
    if eq key cell.N.key [@bs] then Some cell.N.value
    else getAux ~eq key  cell.N.next

let get h key =
  let h_buckets = h.C.buckets  in 
  let nid = (Belt_Id.getHashInternal h.C.hash) key [@bs] land (A.length h_buckets - 1) in 
  match C.toOpt @@ A.getUnsafe h_buckets nid with
  | None -> None
  | Some (cell1 : _ N.bucket) ->
    let eq = Belt_Id.getEqInternal h.C.eq in
    if eq key cell1.key [@bs] then 
      Some  cell1.value
    else
      match C.toOpt cell1.N.next with
      | None -> None
      | Some cell2 ->
        if eq key cell2.key [@bs] then 
          Some cell2.value else
          match C.toOpt cell2.next with
          | None -> None
          | Some cell3 ->
            if eq key cell3.key [@bs] then 
              Some cell3.value
            else 
              getAux ~eq key cell3.next


let rec memInBucket key cell ~eq = 
  eq cell.N.key key [@bs] || 
  (match C.toOpt cell.N.next with 
   | None -> false 
   | Some nextCell -> 
     memInBucket ~eq key nextCell)

let has h key =
  let h_buckets = h.C.buckets  in 
  let nid = (Belt_Id.getHashInternal h.C.hash) key [@bs] land (A.length h_buckets - 1) in 
  let bucket = A.getUnsafe h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    memInBucket ~eq:(Belt_Id.getEqInternal h.C.eq) key bucket




let make (type key identity) ~hintSize ~(id : (key,identity) id) =
  let module M = (val id) in 
  C.make ~hash:M.hash ~eq:M.eq  ~hintSize

  
let fromArray (type a  identity) arr ~id:(id:(a,identity) id) =     
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
  let hash, eq = Belt_Id.getHashInternal h.C.hash , Belt_Id.getEqInternal h.C.eq in 
  let len = A.length arr in 
  for i = 0 to len - 1 do 
    let key,value = (A.getUnsafe arr i) in 
    set0 h  ~eq ~hash key value
  done
    

module Int = Belt_HashMapInt
module String = Belt_HashMapString  
