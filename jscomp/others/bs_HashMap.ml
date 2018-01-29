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
(**  Adapted by Authors of BuckleScript 2017                           *)

module N = Bs_internalBuckets 
module C = Bs_internalBucketsType
module A = Bs_Array


type ('a, 'id) eq = ('a, 'id) Bs_Hash.eq
type ('a, 'id) hash = ('a, 'id) Bs_Hash.hash
type ('a, 'id) dict = ('a, 'id) Bs_Hash.t                        
type ('a,'b,'id) t =
  ( ('a, 'id) hash, ('a, 'id) eq, 'a, 'b) N.t



let rec copyBucketReHash ~hash ~h_buckets ~ndata_tail old_bucket = 
  match C.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = (Bs_Hash.getHashInternal hash) (N.key cell) [@bs] land (A.length h_buckets - 1) in 
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
  if (Bs_Hash.getEqInternal eq) (N.key cell) key [@bs]
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
  let i = (Bs_Hash.getHashInternal hash) key [@bs] land (buckets_len - 1) in 
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
  let eq, hash = C.eq h, C.hash h in 
  set0 h key value ~eq ~hash

let rec removeInBucket  h h_buckets  i key prec bucket ~eq =
  match C.toOpt bucket with
  | None -> ()
  | Some cell ->
    let cell_next = N.next cell in 
    if (Bs_Hash.getEqInternal eq) (N.key cell) key [@bs]
    then 
      begin        
        N.nextSet prec cell_next ; 
        C.sizeSet h (C.size h - 1);        
      end
    else removeInBucket ~eq h h_buckets i key cell cell_next


let remove h key = 
  let eq = C.eq h in 
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHashInternal (C.hash h)) key [@bs] land (A.length h_buckets - 1) in  
  let bucket = A.getUnsafe h_buckets i in 
  match C.toOpt bucket with 
  | None -> ()
  | Some cell -> 
    if (Bs_Hash.getEqInternal eq) (N.key cell ) key [@bs] then 
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
  let eq = Bs_Hash.getEqInternal (C.eq h) in
  let h_buckets = C.buckets h in 
  let nid = (Bs_Hash.getHashInternal (C.hash h)) key [@bs] land (A.length h_buckets - 1) in 
  match C.toOpt @@ A.getUnsafe h_buckets nid with
  | None -> None
  | Some cell1  ->
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


let rec memInBucket ~eq key cell = 
  eq (N.key cell) key [@bs] || 
  (match C.toOpt (N.next cell) with 
   | None -> false 
   | Some nextCell -> 
     memInBucket ~eq key nextCell)

let has h key =
  let eq = Bs_Hash.getEqInternal (C.eq h) in 
  let h_buckets = C.buckets h in 
  let nid = (Bs_Hash.getHashInternal (C.hash h)) key [@bs] land (A.length h_buckets - 1) in 
  let bucket = A.getUnsafe h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    memInBucket ~eq key bucket




let make (type elt) (type id) initialize_size ~(dict : (elt,id) dict) =
  let module M = (val dict) in 
  C.make ~hash:M.hash ~eq:M.eq  initialize_size

let clear = C.clear
let size = C.size
let forEach = N.forEach
let reduce = N.reduce
let logStats = N.logStats
let keepMapInPlace  = N.keepMapInPlace
let toArray = N.toArray
let copy  = N.copy
let keysToArray = N.keysToArray
let valuesToArray = N.valuesToArray 
let getBucketHistogram  = N.getBucketHistogram 
let isEmpty = C.isEmpty


 
  
let ofArray (type a) (type id) arr ~dict:(dict:(a,id) Bs_Hash.t) =     
  let module M = (val dict) in
  let eq, hash = M.eq, M.hash in 
  let len = A.length arr in 
  let v = C.make ~hash ~eq len in 
  for i = 0 to len - 1 do 
    let key,value = (A.getUnsafe arr i) in 
    set0 ~eq ~hash v key value
  done ;
  v
      
let mergeMany h arr = 
  let hash, eq = C.hash h , C.eq h in 
  let len = A.length arr in 
  for i = 0 to len - 1 do 
    let key,value = (A.getUnsafe arr i) in 
    set0 h  ~eq ~hash key value
  done
    

module Int = Bs_HashMapInt
module String = Bs_HashMapString  
