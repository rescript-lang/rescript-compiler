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
module B = Bs_Bag 
module A = Bs_Array

type ('a, 'b,'id) t0 = ('a,'b) N.t0

type ('a,'b,'id) t = 
  (('a, 'id) Bs_Hash.t,
   ('a,'b,'id) t0) B.bag 



let rec copyBucketReHash ~hash ~h_buckets ~ndata_tail h old_bucket = 
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
    copyBucketReHash ~hash ~h_buckets ~ndata_tail h (N.next cell)


let resize ~hash h =
  let odata = C.buckets h in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    C.bucketsSet h  h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      copyBucketReHash ~hash ~h_buckets ~ndata_tail h (A.getUnsafe odata i)
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

(* if [key] already exists, replace it, otherwise add it 
   Here we add it to the head, it could be tail
*)      
let setDone0 ~hash ~eq  h key value =
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHashInternal hash) key [@bs] land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  match C.toOpt l with  
  | None -> 
    A.setUnsafe h_buckets i (C.return (N.bucket ~key ~value ~next:l));
    C.sizeSet h (C.size  h + 1);
    if C.size h > Array.length (C.buckets h) lsl 1 then resize ~hash h
    (* do we really need resize here ? *)
  | Some bucket -> 
    begin 
      if replaceInBucket ~eq key value bucket then begin
        A.setUnsafe h_buckets i (C.return (N.bucket ~key ~value ~next:l));
        C.sizeSet h (C.size  h + 1);
        if C.size h > Array.length (C.buckets h) lsl 1 then resize ~hash h
        (* TODO: duplicate bucklets ? *)
      end 
    end

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

let remove0 ~hash ~eq h key =  
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHashInternal hash) key [@bs] land (Array.length h_buckets - 1) in  
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


let rec findAux ~eq key buckets = 
  match C.toOpt buckets with 
  | None ->
    None
  | Some cell ->
    if (Bs_Hash.getEqInternal eq) key (N.key cell) [@bs] then Some (N.value cell)
    else findAux ~eq key  (N.next cell)

let get0 ~hash ~eq h key =
  let h_buckets = C.buckets h in 
  let nid = (Bs_Hash.getHashInternal hash) key [@bs] land (Array.length h_buckets - 1) in 
  match C.toOpt @@ A.getUnsafe h_buckets nid with
  | None -> None
  | Some cell1  ->
    if (Bs_Hash.getEqInternal eq) key (N.key cell1) [@bs] then 
      Some  (N.value cell1)
    else
      match C.toOpt (N.next  cell1) with
      | None -> None
      | Some cell2 ->
        if (Bs_Hash.getEqInternal eq) key 
            (N.key cell2) [@bs] then 
          Some (N.value cell2) else
          match C.toOpt (N.next cell2) with
          | None -> None
          | Some cell3 ->
            if (Bs_Hash.getEqInternal eq) key 
                (N.key cell3) [@bs] then 
              Some (N.value cell3)
            else 
              findAux ~eq key (N.next cell3)


let rec memInBucket ~eq key cell = 
  (Bs_Hash.getEqInternal eq) (N.key cell) key [@bs] || 
  (match C.toOpt (N.next cell) with 
   | None -> false 
   | Some nextCell -> 
     memInBucket ~eq key nextCell)

let has0 ~hash ~eq h key =
  let h_buckets = C.buckets h in 
  let nid = (Bs_Hash.getHashInternal hash) key [@bs] land (Array.length h_buckets - 1) in 
  let bucket = A.getUnsafe h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    memInBucket ~eq key bucket




let make initialize_size ~dict = 
  B.bag ~data:(C.create0 initialize_size) ~dict 
let clear h = C.clear0 (B.data h)
let size h = C.size (B.data h)                 
let forEach h f = N.forEach0 (B.data h) f
let reduce h init f = N.reduce0 (B.data h) init f
let logStats h = N.logStats0 (B.data h)

let set (type a)  (type id) (h : (a,_,id) t) (key:a) info = 
  let module M = (val B.dict h) in 
  setDone0 ~hash:M.hash ~eq:M.eq (B.data h) key info 

let remove (type a) (type id) (h : (a,_,id) t) (key : a) = 
  let module M = (val B.dict h) in   
  remove0 ~hash:M.hash ~eq:M.eq (B.data h) key 

let get (type a) (type id) (h : (a, _, id) t) (key : a) =           
  let module M = (val B.dict h) in   
  get0 ~hash:M.hash ~eq:M.eq (B.data h) key 

let has (type a) (type id) (h : (a,_,id) t) (key : a) =           
  let module M = (val B.dict h) in   
  has0 ~hash:M.hash ~eq:M.eq (B.data h) key   

let keepMapInPlace h f = N.filterMapInplace0 (B.data h) f

  
let toArray h = N.toArray0 (B.data h)

let ofArray0 arr ~hash ~eq = 
  let len = A.length arr in 
  let v = C.create0 len in 
  for i = 0 to len - 1 do 
    let key,value = (A.getUnsafe arr i) in 
    setDone0 ~eq ~hash v key value
  done ;
  v

(* TOOD: optimize heuristics for resizing *)  
let mergeArrayDone0  h arr ~hash ~eq =   
  let len = A.length arr in 
  for i = 0 to len - 1 do 
    let key,value = (A.getUnsafe arr i) in 
    setDone0 h  ~eq ~hash key value
  done
    
let ofArray (type a) (type id) arr ~dict:(dict:(a,id) Bs_Hash.t) =     
  let module M = (val dict) in 
  B.bag ~dict  ~data:M.(ofArray0 ~eq ~hash arr)

let mergeMany (type a) (type id) (h : (a,_,id) t) arr = 
  let module M = (val B.dict h) in
  mergeArrayDone0 ~hash:M.hash ~eq:M.eq (B.data h) arr


let copy h = B.bag ~dict:(B.dict h) ~data:(N.copy (B.data h))

let keysToArray h = N.keys0 (B.data h)

let valuesToArray h = N.values0 (B.data h)
let getBucketHistogram h = N.getBucketHistogram (B.data h)

let isEmpty h = C.size (B.data h) = 0
