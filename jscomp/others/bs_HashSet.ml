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


module N = Bs_internalSetBuckets
module C = Bs_internalBucketsType
module B = Bs_Bag 
module A = Bs_Array

type ('a,'id) t0 = 'a N.t0 


type 'a bucket = 'a N.bucket

type ('a,'id) t = 
  (('a, 'id) Bs_Hash.t,
   ('a,'id) t0) B.bag 



let rec copyBucket ~hash ~h_buckets ~ndata_tail h old_bucket = 
  match C.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = (Bs_Hash.getHashInternal hash) (N.key cell) [@bs] land (Array.length h_buckets - 1) in 
    let v = C.return cell in 
    begin match C.toOpt (A.getUnsafe ndata_tail nidx) with
      | None -> 
        A.setUnsafe h_buckets nidx  v
      | Some tail ->
        N.nextSet tail v  (* cell put at the end *)            
    end;          
    A.setUnsafe ndata_tail nidx  v;
    copyBucket ~hash ~h_buckets ~ndata_tail h (N.next cell)


let resize ~hash h =
  let odata = C.buckets h in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    C.bucketsSet h  h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      copyBucket ~hash ~h_buckets ~ndata_tail h (A.getUnsafe odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (A.getUnsafe ndata_tail i) with
      | None -> ()
      | Some tail -> N.nextSet tail C.emptyOpt
    done
  end



let rec removeBucket ~eq h h_buckets  i key prec cell =
  let cell_next = N.next cell in 
  if (Bs_Hash.getEqInternal eq) (N.key cell) key [@bs]
  then 
    begin
      N.nextSet prec cell_next;
      C.sizeSet h (C.size h - 1);        
    end
  else 
    match C.toOpt cell_next with 
    | None -> 
      ()
    | Some cell_next ->
      removeBucket ~eq h h_buckets i key cell cell_next

let remove0 ~hash ~eq h key =  
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHashInternal hash) key [@bs] land (Array.length h_buckets - 1) in  
  let l = (A.getUnsafe h_buckets i) in 
  match C.toOpt l with 
  | None -> ()
  | Some cell -> 
    let next_cell = (N.next cell) in 
    if (Bs_Hash.getEqInternal eq) (N.key cell) key [@bs] then 
      begin 
        C.sizeSet h (C.size h - 1) ;
        A.setUnsafe h_buckets i next_cell
      end
    else       
      match C.toOpt next_cell with 
      | None -> ()
      | Some next_cell -> 
        removeBucket ~eq h h_buckets i key cell next_cell




let rec addBucket ~eq h key  cell = 
  if (Bs_Hash.getEqInternal eq) (N.key cell) key [@bs]
  then
    N.keySet cell key
  else
    let  n = N.next cell in 
    match C.toOpt n with 
    | None ->  
      C.sizeSet h (C.size h + 1);
      N.nextSet cell (C.return @@ N.bucket ~key ~next:n)
    | Some n -> addBucket ~eq h key  n

let addDone0     
    (h : (_,'id) t0) key
    ~hash:(hash:(_,'id)Bs_Hash.hash) 
    ~eq:(eq:(_,'id) Bs_Hash.eq)
  =
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHashInternal hash) key [@bs] land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  (match C.toOpt l with                                    
   | None -> 
     C.sizeSet h (C.size  h + 1);
     A.setUnsafe h_buckets i 
       (C.return @@ N.bucket ~key ~next:C.emptyOpt)
   | Some cell -> 
     addBucket ~eq h key cell);
  if C.size h > Array.length (C.buckets h) lsl 1 then resize ~hash h


let add0 h key ~hash ~eq  =  addDone0 h key ~hash ~eq; h
let rec memInBucket ~eq key cell = 
  (Bs_Hash.getEqInternal eq) 
    (N.key cell) key [@bs] || 
  (match C.toOpt (N.next cell) with 
   | None -> false 
   | Some nextCell -> 
     memInBucket ~eq key nextCell)

let has0 h key ~hash ~eq =
  let h_buckets = C.buckets h in 
  let nid = (Bs_Hash.getHashInternal hash) key [@bs] land (Array.length h_buckets - 1) in 
  let bucket = (A.getUnsafe h_buckets nid) in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    memInBucket ~eq key bucket


let toArray h = N.toArray0 (B.data h)
(*  Wrapper  *)
let make dict initialize_size = 
  B.bag ~data:(C.make initialize_size)
    ~dict 
let clear h = C.clear (B.data h)

let size h = C.size (B.data h)                 
let forEach h f  = N.forEach0 (B.data h) f 
let reduce h init f = N.reduce0 (B.data h) init f
let logStats h = N.logStats0 (B.data h)

let add (type a) (type id) (h : (a,id) t) (key:a)  = 
  let module M = (val  B.dict h) in 
  addDone0 ~hash:M.hash ~eq:M.eq (B.data h) key 

let remove (type a)  (type id) (h : (a,id) t) (key : a) = 
  let module M = (val B.dict h) in   
  remove0 ~hash:M.hash ~eq:M.eq (B.data h) key 

let has (type a) (type id) (h : (a,id) t) (key : a) =           
  let module M = (val B.dict h) in   
  has0 ~hash:M.hash ~eq:M.eq (B.data h) key   

let ofArray0  ~hash ~eq arr  = 
  let len = Bs.Array.length arr in 
  let v = C.make len in 
  for i = 0 to len - 1 do 
    addDone0 ~eq ~hash v (A.getUnsafe arr i)
  done ;
  v

(* TOOD: optimize heuristics for resizing *)  
let addArray0 ~hash ~eq  h arr =   
  let len = Bs.Array.length arr in 
  for i = 0 to len - 1 do 
    addDone0 h  ~eq ~hash (A.getUnsafe arr i)
  done 

let ofArray (type a) (type id)
    arr
    ~dict:(dict:(a,id) Bs_Hash.t)  =     
  let module M = (val dict) in 
  B.bag ~dict ~data:M.(ofArray0 ~eq~hash arr)

let mergeMany (type a) (type id) (h : (a,id) t) arr = 
  let module M = (val B.dict h) in
  addArray0 ~hash:M.hash ~eq:M.eq (B.data h) arr


  
let getData = B.data
let getDict = B.dict
let packDictData = B.bag 
let getBucketHistogram h = N.getBucketHistogram (B.data h)

let isEmpty h = C.size (B.data h) = 0
