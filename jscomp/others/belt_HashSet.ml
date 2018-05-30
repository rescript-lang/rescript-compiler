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

module Int = Belt_HashSetInt

module String = Belt_HashSetString


module N = Belt_internalSetBuckets
module C = Belt_internalBucketsType
module A = Belt_Array

type ('a, 'id) eq = ('a, 'id) Belt_Id.eq
type ('a, 'id) hash = ('a, 'id) Belt_Id.hash
type ('a, 'id) id = ('a, 'id) Belt_Id.hashable

type ('a,'id) t =  ( ('a, 'id) hash, ('a, 'id) eq, 'a) N.t



let rec copyBucket ~hash ~h_buckets ~ndata_tail  old_bucket = 
  match C.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = (Belt_Id.getHashInternal hash) (N.key cell) [@bs] land (A.length h_buckets - 1) in 
    let v = C.return cell in 
    begin match C.toOpt (A.getUnsafe ndata_tail nidx) with
      | None -> 
        A.setUnsafe h_buckets nidx  v
      | Some tail ->
        N.nextSet tail v  (* cell put at the end *)            
    end;          
    A.setUnsafe ndata_tail nidx  v;
    copyBucket ~hash ~h_buckets ~ndata_tail  (N.next cell)


let tryDoubleResize ~hash h =
  let odata = C.buckets h in
  let osize = A.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    C.bucketsSet h  h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      copyBucket ~hash ~h_buckets ~ndata_tail  (A.getUnsafe odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (A.getUnsafe ndata_tail i) with
      | None -> ()
      | Some tail -> N.nextSet tail C.emptyOpt
    done
  end



let rec removeBucket ~eq h h_buckets  i key prec cell =
  let cell_next = N.next cell in 
  if (Belt_Id.getEqInternal eq) (N.key cell) key [@bs]
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

let remove h key = 
  let eq = C.eq h in 
  let h_buckets = C.buckets h in 
  let i = (Belt_Id.getHashInternal (C.hash h)) key [@bs] land (A.length h_buckets - 1) in  
  let l = A.getUnsafe h_buckets i in 
  match C.toOpt l with 
  | None -> ()
  | Some cell -> 
    let next_cell = N.next cell in 
    if (Belt_Id.getEqInternal eq) (N.key cell) key [@bs] then 
      begin 
        C.sizeSet h (C.size h - 1) ;
        A.setUnsafe h_buckets i next_cell
      end
    else       
      match C.toOpt next_cell with 
      | None -> ()
      | Some next_cell -> 
        removeBucket ~eq h h_buckets i key cell next_cell
  


let rec addBucket h key  cell  ~eq = 
  if not ((Belt_Id.getEqInternal eq) (N.key cell) key [@bs]) then
    let  n = N.next cell in 
    match C.toOpt n with 
    | None ->  
      C.sizeSet h (C.size h + 1);
      N.nextSet cell (C.return @@ N.bucket ~key ~next:C.emptyOpt)
    | Some n -> addBucket ~eq h key  n

let add0 h key  ~hash  ~eq =
  let h_buckets = C.buckets h in 
  let buckets_len = A.length h_buckets in 
  let i = (Belt_Id.getHashInternal hash) key [@bs] land (buckets_len - 1) in 
  let l = A.getUnsafe h_buckets i in  
  (match C.toOpt l with                                    
   | None -> 
     C.sizeSet h (C.size  h + 1);
     A.setUnsafe h_buckets i 
       (C.return @@ N.bucket ~key ~next:C.emptyOpt)
   | Some cell -> 
     addBucket ~eq h key cell);
  if C.size h > buckets_len lsl 1 then tryDoubleResize ~hash h

let add h key  = 
  add0 ~hash:(C.hash h) ~eq:(C.eq h) h  key 

let rec memInBucket ~eq key cell = 
  (Belt_Id.getEqInternal eq) 
    (N.key cell) key [@bs] || 
  (match C.toOpt (N.next cell) with 
   | None -> false 
   | Some nextCell -> 
     memInBucket ~eq key nextCell)


  
let has h key =           
  let eq, h_buckets = C.eq h, C.buckets h  in 
  let nid = (Belt_Id.getHashInternal (C.hash h)) key [@bs] land (A.length h_buckets - 1) in 
  let bucket = A.getUnsafe h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    memInBucket ~eq key bucket




let make (type value) (type identity)  ~hintSize ~(id: (value, identity) id)=
  let module M = (val id) in 
  C.make ~hintSize ~hash:M.hash ~eq:M.eq

let clear = C.clear
let size  = C.size
let forEachU  = N.forEachU
let forEach = N.forEach                  
let reduceU = N.reduceU
let reduce = N.reduce                
let logStats = N.logStats
let toArray  = N.toArray
let copy = N.copy
let getBucketHistogram  = N.getBucketHistogram 
let isEmpty  = C.isEmpty

let fromArray (type a) (type identity) arr ~(id:(a,identity) id)  =     
  let module M = (val id) in 
  let eq, hash = M.eq , M.hash in 
    let len = A.length arr in 
  let v = C.make ~hintSize:len ~hash ~eq  in 
  for i = 0 to len - 1 do 
    add0 ~eq ~hash v (A.getUnsafe arr i)
  done ;
  v

let mergeMany h arr = 
  let eq, hash = C.eq h, C.hash h in 
  let len = A.length arr in 
  for i = 0 to len - 1 do 
    add0 h  ~eq ~hash (A.getUnsafe arr i)
  done 
  

