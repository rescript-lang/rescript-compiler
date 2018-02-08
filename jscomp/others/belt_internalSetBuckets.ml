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

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)
module C = Belt_internalBucketsType
(* TODO:
   the current implementation relies on the fact that bucket 
   empty value is [undefined] in both places,
   in theory, it can be different 

*)
type 'a bucket = {
  mutable key : 'a;
  mutable next : 'a bucket C.opt
}  
and ('hash, 'eq, 'a) t = ('hash, 'eq, 'a bucket) C.container  
[@@bs.deriving abstract]

module A = Belt_Array

let rec copy ( x : _ t) : _ t= 
  C.container
    ~hash:(C.hash x)
    ~eq:(C.eq x )
    ~size:(C.size x)
    ~buckets:(copyBuckets (C.buckets x))
and copyBuckets ( buckets : _ bucket C.opt array) =  
  let len = A.length buckets in 
  let newBuckets = A.makeUninitializedUnsafe len in 
  for i = 0 to len - 1 do 
    A.setUnsafe newBuckets i 
    (copyBucket (A.getUnsafe buckets i))
  done ;
  newBuckets
and copyBucket c =   
  match C.toOpt c with 
  | None -> c 
  | Some c -> 
    let head = (bucket ~key:(key c) 
                  ~next:(C.emptyOpt)) in 
    copyAuxCont (next c) head;
    C.return head
and copyAuxCont c prec =       
  match C.toOpt c with 
  | None -> ()
  | Some nc -> 
    let ncopy = bucket ~key:(key nc) ~next:C.emptyOpt in 
    nextSet prec (C.return ncopy) ;
    copyAuxCont (next nc) ncopy


let rec bucketLength accu buckets = 
  match C.toOpt buckets with 
  | None -> accu
  | Some cell -> bucketLength (accu + 1) (next cell)



let rec doBucketIter ~f buckets = 
  match C.toOpt buckets with 
  | None ->
    ()
  | Some cell ->
    f (key cell)  [@bs]; doBucketIter ~f (next cell)

let forEachU h f =
  let d = C.buckets h in
  for i = 0 to A.length d - 1 do
    doBucketIter f (A.getUnsafe d i)
  done

let forEach h f = forEachU h (fun[@bs] a -> f a )
    
let rec fillArray i arr cell =  
  A.setUnsafe arr i (key cell);
  match C.toOpt (next cell) with 
  | None -> i + 1
  | Some v -> fillArray (i + 1) arr v 

let toArray h = 
  let d = C.buckets h in 
  let current = ref 0 in 
  let arr = A.makeUninitializedUnsafe (C.size h) in 
  for i = 0 to A.length d - 1 do  
    let cell = A.getUnsafe d i in 
    match C.toOpt cell with 
    | None -> ()
    | Some cell -> 
      current := fillArray !current arr cell
  done;
  arr 



let rec doBucketFold ~f b accu =
  match C.toOpt b with
  | None ->
    accu
  | Some cell ->
    doBucketFold ~f (next cell) (f  accu (key cell) [@bs]) 

let reduceU h init f =
  let d = C.buckets h in
  let accu = ref init in
  for i = 0 to A.length d - 1 do
    accu := doBucketFold ~f (A.getUnsafe d i) !accu
  done;
  !accu

let reduce h init f = reduceU h init (fun [@bs] a b -> f a b)
    
let getMaxBucketLength h =
  A.reduceU (C.buckets h) 0
    (fun[@bs] m b -> 
       let len = bucketLength 0 b in
       Pervasives.max m len)

let getBucketHistogram h =
  let mbl = getMaxBucketLength h in 
  let histo = A.makeByU (mbl + 1) (fun[@bs] _ -> 0) in
  A.forEachU (C.buckets h)
    (fun[@bs] b ->
       let l = bucketLength 0 b in
       A.setUnsafe histo l (A.getUnsafe histo l + 1)
    );
  histo


let logStats h =
  let histogram =  getBucketHistogram h in 
  Js.log [%obj{ bindings = C.size h;
                buckets = A.length (C.buckets h);
                histogram  }]


