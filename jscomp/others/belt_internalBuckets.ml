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
(**  Adapted by Hongbo Zhang, Authors of ReScript 2017                           *)

(* For JS backends, we use `undefined` as default value, so that buckets
   could be allocated lazily
*)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)
module C = Belt_internalBucketsType
(* TODO:
   the current implementation relies on the fact that bucket 
   empty value is `undefined` in both places,
   in theory, it can be different 

*)
type ('a,'b) bucket = {
  mutable key : 'a;
  mutable value : 'b;
  mutable next : ('a,'b) bucket C.opt
}  
and ('hash, 'eq, 'a, 'b) t = ('hash, 'eq, ('a,'b) bucket) C.container  


module A = Belt_Array


let rec copy ( x : _ t) : _ t= 
  {
    hash = x.hash;
    eq = x.eq;
    size = x.size;
    buckets = copyBuckets x.buckets
  }
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
    let head = {key = c.key ; value = c.value;
                  next = (C.emptyOpt)} in 
    copyAuxCont c.next head;
    C.return head
and copyAuxCont c prec =       
  match C.toOpt c with 
  | None -> ()
  | Some nc -> 
    let ncopy = 
        {key = nc.key;  value = nc.value; next = C.emptyOpt} in 
    prec.next <- (C.return ncopy) ;
    copyAuxCont nc.next ncopy




let rec bucketLength accu buckets = 
  match C.toOpt buckets with 
  | None -> accu
  | Some cell -> bucketLength (accu + 1) cell.next



let rec do_bucket_iter ~f buckets = 
  match C.toOpt buckets with 
  | None ->
    ()
  | Some cell ->
    f cell.key  cell.value [@bs] |. ignore ; do_bucket_iter ~f cell.next

let forEachU h f =
  let d = h.C.buckets in
  for i = 0 to A.length d - 1 do
    do_bucket_iter ~f (A.getUnsafe d i)
  done

let forEach h f = forEachU h (fun [@bs] a b -> f a b)
    
let rec do_bucket_fold ~f b accu =
  match C.toOpt b with
  | None ->
    accu
  | Some cell ->
    do_bucket_fold ~f cell.next (f accu cell.key cell.value  [@bs]) 

let reduceU  h init f =
  let d =h.C.buckets in
  let accu = ref init in
  for i = 0 to A.length d - 1 do
    accu .contents<- do_bucket_fold ~f (A.getUnsafe d i) accu.contents
  done;
  accu.contents

let reduce h init f = reduceU h init (fun [@bs] a b c -> f a b c)


let getMaxBucketLength h =
  A.reduceU h.C.buckets 0
    (fun[@bs] m b -> 
       let len = bucketLength 0 b in
       Pervasives.max m len)

let getBucketHistogram h =
  let mbl = getMaxBucketLength h in 
  let histo = A.makeByU (mbl + 1) (fun[@bs] _ -> 0) in
  A.forEachU h.C.buckets
    (fun[@bs] b ->
       let l = bucketLength 0 b in
       A.setUnsafe histo l (A.getUnsafe histo l + 1)
    );
  histo

let logStats h =
  let histogram = getBucketHistogram h in 
  Js.log [%obj{ bindings = h.C.size;
                buckets = A.length h.C.buckets;
                histogram}]


(** iterate the Buckets, in place remove the elements *)                
let rec filterMapInplaceBucket f h i prec cell =
  let n = cell.next in
  begin match f cell.key cell.value [@bs] with
    | None ->
      h.C.size <- (h.C.size- 1); (* delete *)
      begin match C.toOpt n with 
        | Some nextCell -> 
          filterMapInplaceBucket f h i prec nextCell
        | None -> 
          match C.toOpt prec with 
          | None -> A.setUnsafe h.C.buckets i prec
          | Some cell -> cell.next <- n
      end
    | Some data -> (* replace *)
      let bucket = C.return cell in 
      begin match C.toOpt prec with
        | None -> A.setUnsafe h.C.buckets i  bucket 
        | Some _ -> cell.next <- bucket
      end;
      cell.value <- data;
      match C.toOpt n with 
      | None -> cell.next <- n 
      | Some nextCell -> 
        filterMapInplaceBucket f h i bucket nextCell
  end

let keepMapInPlaceU h f =
  let h_buckets = h.C.buckets in
  for i = 0 to A.length h_buckets - 1 do
    let v = A.getUnsafe h_buckets i in 
    match C.toOpt v with 
    | None -> ()
    | Some v -> filterMapInplaceBucket f h i C.emptyOpt v
  done

let keepMapInPlace h f = keepMapInPlaceU h (fun [@bs] a b -> f a b)
    
let rec fillArray i arr cell =  
  A.setUnsafe arr i (cell.key, cell.value);
  match C.toOpt cell.next with 
  | None -> i + 1
  | Some v -> fillArray (i + 1) arr v 

(* let toArray h = 
  let d =h.bucketsin 
  let current = ref 0 in 
  let arr = A.makeUninitializedUnsafe (C.sizeGet h) in 
  for i = 0 to A.length d - 1 do  
    let cell = A.getUnsafe d i in 
    match C.toOpt cell with 
    | None -> ()
    | Some cell -> 
      current .contents<- fillArray current.contents arr cell
  done;
  arr  *)

let rec fillArrayMap i arr cell f =  
  A.setUnsafe arr i (f cell [@bs]);
  match C.toOpt cell.next with 
  | None -> i + 1
  | Some v -> fillArrayMap (i + 1) arr v f

let linear h f = 
  let d = h.C.buckets in 
  let current = ref 0 in 
  let arr = A.makeUninitializedUnsafe h.C.size in 
  for i = 0 to A.length d - 1 do  
    let cell = A.getUnsafe d i in 
    match C.toOpt cell with 
    | None -> ()
    | Some cell -> 
      current .contents<- fillArrayMap current.contents arr cell f
  done;
  arr 

let keysToArray h = linear h (fun [@bs] x -> x.key)  
let valuesToArray h = linear h (fun [@bs] x -> x.value)  
let toArray h = linear h (fun [@bs]x -> x.key, x.value)



