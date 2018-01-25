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

#ifdef TYPE_STRING
type key = string
type seed = int
external caml_hash_mix_string : seed -> string -> seed  = "caml_hash_mix_string"
external final_mix : seed -> seed = "caml_hash_final_mix"
let hash (s : key) =   
  final_mix  (caml_hash_mix_string 0 s )
#elif defined TYPE_INT
type key = int
type seed = int
external caml_hash_mix_int : seed -> int -> seed  = "caml_hash_mix_int"
external final_mix : seed -> seed = "caml_hash_final_mix"
let hash (s : key) = 
  final_mix (caml_hash_mix_int 0 s)
#else 
  [%error "unknown type"]
#endif

module N = Bs_internalBuckets
module C = Bs_internalBucketsType
module A = Bs_Array


type 'b t = (key,'b) N.t0


let rec copyBucketReHash  ~h_buckets ~ndata_tail h old_bucket = 
  match C.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = hash (N.key cell) land (A.length h_buckets - 1) in 
    let v = C.return cell in 
    begin match C.toOpt (A.getUnsafe ndata_tail nidx) with
      | None -> 
        A.setUnsafe h_buckets nidx  v
      | Some tail ->
        N.nextSet tail v  (* cell put at the end *)            
    end;          
    A.setUnsafe ndata_tail nidx  v;
    copyBucketReHash  ~h_buckets ~ndata_tail h (N.next cell)


let resize  h =
  let odata = C.buckets h in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    C.bucketsSet h  h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      copyBucketReHash  ~h_buckets ~ndata_tail h (A.getUnsafe odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (A.getUnsafe ndata_tail i) with
      | None -> ()
      | Some tail -> N.nextSet tail C.emptyOpt
    done
  end


let rec replaceInBucket  (key : key) info cell = 
    if  N.key cell = key 
    then
      begin
        N.valueSet cell info;
        false
      end
    else
    match C.toOpt (N.next cell) with 
    | None -> true 
    | Some cell -> 
      replaceInBucket key info cell

let setDone  h (key : key) value =
  let h_buckets = C.buckets h in 
  let i = hash key land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  match C.toOpt l with 
  | None -> 
    A.setUnsafe h_buckets i (C.return (N.bucket ~key ~value ~next:l));
    C.sizeSet h (C.size h + 1);
    if C.size h > Array.length (C.buckets h) lsl 1 then resize h 
  | Some bucket -> 
    begin 
    if replaceInBucket key value bucket then begin
      A.setUnsafe h_buckets i (C.return (N.bucket ~key ~value ~next:l));
      C.sizeSet h (C.size h + 1);
      if C.size h > Array.length (C.buckets h) lsl 1 then resize  h
    end   
    end

let set h key value = setDone h key value ; h

let rec removeInBucket h h_buckets  i (key : key) prec buckets =
  match C.toOpt buckets with
  | None -> ()
  | Some cell  ->
    let cell_next = N.next cell in 
    if  N.key cell = key 
    then 
      begin
        N.nextSet prec cell_next;
        C.sizeSet h (C.size h - 1);        
      end
    else removeInBucket  h h_buckets i key cell cell_next

let removeDone  h key =  
  let h_buckets = C.buckets h in 
  let i = hash key land (Array.length h_buckets - 1) in  
  let bucket = (A.getUnsafe h_buckets i) in 
  match C.toOpt bucket with 
  | None -> ()
  | Some cell -> 
    if N.key cell = key then 
    begin 
      A.setUnsafe h_buckets i (N.next cell);
      C.sizeSet h (C.size h - 1)
    end 
    else 
      removeInBucket  h h_buckets i key cell (N.next cell)

let remove h key  = removeDone h key; h

let rec findAux  (key : key) buckets = 
  match C.toOpt buckets with 
  | None ->
    None
  | Some cell ->
    if key = N.key cell  then Some (N.value cell) 
    else findAux key  (N.next cell)

let get  h (key : key) =
  let h_buckets = C.buckets h in 
  let nid = hash key  land (A.length h_buckets - 1) in 
  match C.toOpt @@ A.getUnsafe h_buckets nid with
  | None -> None
  | Some cell1 ->
    if key = (N.key cell1)  then Some (N.value cell1) else
      match C.toOpt (N.next cell1) with
      | None -> None
      | Some cell2 ->
        if  key = (N.key cell2)  then Some (N.value cell2) else
          match C.toOpt (N.next cell2) with
          | None -> None
          | Some cell3 ->
            if  key = (N.key cell3)  then Some (N.value cell3)
            else findAux  key (N.next cell3)




let rec memInBucket (key : key) cell = 
    (N.key cell)  = key  ||
    (match C.toOpt (N.next cell) with 
    | None -> false
    | Some nextCell -> memInBucket  key nextCell
     )
    
let has h key =
  let h_buckets = C.buckets h in 
  let nid = hash key land (A.length h_buckets - 1) in 
  let bucket = A.getUnsafe h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false
  | Some bucket -> 
    memInBucket  key bucket


let create = C.create0
let clear = C.clear0
let size = C.size
let forEach = N.forEach0
let reduce = N.reduce0
let logStats = N.logStats0
let filterMapDone = N.filterMapInplace0
let filterMap h f = filterMapDone h f; h
let toArray = N.toArray0 

let ofArray arr  = 
  let len = A.length arr in 
  let v = create len in 
  for i = 0 to len - 1 do 
    let k,value = (A.getUnsafe arr i) in
    setDone v k value
  done ;
  v

(* TOOD: optimize heuristics for resizing *)  
let mergeArrayDone h arr =   
  let len = A.length arr in 
  for i = 0 to len - 1 do 
    let k,v = (A.getUnsafe arr i) in
    setDone h k v 
  done

let mergeArray h arr = mergeArrayDone h arr; h

let copy = N.copy

let keysToArray = N.keys0
let valuesToArray = N.values0
let getBucketHistogram = N.getBucketHistogram 
