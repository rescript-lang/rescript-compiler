# 2 "hashmap.cppo.ml"
type key = string
type seed = int
external caml_hash_mix_string : seed -> string -> seed  = "caml_hash_mix_string"
external final_mix : seed -> seed = "caml_hash_final_mix"
let hash (s : key) =   
  final_mix  (caml_hash_mix_string 0 s )

# 19
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
type ('a, 'b,'id) t0 = ('a,'b) N.t0 

type 'b t = (key,'b,unit) t0


let rec insert_bucket  ~h_buckets ~ndata_tail h old_bucket = 
  match C.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = hash (N.key cell) land (Array.length h_buckets - 1) in 
    let v = C.return cell in 
    begin match C.toOpt (Bs_Array.unsafe_get ndata_tail nidx) with
      | None -> 
        Bs_Array.unsafe_set h_buckets nidx  v
      | Some tail ->
        N.nextSet tail v  (* cell put at the end *)            
    end;          
    Bs_Array.unsafe_set ndata_tail nidx  v;
    insert_bucket  ~h_buckets ~ndata_tail h (N.next cell)


let resize  h =
  let odata = C.buckets h in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    C.bucketsSet h  h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      insert_bucket  ~h_buckets ~ndata_tail h (Bs_Array.unsafe_get odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (Bs_Array.unsafe_get ndata_tail i) with
      | None -> ()
      | Some tail -> N.nextSet tail C.emptyOpt
    done
  end


let rec replace_bucket  (key : key) info cell = 
    if  (N.key cell) = key 
    then
      begin
        N.keySet cell key;
        N.valueSet cell info;
        false
      end
    else
    match C.toOpt (N.next cell) with 
    | None -> true 
    | Some cell -> 
      replace_bucket key info cell

let set  h (key : key) value =
  let h_buckets = C.buckets h in 
  let i = hash key land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  match C.toOpt l with 
  | None -> 
    Bs_Array.unsafe_set h_buckets i (C.return (N.bucket ~key ~value ~next:l));
    C.sizeSet h (C.size h + 1);
    if C.size h > Array.length (C.buckets h) lsl 1 then resize h 
  | Some bucket -> 
    begin 
    if replace_bucket key value bucket then begin
      Bs_Array.unsafe_set h_buckets i (C.return 
                                       (N.bucket ~key ~value ~next:l));
      C.sizeSet h (C.size h + 1);
      if C.size h > Array.length (C.buckets h) lsl 1 then resize  h
    end   
    end

let rec remove_bucket h h_buckets  i (key : key) prec buckets =
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
    else remove_bucket  h h_buckets i key cell cell_next

let remove  h key =  
  let h_buckets = C.buckets h in 
  let i = hash key land (Array.length h_buckets - 1) in  
  let bucket = (Bs_Array.unsafe_get h_buckets i) in 
  match C.toOpt bucket with 
  | None -> ()
  | Some cell -> 
    if (N.key cell) = key then 
    begin 
      Bs_Array.unsafe_set h_buckets i (N.next cell);
      C.sizeSet h (C.size h - 1)
    end 
    else 
      remove_bucket  h h_buckets i key cell (N.next cell)



let rec findAux  (key : key) buckets = 
  match C.toOpt buckets with 
  | None ->
    None
  | Some cell ->
    if key = (N.key cell)  then Some (N.value cell) 
    else findAux key  (N.next cell)

let get  h (key : key) =
  let h_buckets = C.buckets h in 
  let nid = hash key  land (Array.length h_buckets - 1) in 
  match C.toOpt @@ Bs_Array.unsafe_get h_buckets nid with
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




let rec mem_in_bucket (key : key) cell = 
    (N.key cell)  = key  ||
    (match C.toOpt (N.next cell) with 
    | None -> false
    | Some nextCell -> mem_in_bucket  key nextCell
     )
    
let has  h key =
  let h_buckets = C.buckets h in 
  let nid = hash key land (Array.length h_buckets - 1) in 
  let bucket = Bs_Array.unsafe_get h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false
  | Some bucket -> 
    mem_in_bucket  key bucket


let create = C.create0
let clear = C.clear0
let reset = C.reset0
let size = C.length0
let forEach = N.iter0
let reduce = N.fold0
let logStats = N.logStats0
let filterMapDone = N.filterMapInplace0
let filterMap h f = filterMapDone h f; h
let toArray = N.toArray0 

let ofArray arr  = 
  let len = Bs.Array.length arr in 
  let v = create len in 
  for i = 0 to len - 1 do 
    let k,value = (Bs.Array.unsafe_get arr i) in
    set v k value
  done ;
  v

(* TOOD: optimize heuristics for resizing *)  
let mergeArrayDone h arr =   
  let len = Bs.Array.length arr in 
  for i = 0 to len - 1 do 
    let k,v = (Bs_Array.unsafe_get arr i) in
    set h k v 
done

let mergeArray h arr = mergeArrayDone h arr; h
