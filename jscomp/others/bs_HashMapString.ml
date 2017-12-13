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
type ('a, 'b,'id) t0 = ('a,'b,'id) Bs_internalBuckets.t0 =
  {
    mutable size: int;                        (* number of entries *)
    mutable buckets: ('a, 'b) Bs_internalBuckets.bucketlist array;  (* the buckets *)
    initial_size: int;                        (* initial array size *)
  }

type ('a,'b) buckets 

type 'b t = (key,'b,unit) t0


let rec insert_bucket  ~h_buckets ~ndata_tail h old_bucket = 
  match Bs_internalBuckets.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = hash (Bs_internalBuckets.key cell) land (Array.length h_buckets - 1) in 
    let v = Bs_internalBuckets.return cell in 
    begin match Bs_internalBuckets.toOpt (Bs_Array.unsafe_get ndata_tail nidx) with
      | None -> 
        Bs_Array.unsafe_set h_buckets nidx  v
      | Some tail ->
        Bs_internalBuckets.nextSet tail v  (* cell put at the end *)            
    end;          
    Bs_Array.unsafe_set ndata_tail nidx  v;
    insert_bucket  ~h_buckets ~ndata_tail h (Bs_internalBuckets.next cell)


let resize  h =
  let odata = h.buckets in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = Bs_internalBuckets.makeSize nsize  in
    let ndata_tail = Bs_internalBuckets.makeSize nsize  in (* keep track of tail *)
    h.buckets <- h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      insert_bucket  ~h_buckets ~ndata_tail h (Bs_Array.unsafe_get odata i)
    done;
    for i = 0 to nsize - 1 do
      match Bs_internalBuckets.toOpt (Bs_Array.unsafe_get ndata_tail i) with
      | None -> ()
      | Some tail -> Bs_internalBuckets.nextSet tail Bs_internalBuckets.emptyOpt
    done
  end


let add  h key value =
  let h_buckets = h.buckets in  
  let h_buckets_lenth = Array.length h_buckets in 
  let i =  hash key land (h_buckets_lenth - 1) in 
  let bucket = 
    Bs_internalBuckets.newBuckets ~key ~value ~next:(Bs_Array.unsafe_get h_buckets i) in  
  Bs_Array.unsafe_set h_buckets i  (Bs_internalBuckets.return bucket);
  let h_new_size = h.size + 1 in 
  h.size <- h_new_size;
  if h_new_size > h_buckets_lenth lsl 1 then resize  h


let rec remove_bucket h h_buckets  i (key : key) prec buckets =
  match Bs_internalBuckets.toOpt buckets with
  | None -> ()
  | Some cell  ->
    let cell_next = Bs_internalBuckets.next cell in 
    if  Bs_internalBuckets.key cell = key 
    then 
      begin
        (match Bs_internalBuckets.toOpt prec with
         | None -> Bs_Array.unsafe_set h_buckets i  cell_next
         | Some c -> Bs_internalBuckets.nextSet c cell_next);
        h.size <- h.size - 1;        
      end
    else remove_bucket  h h_buckets i key buckets cell_next

let remove  h key =  
  let h_buckets = h.buckets in 
  let i = hash key land (Array.length h_buckets - 1) in  
  remove_bucket  h h_buckets i key Bs_internalBuckets.emptyOpt (Bs_Array.unsafe_get h_buckets i)

let rec removeAllBuckets h h_buckets  i (key : key) prec buckets =
  match Bs_internalBuckets.toOpt buckets with
  | None -> ()
  | Some cell ->
    let cell_next = Bs_internalBuckets.next cell in 
    if  Bs_internalBuckets.key cell = key
    then 
      begin
        (match Bs_internalBuckets.toOpt prec with
         | None -> Bs_Array.unsafe_set h_buckets i  cell_next
         | Some c -> Bs_internalBuckets.nextSet c cell_next);
        h.size <- h.size - 1;        
      end;
    removeAllBuckets h h_buckets i key buckets cell_next

let removeAll0  h key =
  let h_buckets = h.buckets in 
  let i = hash key  land (Array.length h_buckets - 1) in  
  removeAllBuckets h h_buckets i key Bs_internalBuckets.emptyOpt (Bs_Array.unsafe_get h_buckets i)


(* TODO: add [removeAll] *)


let rec find_rec  (key : key) buckets = 
  match Bs_internalBuckets.toOpt buckets with 
  | None ->
    None
  | Some cell ->
    if key = (Bs_internalBuckets.key cell)  then Some (Bs_internalBuckets.value cell) 
    else find_rec key  (Bs_internalBuckets.next cell)

let findOpt  h (key : key) =
  let h_buckets = h.buckets in 
  let nid = hash key  land (Array.length h_buckets - 1) in 
  match Bs_internalBuckets.toOpt @@ Bs_Array.unsafe_get h_buckets nid with
  | None -> None
  | Some cell1 ->
    if key = (Bs_internalBuckets.key cell1)  then Some (Bs_internalBuckets.value cell1) else
      match Bs_internalBuckets.toOpt (Bs_internalBuckets.next cell1) with
      | None -> None
      | Some cell2 ->
        if  key = (Bs_internalBuckets.key cell2)  then Some (Bs_internalBuckets.value cell2) else
          match Bs_internalBuckets.toOpt (Bs_internalBuckets.next cell2) with
          | None -> None
          | Some cell3 ->
            if  key = (Bs_internalBuckets.key cell3)  then Some (Bs_internalBuckets.value cell3)
            else find_rec  key (Bs_internalBuckets.next cell3)


let findAll  h (key : key) =
  let rec find_in_bucket buckets = 
    match Bs_internalBuckets.toOpt buckets with 
    | None ->
      []
    | Some cell -> 
      if  (Bs_internalBuckets.key cell) = key 
      then (Bs_internalBuckets.value cell) :: find_in_bucket (Bs_internalBuckets.next cell)
      else find_in_bucket (Bs_internalBuckets.next cell) in
  let h_buckets = h.buckets in     
  let nid = hash key land (Array.length h_buckets - 1) in 
  find_in_bucket (Bs_Array.unsafe_get h_buckets nid)

let rec replace_bucket  (key : key) info buckets = 
  match Bs_internalBuckets.toOpt buckets with 
  | None ->
    true
  | Some cell ->
    if  (Bs_internalBuckets.key cell) = key 
    then
      begin
        Bs_internalBuckets.keySet cell key;
        Bs_internalBuckets.valueSet cell info;
        false
      end
    else
      replace_bucket key info (Bs_internalBuckets.next cell)

let replace  h (key : key) info =
  let h_buckets = h.buckets in 
  let i = hash key land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  if replace_bucket  key info l then begin
    Bs_Array.unsafe_set h_buckets i (Bs_internalBuckets.return 
                                       (Bs_internalBuckets.newBuckets ~key ~value:info ~next:l));
    h.size <- h.size + 1;
    if h.size > Array.length h.buckets lsl 1 then resize  h
  end 

let rec mem_in_bucket (key : key) buckets = 
  match Bs_internalBuckets.toOpt buckets with 
  | None ->
    false
  | Some cell ->
    (Bs_internalBuckets.key cell)  = key  || mem_in_bucket  key (Bs_internalBuckets.next cell)
let mem  h key =
  let h_buckets = h.buckets in 
  let nid = hash key land (Array.length h_buckets - 1) in 
  mem_in_bucket  key (Bs_Array.unsafe_get h_buckets nid)


let create = Bs_internalBuckets.create0
let clear = Bs_internalBuckets.clear0
let reset = Bs_internalBuckets.reset0
let length = Bs_internalBuckets.length0
let iter = Bs_internalBuckets.iter0
let fold = Bs_internalBuckets.fold0
let logStats = Bs_internalBuckets.logStats0
let filterMapInplace = Bs_internalBuckets.filterMapInplace0

