# 9 "hashmap.cppo.ml"
type key = int
type seed = int
external caml_hash_mix_int : seed -> int -> seed  = "caml_hash_mix_int"
external final_mix : seed -> seed = "caml_hash_final_mix"
let hash (s : key) = 
  final_mix (caml_hash_mix_int 0 s)

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


type ('a,'b) buckets = ('a,'b) Bs_internalBuckets.buckets =
  {
    mutable key : 'a ; 
    mutable value : 'b ; 
    mutable next : ('a, 'b) buckets Bs_internalBuckets.opt
  }

type 'b t = (key,'b,unit) t0


let rec insert_bucket  ~h_buckets ~ndata_tail h old_bucket = 
  match Bs_internalBuckets.toOpt old_bucket with 
  | None -> ()
  | Some ({key; next} as cell) ->
    (* let nidx = key_index ~hash h key in *)
    let nidx = hash key land (Array.length h_buckets - 1) in 
    let v = Bs_internalBuckets.return cell in 
    begin match Bs_internalBuckets.toOpt (Bs_Array.unsafe_get ndata_tail nidx) with
      | None -> 
        Bs_Array.unsafe_set h_buckets nidx  v
      | Some tail ->
        tail.next <- v ; (* cell put at the end *)            
    end;          
    Bs_Array.unsafe_set ndata_tail nidx  v;
    insert_bucket  ~h_buckets ~ndata_tail h next


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
      | Some tail -> tail.next <- Bs_internalBuckets.emptyOpt
    done
  end


let add  h key value =
  let h_buckets = h.buckets in  
  let h_buckets_lenth = Array.length h_buckets in 
  let i =  hash key land (h_buckets_lenth - 1) in 
  let bucket = {key; value; next= Bs_Array.unsafe_get h_buckets i} in  
  Bs_Array.unsafe_set h_buckets i  (Bs_internalBuckets.return bucket);
  let h_new_size = h.size + 1 in 
  h.size <- h_new_size;
  if h_new_size > h_buckets_lenth lsl 1 then resize  h


let rec remove_bucket h h_buckets  i key prec buckets =
  match Bs_internalBuckets.toOpt buckets with
  | None -> ()
  | Some {key=k; next} ->
    if  (k : key) = key 
    then 
      begin
        (match Bs_internalBuckets.toOpt prec with
         | None -> Bs_Array.unsafe_set h_buckets i  next
         | Some c -> c.next <- next);
        h.size <- h.size - 1;        
      end
    else remove_bucket  h h_buckets i key buckets next

let remove  h key =  
  let h_buckets = h.buckets in 
  (* let i = key_index ~hash h key in *)
  let i = hash key land (Array.length h_buckets - 1) in  
  remove_bucket  h h_buckets i key Bs_internalBuckets.emptyOpt (Bs_Array.unsafe_get h_buckets i)

let rec removeAllBuckets h h_buckets  i key prec buckets =
  match Bs_internalBuckets.toOpt buckets with
  | None -> ()
  | Some {key=k; next} ->
    if  (k : key) = key
    then 
      begin
        (match Bs_internalBuckets.toOpt prec with
         | None -> Bs_Array.unsafe_set h_buckets i  next
         | Some c -> c.next <- next);
        h.size <- h.size - 1;        
      end;
    removeAllBuckets h h_buckets i key buckets next

let removeAll0  h key =
  let h_buckets = h.buckets in 
  (* let i = key_index ~hash h key in *)
  let i = hash key  land (Array.length h_buckets - 1) in  
  removeAllBuckets h h_buckets i key Bs_internalBuckets.emptyOpt (Bs_Array.unsafe_get h_buckets i)


(* TODO: add [removeAll] *)


let rec find_rec  key buckets = 
  match Bs_internalBuckets.toOpt buckets with 
  | None ->
    None
  | Some { key = k; value = d; next =  rest} ->
    if (key : key) = k  then Some d else find_rec key  rest

let findOpt  h (key : key) =
  let h_buckets = h.buckets in 
  (* let nid = key_index ~hash h key in  *)
  let nid = hash key  land (Array.length h_buckets - 1) in 
  match Bs_internalBuckets.toOpt @@ Bs_Array.unsafe_get h_buckets nid with
  | None -> None
  | Some {key = k1; value  = d1; next =  rest1} ->
    if key =k1  then Some d1 else
      match Bs_internalBuckets.toOpt rest1 with
      | None -> None
      | Some {key = k2; value =  d2; next =  rest2} ->
        if  key = k2  then Some d2 else
          match Bs_internalBuckets.toOpt rest2 with
          | None -> None
          | Some { key = k3; value = d3; next =  rest3} ->
            if  key = k3  then Some d3 else find_rec  key rest3


let findAll  h (key : key) =
  let rec find_in_bucket buckets = 
    match Bs_internalBuckets.toOpt buckets with 
    | None ->
      []
    | Some {key = k; value = d; next =  rest} ->
      if  k = key 
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  let h_buckets = h.buckets in     
  (* let nid = key_index ~hash h key in  *)
  let nid = hash key land (Array.length h_buckets - 1) in 
  find_in_bucket (Bs_Array.unsafe_get h_buckets nid)

let rec replace_bucket  (key : key) info buckets = 
  match Bs_internalBuckets.toOpt buckets with 
  | None ->
    true
  | Some ({key = k; value =  i; next} as slot) ->
    if  k = key 
    then
      begin
        slot.key <- key;
        slot.value <- info;
        false
      end
    else
      replace_bucket key info next

let replace  h (key : key) info =
  let h_buckets = h.buckets in 
  (* let i = key_index ~hash h key in *)
  let i = hash key land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  if replace_bucket  key info l then begin
    Bs_Array.unsafe_set h_buckets i (Bs_internalBuckets.return {key ; value = info; next = l});
    h.size <- h.size + 1;
    if h.size > Array.length h.buckets lsl 1 then resize  h
  end 

let rec mem_in_bucket (key : key) buckets = 
  match Bs_internalBuckets.toOpt buckets with 
  | None ->
    false
  | Some {key = k; value = d; next =  rest} ->
     k  = key  || mem_in_bucket  key rest     
let mem  h key =
  let h_buckets = h.buckets in 
  (* let nid = (key_index ~hash h key) in  *)
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

