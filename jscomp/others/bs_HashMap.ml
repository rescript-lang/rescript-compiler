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

type ('a,'b,'id) t = {
  dict : ('a, 'id) Bs_Hash.t;
  data : ('a,'b,'id) t0;
}


let rec insert_bucket ~hash ~h_buckets ~ndata_tail h old_bucket = 
  match Bs_internalBuckets.toOpt old_bucket with 
  | None -> ()
  | Some ({key; next} as cell) ->
    (* let nidx = key_index ~hash h key in *)
    let nidx = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
    let v = Bs_internalBuckets.return cell in 
    begin match Bs_internalBuckets.toOpt (Bs_Array.unsafe_get ndata_tail nidx) with
      | None -> 
        Bs_Array.unsafe_set h_buckets nidx  v
      | Some tail ->
        tail.next <- v ; (* cell put at the end *)            
    end;          
    Bs_Array.unsafe_set ndata_tail nidx  v;
    insert_bucket ~hash ~h_buckets ~ndata_tail h next


let resize ~hash h =
  let odata = h.buckets in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = Bs_internalBuckets.makeSize nsize  in
    let ndata_tail = Bs_internalBuckets.makeSize nsize  in (* keep track of tail *)
    h.buckets <- h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      insert_bucket ~hash ~h_buckets ~ndata_tail h (Bs_Array.unsafe_get odata i)
    done;
    for i = 0 to nsize - 1 do
      match Bs_internalBuckets.toOpt (Bs_Array.unsafe_get ndata_tail i) with
      | None -> ()
      | Some tail -> tail.next <- Bs_internalBuckets.emptyOpt
    done
  end


let add0 ~hash h key value =
  let h_buckets = h.buckets in  
  let h_buckets_lenth = Array.length h_buckets in 
  let i = (Bs_Hash.getHash hash) key [@bs] land (h_buckets_lenth - 1) in 
  let bucket = {key; value; next= Bs_Array.unsafe_get h_buckets i} in  
  Bs_Array.unsafe_set h_buckets i  (Bs_internalBuckets.return bucket);
  let h_new_size = h.size + 1 in 
  h.size <- h_new_size;
  if h_new_size > h_buckets_lenth lsl 1 then resize ~hash  h


let rec remove_bucket ~eq h h_buckets  i key prec buckets =
  match Bs_internalBuckets.toOpt buckets with
  | None -> ()
  | Some {key=k; next} ->
    if (Bs_Hash.getEq eq) k key [@bs]
    then 
      begin
        (match Bs_internalBuckets.toOpt prec with
         | None -> Bs_Array.unsafe_set h_buckets i  next
         | Some c -> c.next <- next);
        h.size <- h.size - 1;        
      end
    else remove_bucket ~eq h h_buckets i key buckets next

let remove0 ~hash ~eq h key =  
  let h_buckets = h.buckets in 
  (* let i = key_index ~hash h key in *)
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in  
  remove_bucket ~eq h h_buckets i key Bs_internalBuckets.emptyOpt (Bs_Array.unsafe_get h_buckets i)

let rec removeAllBuckets ~eq h h_buckets  i key prec buckets =
  match Bs_internalBuckets.toOpt buckets with
  | None -> ()
  | Some {key=k; next} ->
    if (Bs_Hash.getEq eq) k key [@bs]
    then 
      begin
        (match Bs_internalBuckets.toOpt prec with
         | None -> Bs_Array.unsafe_set h_buckets i  next
         | Some c -> c.next <- next);
        h.size <- h.size - 1;        
      end;
    removeAllBuckets ~eq h h_buckets i key buckets next

let removeAll0 ~hash ~eq h key =
  let h_buckets = h.buckets in 
  (* let i = key_index ~hash h key in *)
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in  
  removeAllBuckets ~eq h h_buckets i key Bs_internalBuckets.emptyOpt (Bs_Array.unsafe_get h_buckets i)


(* TODO: add [removeAll] *)


let rec find_rec ~eq key buckets = 
  match Bs_internalBuckets.toOpt buckets with 
  | None ->
    None
  | Some { key = k; value = d; next =  rest} ->
    if (Bs_Hash.getEq eq) key k [@bs] then Some d else find_rec ~eq key  rest

let findOpt0 ~hash ~eq h key =
  let h_buckets = h.buckets in 
  (* let nid = key_index ~hash h key in  *)
  let nid = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  match Bs_internalBuckets.toOpt @@ Bs_Array.unsafe_get h_buckets nid with
  | None -> None
  | Some {key = k1; value  = d1; next =  rest1} ->
    if (Bs_Hash.getEq eq) key k1 [@bs] then Some d1 else
      match Bs_internalBuckets.toOpt rest1 with
      | None -> None
      | Some {key = k2; value =  d2; next =  rest2} ->
        if (Bs_Hash.getEq eq) key k2 [@bs] then Some d2 else
          match Bs_internalBuckets.toOpt rest2 with
          | None -> None
          | Some { key = k3; value = d3; next =  rest3} ->
            if (Bs_Hash.getEq eq) key k3 [@bs] then Some d3 else find_rec ~eq key rest3


let findAll0 ~hash ~eq h key =
  let rec find_in_bucket buckets = 
    match Bs_internalBuckets.toOpt buckets with 
    | None ->
      []
    | Some {key = k; value = d; next =  rest} ->
      if (Bs_Hash.getEq eq) k key [@bs]
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  let h_buckets = h.buckets in     
  (* let nid = key_index ~hash h key in  *)
  let nid = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  find_in_bucket (Bs_Array.unsafe_get h_buckets nid)

let rec replace_bucket ~eq  key info buckets = 
  match Bs_internalBuckets.toOpt buckets with 
  | None ->
    true
  | Some ({key = k; value =  i; next} as slot) ->
    if (Bs_Hash.getEq eq) k key [@bs]
    then
      begin
        slot.key <- key;
        slot.value <- info;
        false
      end
    else
      replace_bucket ~eq key info next

let replace0 ~hash ~eq  h key info =
  let h_buckets = h.buckets in 
  (* let i = key_index ~hash h key in *)
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  if replace_bucket ~eq key info l then begin
    Bs_Array.unsafe_set h_buckets i (Bs_internalBuckets.return {key ; value = info; next = l});
    h.size <- h.size + 1;
    if h.size > Array.length h.buckets lsl 1 then resize ~hash h
  end 

let rec mem_in_bucket ~eq key buckets = 
  match Bs_internalBuckets.toOpt buckets with 
  | None ->
    false
  | Some {key = k; value = d; next =  rest} ->
    (Bs_Hash.getEq eq) k key [@bs] || mem_in_bucket ~eq key rest     
let mem0 ~hash ~eq h key =
  let h_buckets = h.buckets in 
  (* let nid = (key_index ~hash h key) in  *)
  let nid = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  mem_in_bucket ~eq key (Bs_Array.unsafe_get h_buckets nid)


let create0 = Bs_internalBuckets.create0
let clear0 = Bs_internalBuckets.clear0
let reset0 = Bs_internalBuckets.reset0
let length0 = Bs_internalBuckets.length0
let iter0 = Bs_internalBuckets.iter0
let fold0 = Bs_internalBuckets.fold0
let logStats0 = Bs_internalBuckets.logStats0
let filterMapInplace0 = Bs_internalBuckets.filterMapInplace0

(*  Wrapper  *)
let create dict initialize_size = 
  { data  = create0 initialize_size  ;
    dict }
let clear h = clear0 h.data
let reset h = reset0 h.data
let length h = length0 h.data                  
let iter f h = iter0 f h.data
let fold f h init = fold0 f h.data init 
let logStats h = logStats0 h.data

let add (type a) (type b ) (type id) (h : (a,b,id) t) (key:a) (info:b) = 
  let module M = (val  h.dict) in 
  add0 ~hash:M.hash h.data key info 

let remove (type a) (type b) (type id) (h : (a,b,id) t) (key : a) = 
  let module M = (val h.dict) in   
  remove0 ~hash:M.hash ~eq:M.eq h.data key 

let removeAll (type a) (type b) (type id) (h : (a,b,id) t) (key : a) = 
  let module M = (val h.dict) in   
  removeAll0 ~hash:M.hash ~eq:M.eq h.data key 
  
let findOpt (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let module M = (val h.dict) in   
  findOpt0 ~hash:M.hash ~eq:M.eq h.data key 

let findAll (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let module M = (val h.dict) in   
  findAll0 ~hash:M.hash ~eq:M.eq h.data key   

let replace (type a) (type b) (type id)  (h : (a,b,id) t) (key : a) (info : b) =
  let module M = (val h.dict) in 
  replace0 ~hash:M.hash ~eq:M.eq h.data key info

let mem (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let module M = (val h.dict) in   
  mem0 ~hash:M.hash ~eq:M.eq h.data key   

let filterMapInplace  f h =
  filterMapInplace0 f h.data
