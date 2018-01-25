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

type ('a,'b) bucket = ('a,'b) N.bucket

type ('a,'b,'id) t = 
  (('a, 'id) Bs_Hash.t,
   ('a,'b,'id) t0) B.bag 



let rec insert_bucket ~hash ~h_buckets ~ndata_tail h old_bucket = 
  match C.toOpt old_bucket with 
  | None -> ()
  | Some cell ->
    let nidx = (Bs_Hash.getHash hash) (N.key cell) [@bs] land (Array.length h_buckets - 1) in 
    let v = C.return cell in 
    begin match C.toOpt (A.getUnsafe ndata_tail nidx) with
      | None -> 
        A.setUnsafe h_buckets nidx  v
      | Some tail ->
        N.nextSet tail v  (* cell put at the end *)            
    end;          
    A.setUnsafe ndata_tail nidx  v;
    insert_bucket ~hash ~h_buckets ~ndata_tail h (N.next cell)


let resize ~hash h =
  let odata = C.buckets h in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize >= osize then begin (* no overflow *)
    let h_buckets = A.makeUninitialized nsize  in
    let ndata_tail = A.makeUninitialized nsize  in (* keep track of tail *)
    C.bucketsSet h  h_buckets;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      insert_bucket ~hash ~h_buckets ~ndata_tail h (A.getUnsafe odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (A.getUnsafe ndata_tail i) with
      | None -> ()
      | Some tail -> N.nextSet tail C.emptyOpt
    done
  end


let add0 ~hash h key value =
  let h_buckets = C.buckets h in  
  let h_buckets_lenth = Array.length h_buckets in 
  let i = (Bs_Hash.getHash hash) key [@bs] land (h_buckets_lenth - 1) in 
  let bucket = 
    N.bucket ~key ~value ~next:(A.getUnsafe h_buckets i) in  
  A.setUnsafe h_buckets i  (C.return bucket);
  let h_new_size = C.size h + 1 in 
  C.sizeSet h  h_new_size;
  if h_new_size > h_buckets_lenth lsl 1 then resize ~hash  h


let rec remove_bucket ~eq h h_buckets  i key prec buckets =
  match C.toOpt buckets with
  | None -> ()
  | Some cell ->
    let cell_next = N.next cell in 
    if (Bs_Hash.getEq eq) (N.key cell) key [@bs]
    then 
      begin
        (match C.toOpt prec with
         | None -> A.setUnsafe h_buckets i  cell_next
         | Some c -> N.nextSet c cell_next);
        C.sizeSet h (C.size h - 1);        
      end
    else remove_bucket ~eq h h_buckets i key buckets cell_next

let remove0 ~hash ~eq h key =  
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in  
  remove_bucket ~eq h h_buckets i key C.emptyOpt (A.getUnsafe h_buckets i)

let rec removeAllBuckets ~eq h h_buckets  i key prec buckets =
  match C.toOpt buckets with
  | None -> ()
  | Some cell ->
    let cell_next = N.next cell in 
    if (Bs_Hash.getEq eq) (N.key cell) key [@bs]
    then 
      begin
        (match C.toOpt prec with
         | None -> A.setUnsafe h_buckets i  cell_next
         | Some c -> N.nextSet c cell_next);
        C.sizeSet h  (C.size h - 1);        
      end;
    removeAllBuckets ~eq h h_buckets i key buckets cell_next

let removeAll0 ~hash ~eq h key =
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in  
  removeAllBuckets ~eq h h_buckets i key C.emptyOpt (A.getUnsafe h_buckets i)


let rec find_rec ~eq key buckets = 
  match C.toOpt buckets with 
  | None ->
    None
  | Some cell ->
    if (Bs_Hash.getEq eq) key (N.key cell) [@bs] then Some (N.value cell)
    else find_rec ~eq key  (N.next cell)

let get0 ~hash ~eq h key =
  let h_buckets = C.buckets h in 
  let nid = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  match C.toOpt @@ A.getUnsafe h_buckets nid with
  | None -> None
  | Some cell1  ->
    if (Bs_Hash.getEq eq) key (N.key cell1) [@bs] then 
      Some  (N.value cell1)
    else
      match C.toOpt (N.next  cell1) with
      | None -> None
      | Some cell2 ->
        if (Bs_Hash.getEq eq) key 
            (N.key cell2) [@bs] then 
          Some (N.value cell2) else
          match C.toOpt (N.next cell2) with
          | None -> None
          | Some cell3 ->
            if (Bs_Hash.getEq eq) key 
                (N.key cell3) [@bs] then 
              Some (N.value cell3)
            else 
              find_rec ~eq key (N.next cell3)


let getAll0 ~hash ~eq h key =
  let rec find_in_bucket buckets = 
    match C.toOpt buckets with 
    | None ->
      []
    | Some cell ->
      if (Bs_Hash.getEq eq) 
          (N.key cell) key [@bs]
      then (N.value cell) :: find_in_bucket (N.next cell)
      else find_in_bucket (N.next cell)  in
  let h_buckets = C.buckets h in     
  let nid = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  find_in_bucket (A.getUnsafe h_buckets nid)

let rec replace_bucket ~eq  key info buckets = 
  match C.toOpt buckets with 
  | None ->
    true
  | Some cell ->
    if (Bs_Hash.getEq eq) (N.key cell) key [@bs]
    then
      begin
        N.keySet cell key;
        N.valueSet cell info;
        false
      end
    else
      replace_bucket ~eq key info (N.next cell)

let replace0 ~hash ~eq  h key info =
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  if replace_bucket ~eq key info l then begin
    A.setUnsafe h_buckets i (C.return 
                                       (N.bucket ~key ~value:info ~next:l));
    C.sizeSet h (C.size  h + 1);
    if C.size h > Array.length (C.buckets h) lsl 1 then resize ~hash h
    (* TODO: duplicate bucklets ? *)
  end 

let rec mem_in_bucket ~eq key cell = 
    (Bs_Hash.getEq eq) 
      (N.key cell) key [@bs] || 
      (match C.toOpt (N.next cell) with 
      | None -> false 
      | Some nextCell -> 
      mem_in_bucket ~eq key nextCell)

let has0 ~hash ~eq h key =
  let h_buckets = C.buckets h in 
  let nid = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  let bucket = (A.getUnsafe h_buckets nid) in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    mem_in_bucket ~eq key bucket


let create0 = C.create0
let clear0 = C.clear0
let size0 = C.size
let forEach0 = N.forEach0
let reduce0 = N.reduce0
let logStats0 = N.logStats0
let filterMapInplace0 = N.filterMapInplace0

(*  Wrapper  *)
let create dict initialize_size = 
  B.bag ~data:(create0 initialize_size) ~dict 
let clear h = clear0 (B.data h)

let size h = C.size (B.data h)                 
let forEach h f = N.forEach0 (B.data h) f
let reduce h init f = N.reduce0 (B.data h) init f
let logStats h = logStats0 (B.data h)

let add (type a) (type id) (h : (a,_,id) t) key info  = 
  let module M = (val B.dict h) in 
  add0 ~hash:M.hash (B.data h) key info 

let remove (type a) (type id) (h : (a,_,id) t) key = 
  let module M = (val B.dict h) in   
  remove0 ~hash:M.hash ~eq:M.eq (B.data h) key 

let removeAll (type a)  (type id) (h : (a,_,id) t) key = 
  let module M = (val B.dict h) in   
  removeAll0 ~hash:M.hash ~eq:M.eq (B.data h) key 

let get (type a) (type id) (h : (a,_,id) t) key =           
  let module M = (val B.dict h) in   
  get0 ~hash:M.hash ~eq:M.eq (B.data h) key 

let getAll (type a)  (type id) (h : (a,_,id) t) key =           
  let module M = (val B.dict h) in   
  getAll0 ~hash:M.hash ~eq:M.eq (B.data h) key   

let replace (type a)  (type id)  (h : (a,_,id) t) key info =
  let module M = (val B.dict h) in 
  replace0 ~hash:M.hash ~eq:M.eq (B.data h) key info

let has (type a)  (type id) (h : (a,_,id) t) key =           
  let module M = (val B.dict h) in   
  has0 ~hash:M.hash ~eq:M.eq (B.data h) key   

let filterMapDone h f =
  filterMapInplace0 (B.data h) f
