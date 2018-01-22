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
    begin match C.toOpt (Bs_Array.unsafe_get ndata_tail nidx) with
      | None -> 
        Bs_Array.unsafe_set h_buckets nidx  v
      | Some tail ->
        N.nextSet tail v  (* cell put at the end *)            
    end;          
    Bs_Array.unsafe_set ndata_tail nidx  v;
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
      insert_bucket ~hash ~h_buckets ~ndata_tail h (Bs_Array.unsafe_get odata i)
    done;
    for i = 0 to nsize - 1 do
      match C.toOpt (Bs_Array.unsafe_get ndata_tail i) with
      | None -> ()
      | Some tail -> N.nextSet tail C.emptyOpt
    done
  end

let rec replace_in_bucket ~eq  key info cell = 
  if (Bs_Hash.getEq eq) (N.key cell) key [@bs]
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
      replace_in_bucket ~eq key info cell

(* if [key] already exists, replace it, otherwise add it 
   Here we add it to the head, it could be tail
*)      
let add0 ~hash ~eq  h key value =
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  match C.toOpt l with  
  | None -> 
    Bs_Array.unsafe_set h_buckets i (C.return 
                                       (N.bucket ~key ~value ~next:l));
    C.sizeSet h (C.size  h + 1);
    if C.size h > Array.length (C.buckets h) lsl 1 then resize ~hash h
    (* do we really need resize here ? *)
  | Some bucket -> 
    begin 
      if replace_in_bucket ~eq key value bucket then begin
        Bs_Array.unsafe_set h_buckets i (C.return 
                                           (N.bucket ~key ~value ~next:l));
        C.sizeSet h (C.size  h + 1);
        if C.size h > Array.length (C.buckets h) lsl 1 then resize ~hash h
        (* TODO: duplicate bucklets ? *)
      end 
    end

let rec remove_bucket ~eq h h_buckets  i key prec bucket =
  match C.toOpt bucket with
  | None -> ()
  | Some cell ->
    let cell_next = N.next cell in 
    if (Bs_Hash.getEq eq) (N.key cell) key [@bs]
    then 
      begin        
        N.nextSet prec cell_next ; 
        C.sizeSet h (C.size h - 1);        
      end
    else remove_bucket ~eq h h_buckets i key cell cell_next

let remove0 ~hash ~eq h key =  
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in  
  let bucket = Bs_Array.unsafe_get h_buckets i in 
  match C.toOpt bucket with 
  | None -> ()
  | Some cell -> 
    if (Bs_Hash.getEq eq) (N.key cell ) key [@bs] then 
    begin 
      Bs_Array.unsafe_set h_buckets i (N.next cell);
      C.sizeSet h (C.size h - 1)
    end 
    else  
      remove_bucket ~eq h h_buckets i key  cell (N.next cell)


let rec findAux ~eq key buckets = 
  match C.toOpt buckets with 
  | None ->
    None
  | Some cell ->
    if (Bs_Hash.getEq eq) key (N.key cell) [@bs] then Some (N.value cell)
    else findAux ~eq key  (N.next cell)

let findOpt0 ~hash ~eq h key =
  let h_buckets = C.buckets h in 
  let nid = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  match C.toOpt @@ Bs_Array.unsafe_get h_buckets nid with
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
              findAux ~eq key (N.next cell3)





let rec mem_in_bucket ~eq key cell = 
  (Bs_Hash.getEq eq) 
    (N.key cell) key [@bs] || 
  (match C.toOpt (N.next cell) with 
   | None -> false 
   | Some nextCell -> 
     mem_in_bucket ~eq key nextCell)

let mem0 ~hash ~eq h key =
  let h_buckets = C.buckets h in 
  let nid = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  let bucket = Bs_Array.unsafe_get h_buckets nid in 
  match C.toOpt bucket with 
  | None -> false 
  | Some bucket -> 
    mem_in_bucket ~eq key bucket


let create0 = C.create0
let clear0 = C.clear0
let reset0 = C.reset0
let length0 = C.length0
let iter0 = N.iter0
let fold0 = N.fold0
let logStats0 = N.logStats0
let filterMapInplace0 = N.filterMapInplace0
let toArray0 = N.toArray0

(*  Wrapper  *)
let create dict initialize_size = 
  B.bag ~data:(create0 initialize_size)
    ~dict 
let clear h = clear0 (B.data h)
let reset h = reset0 (B.data h)
let length h = length0 (B.data h)                 
let iter h f = iter0 (B.data h) f
let fold h init f = fold0 (B.data h) init f
let logStats h = logStats0 (B.data h)

let add (type a) (type b ) (type id) (h : (a,b,id) t) (key:a) (info:b) = 
  let dict,data = B.(dict h, data h) in 
  let module M = (val  dict) in 
  add0 ~hash:M.hash ~eq:M.eq data key info 

let remove (type a) (type b) (type id) (h : (a,b,id) t) (key : a) = 
  let dict,data = B.(dict h, data h) in
  let module M = (val dict) in   
  remove0 ~hash:M.hash ~eq:M.eq data key 


let findOpt (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let dict,data = B.(dict h, data h) in
  let module M = (val dict) in   
  findOpt0 ~hash:M.hash ~eq:M.eq data key 




let mem (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let dict,data = B.(dict h, data h) in
  let module M = (val dict) in   
  mem0 ~hash:M.hash ~eq:M.eq data key   

let filterMapInplace h f =
  filterMapInplace0 (B.data h) f
let toArray (type a) (type b) (type id) (h : (a,b,id) t) = 
  toArray0 (B.data h)
let ofArray0  ~hash ~eq arr  = 
  let len = Bs.Array.length arr in 
  let v = create0 len in 
  for i = 0 to len - 1 do 
    let key,value = (Bs.Array.unsafe_get arr i) in 
    add0 ~eq ~hash v key value
  done ;
  v

(* TOOD: optimize heuristics for resizing *)  
let addArray0 ~hash ~eq  h arr =   
  let len = Bs.Array.length arr in 
  for i = 0 to len - 1 do 
    let key,value = (Bs_Array.unsafe_get arr i) in 
    add0 h  ~eq ~hash key value
  done 

let ofArray (type a) (type id)
    ~dict:(dict:(a,id) Bs_Hash.t) arr =     
  let module M = (val dict) in 
  B.bag ~dict 
    ~data:M.(ofArray0 ~eq~hash arr)

let addArray (type a) (type b) (type id)
    (h : (a,b,id) t) arr = 
  let dict,data = B.(dict h, data h) in 
  let module M = (val dict) in
  M.(addArray0 ~hash ~eq data arr)

let keys0 = N.keys0  
let keys h =
  keys0 (B.data h)
let values0 = N.values0  
let values h = N.values0 (B.data h)

let getData = B.data
let getDict = B.dict
let packDictData = B.bag 
