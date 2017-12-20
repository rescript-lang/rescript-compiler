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


module N = Bs_internalSetBuckets
module C = Bs_internalBucketsType
module B = Bs_Bag 
type ('a,'id) t0 = 'a N.t0 


type 'a bucket = 'a N.bucket

type ('a,'id) t = 
  (('a, 'id) Bs_Hash.t,
   ('a,'id) t0) B.bag 



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
    let h_buckets = C.makeSize nsize  in
    let ndata_tail = C.makeSize nsize  in (* keep track of tail *)
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



let rec remove_bucket ~eq h h_buckets  i key prec cell =
  let cell_next = N.next cell in 
  if (Bs_Hash.getEq eq) (N.key cell) key [@bs]
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
      remove_bucket ~eq h h_buckets i key cell cell_next

let remove0 ~hash ~eq h key =  
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in  
  let l = (Bs_Array.unsafe_get h_buckets i) in 
  match C.toOpt l with 
  | None -> ()
  | Some cell -> 
    let next_cell = (N.next cell) in 
    if (Bs_Hash.getEq eq) (N.key cell) key [@bs] then 
      begin 
        C.sizeSet h (C.size h - 1) ;
        Bs_Array.unsafe_set h_buckets i next_cell
      end
    else       
      match C.toOpt next_cell with 
      | None -> ()
      | Some next_cell -> 
        remove_bucket ~eq h h_buckets i key cell next_cell




let rec addBucket ~eq h key  cell = 
  if (Bs_Hash.getEq eq) (N.key cell) key [@bs]
  then
    N.keySet cell key
  else
    let  n = N.next cell in 
    match C.toOpt n with 
    | None ->  
      C.sizeSet h (C.size h + 1);
      N.nextSet cell (C.return @@ N.bucket ~key ~next:n)
    | Some n -> addBucket ~eq h key  n

let add0 
    ~hash:(hash:(_,'id)Bs_Hash.hash) 
    ~eq:(eq:(_,'id) Bs_Hash.eq)  (h : (_,'id) t0) key  =
  let h_buckets = C.buckets h in 
  let i = (Bs_Hash.getHash hash) key [@bs] land (Array.length h_buckets - 1) in 
  let l = Array.unsafe_get h_buckets i in  
  (match C.toOpt l with                                    
   | None -> 
     C.sizeSet h (C.size  h + 1);
     Bs_Array.unsafe_set h_buckets i 
       (C.return @@ N.bucket ~key ~next:C.emptyOpt)
   | Some cell -> 
     addBucket ~eq h key cell);
  if C.size h > Array.length (C.buckets h) lsl 1 then resize ~hash h


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
  let bucket = (Bs_Array.unsafe_get h_buckets nid) in 
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
let toArray0 = N.toArray0 
let toArray h = toArray0 (B.data h)
(*  Wrapper  *)
let create dict initialize_size = 
  B.bag ~data:(create0 initialize_size)
    ~dict 
let clear h = clear0 (B.data h)
let reset h = reset0 (B.data h)
let length h = length0 (B.data h)                 
let iter f h = iter0 f (B.data h)
let fold f h init = fold0 f (B.data h) init 
let logStats h = logStats0 (B.data h)

let add (type a) (type id) (h : (a,id) t) (key:a)  = 
  let dict,data = B.(dict h, data h) in 
  let module M = (val  dict) in 
  add0 ~hash:M.hash ~eq:M.eq data key 

let remove (type a)  (type id) (h : (a,id) t) (key : a) = 
  let dict,data = B.(dict h, data h) in
  let module M = (val dict) in   
  remove0 ~hash:M.hash ~eq:M.eq data key 


let replace (type a)(type id)  (h : (a,id) t) (key : a)  =
  let dict,data = B.(dict h, data h) in
  let module M = (val dict) in 
  add0 ~hash:M.hash ~eq:M.eq data key

let mem (type a) (type id) (h : (a,id) t) (key : a) =           
  let dict,data = B.(dict h, data h) in
  let module M = (val dict) in   
  mem0 ~hash:M.hash ~eq:M.eq data key   

