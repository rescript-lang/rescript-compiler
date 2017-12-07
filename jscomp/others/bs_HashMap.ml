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
(* Hash tables *)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b,'id) t0 =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    initial_size: int;                        (* initial array size *)
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist


let rec power_2_above x n =
  if x >= n then x
  else if x * 2 < x then x (* overflow *)
  else power_2_above (x * 2) n

let create  initial_size =
  let s = power_2_above 16 initial_size in
  { initial_size = s; size = 0;  data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let h_data = h.data in 
  let len = Array.length h_data in
  for i = 0 to len - 1 do
    Array.unsafe_set h_data i  Empty
  done

let reset h =
  let len = Array.length h.data in
  let h_initial_size = h.initial_size in
  if len = h_initial_size then
    clear h
  else begin
    h.size <- 0;
    h.data <- Array.make h_initial_size Empty
  end


let length h = h.size

let rec do_bucket_iter ~f = function
  | Empty ->
    ()
  | Cons(k, d, rest) ->
    f k d [@bs]; do_bucket_iter ~f rest 

let iter f h =
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket_iter f (Array.unsafe_get d i)
  done

let rec do_bucket_fold ~f b accu =
  match b with
    Empty ->
    accu
  | Cons(k, d, rest) ->
    do_bucket_fold ~f rest (f k d accu [@bs]) 

let fold f h init =
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket_fold ~f (Array.unsafe_get d i) !accu
  done;
  !accu

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

let rec bucket_length accu = function
  | Empty -> accu
  | Cons(_, _, rest) -> bucket_length (accu + 1) rest

let max (m : int) n = if m > n then m else n  

let stats h =
  let mbl =
    Array.fold_left (fun m b -> max m (bucket_length 0 b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
       let l = bucket_length 0 b in
       histo.(l) <- histo.(l) + 1)
    h.data;
  { num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo }



type ('a,'b,'id) t = {
  hash : ('a, 'id) Bs_Hash.t;
  table : ('a,'b,'id) t0;

}


let key_index ~hash (h : ('a, _,_) t0) (key : 'a) =
  ((Bs_Hash.getHash hash) key [@bs]) land (Array.length h.data - 1)

let rec insert_bucket_list ~hash ~ndata h = function
    Empty -> ()
  | Cons(key, data, rest) ->
    insert_bucket_list ~hash ~ndata h rest; (* preserve original order of elements *)
    let nidx = key_index ~hash h key in
    Array.unsafe_set ndata nidx 
      (Cons(key, data, Array.unsafe_get ndata nidx )) 

let resize ~hash  h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if  nsize >= osize then begin 
    let ndata = Array.make nsize Empty in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      insert_bucket_list ~hash ~ndata h (Array.unsafe_get odata i)
    done
  end

let add0 ~hash h key info =
  let i = key_index ~hash h key in
  let bucket = Cons(key, info, h.data.(i)) in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize ~hash  h

let add (type a) (type b ) (type id) (h : (a,b,id) t) (key:a) (info:b) = 
  let module M = (val  h.hash) in 
  add0 ~hash:M.hash h.table key info 

let rec remove_bucket ~eq key h = function
  | Empty ->
    Empty
  | Cons(k, i, next) ->
    if (Bs_Hash.getEq eq) k key [@bs]
    then begin h.size <- h.size - 1; next end
    else Cons(k, i, remove_bucket ~eq key h next) 

let remove0 ~hash ~eq h key =
  let i = key_index ~hash h key in
  let h_data = h.data in 
  Array.unsafe_set h_data i
    (remove_bucket ~eq key h (Array.unsafe_get h_data i))

let remove (type a) (type b) (type id) (h : (a,b,id) t) (key : a) = 
  let module M = (val h.hash) in   
  remove0 ~hash:M.hash ~eq:M.eq h.table key 

let rec find_rec ~eq key = function
  | Empty ->
    raise Not_found
  | Cons(k, d, rest) ->
    if (Bs_Hash.getEq eq) key k [@bs] then d else find_rec ~eq key  rest

let find0 ~hash ~eq h key =
  match h.data.(key_index ~hash h key) with
  | Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
    if (Bs_Hash.getEq eq) key k1 [@bs] then d1 else
      match rest1 with
      | Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
        if (Bs_Hash.getEq eq) key k2 [@bs] then d2 else
          match rest2 with
          | Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
            if (Bs_Hash.getEq eq) key k3 [@bs] then d3 else find_rec ~eq key rest3

let find (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let module M = (val h.hash) in   
  find0 ~hash:M.hash ~eq:M.eq h.table key 

let find_all0 ~hash ~eq h key =
  let rec find_in_bucket = function
    | Empty ->
      []
    | Cons(k, d, rest) ->
      if (Bs_Hash.getEq eq) k key [@bs]
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket h.data.(key_index ~hash h key)

let find_all (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let module M = (val h.hash) in   
  find_all0 ~hash:M.hash ~eq:M.eq h.table key   

let replace0 ~hash ~eq  h key info =
  let rec replace_bucket = function
    | Empty ->
      raise Not_found
    | Cons(k, i, next) ->
      if (Bs_Hash.getEq eq) k key [@bs]
      then Cons(key, info, next)
      else Cons(k, i, replace_bucket next) in
  let i = key_index ~hash h key in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Not_found ->
    h.data.(i) <- Cons(key, info, l);
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1 then resize ~hash  h

let replace (type a) (type b) (type id)  (h : (a,b,id) t) (key : a) (info : b) =
  let module M = (val h.hash) in 
  replace0 ~hash:M.hash ~eq:M.eq h.table key info
    
let rec mem_in_bucket ~eq key = function
  | Empty ->
    false
  | Cons(k, d, rest) ->
    (Bs_Hash.getEq eq) k key [@bs] || mem_in_bucket ~eq key rest     

let mem0 ~hash ~eq h key =
  mem_in_bucket ~eq key (Array.unsafe_get h.data (key_index ~hash h key))

let mem (type a) (type b) (type id) (h : (a,b,id) t) (key : a) =           
  let module M = (val h.hash) in   
  mem0 ~hash:M.hash ~eq:M.eq h.table key   
  

