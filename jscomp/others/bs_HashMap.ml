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
    mutable buckets: ('a, 'b) bucketlist array;  (* the buckets *)
    initial_size: int;                        (* initial array size *)
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

type ('a,'b,'id) t = {
  dict : ('a, 'id) Bs_Hash.t;
  data : ('a,'b,'id) t0;

}

(* type ('a,'b) buckets =  {
   mutable key : 'a ; 
   mutable data : 'b ; 
   next : ('a, 'b) buckets Js.null
   } *)

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}


let rec power_2_above x n =
  if x >= n then x
  else if x * 2 < x then x (* overflow *)
  else power_2_above (x * 2) n

let create0  initial_size =
  let s = power_2_above 16 initial_size in
  { initial_size = s; size = 0;  buckets = Array.make s Empty }

let clear0 h =
  h.size <- 0;
  let h_buckets = h.buckets in 
  let len = Array.length h_buckets in
  for i = 0 to len - 1 do
    Bs_Array.unsafe_set h_buckets i  Empty
  done

let reset0 h =
  let len = Array.length h.buckets in
  let h_initial_size = h.initial_size in
  if len = h_initial_size then
    clear0 h
  else begin
    h.size <- 0;
    h.buckets <- Array.make h_initial_size Empty
  end

let length0 h = h.size


let rec do_bucket_iter ~f = function
  | Empty ->
    ()
  | Cons(k, d, rest) ->
    f k d [@bs]; do_bucket_iter ~f rest 

let iter0 f h =
  let d = h.buckets in
  for i = 0 to Array.length d - 1 do
    do_bucket_iter f (Bs_Array.unsafe_get d i)
  done


let rec do_bucket_fold ~f b accu =
  match b with
    Empty ->
    accu
  | Cons(k, d, rest) ->
    do_bucket_fold ~f rest (f k d accu [@bs]) 

let fold0 f h init =
  let d = h.buckets in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket_fold ~f (Bs_Array.unsafe_get d i) !accu
  done;
  !accu



let rec bucket_length accu = function
  | Empty -> accu
  | Cons(_, _, rest) -> bucket_length (accu + 1) rest

let max (m : int) n = if m > n then m else n  

let logStats0 h =
  let mbl =
    Bs_Array.foldLeft (fun[@bs] m b -> max m (bucket_length 0 b)) 0 h.buckets in
  let histo = Bs_Array.make (mbl + 1) 0 in
  Bs_Array.iter
    (fun[@bs] b ->
       let l = bucket_length 0 b in
       Bs_Array.unsafe_set histo l (Bs_Array.unsafe_get histo l + 1)
    )
    h.buckets;
  Js.log [%obj{ num_bindings = h.size;
                num_buckets = Array.length h.buckets;
                max_bucket_length = mbl;
                bucket_histogram = histo }]





let key_index ~hash (h : ('a, _,_) t0) (key : 'a) =
  ((Bs_Hash.getHash hash) key [@bs]) land (Array.length h.buckets - 1)

let rec insert_bucket_list ~hash ~ndata h = function
    Empty -> ()
  | Cons(key, data, rest) ->
    insert_bucket_list ~hash ~ndata h rest; (* preserve original order of elements *)
    let nidx = key_index ~hash h key in
    Bs_Array.unsafe_set ndata nidx 
      (Cons(key, data, Bs_Array.unsafe_get ndata nidx )) 

let resize ~hash  h =
  let odata = h.buckets in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if  nsize >= osize then begin 
    let ndata = Array.make nsize Empty in
    h.buckets <- ndata;          (* so that indexfun sees the new bucket count *)
    for i = 0 to osize - 1 do
      insert_bucket_list ~hash ~ndata h (Bs_Array.unsafe_get odata i)
    done
  end

let add0 ~hash h key info =
  let i = key_index ~hash h key in
  let h_buckets = h.buckets in  
  let bucket = Cons(key, info, Bs_Array.unsafe_get h_buckets i) in  
  Bs_Array.unsafe_set h_buckets i  bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h_buckets lsl 1 then resize ~hash  h


let rec remove_bucket ~eq key h = function
  | Empty ->
    Empty
  | Cons(k, i, next) ->
    if (Bs_Hash.getEq eq) k key [@bs]
    then begin h.size <- h.size - 1; next end
    else Cons(k, i, remove_bucket ~eq key h next) 

let remove0 ~hash ~eq h key =
  let i = key_index ~hash h key in
  let h_buckets = h.buckets in 
  Bs_Array.unsafe_set h_buckets i
    (remove_bucket ~eq key h (Bs_Array.unsafe_get h_buckets i))


let rec find_rec ~eq key = function
  | Empty ->
    None
  | Cons(k, d, rest) ->
    if (Bs_Hash.getEq eq) key k [@bs] then Some d else find_rec ~eq key  rest

let findOpt0 ~hash ~eq h key =
  match Bs_Array.unsafe_get h.buckets (key_index ~hash h key) with
  | Empty -> None
  | Cons(k1, d1, rest1) ->
    if (Bs_Hash.getEq eq) key k1 [@bs] then Some d1 else
      match rest1 with
      | Empty -> None
      | Cons(k2, d2, rest2) ->
        if (Bs_Hash.getEq eq) key k2 [@bs] then Some d2 else
          match rest2 with
          | Empty -> None
          | Cons(k3, d3, rest3) ->
            if (Bs_Hash.getEq eq) key k3 [@bs] then Some d3 else find_rec ~eq key rest3


let findAll0 ~hash ~eq h key =
  let rec find_in_bucket = function
    | Empty ->
      []
    | Cons(k, d, rest) ->
      if (Bs_Hash.getEq eq) k key [@bs]
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket (Bs_Array.unsafe_get h.buckets (key_index ~hash h key))

let rec replace_bucket ~eq  key info buckets = 
  match buckets with 
  | Empty ->
    raise Not_found
  | Cons(k, i, next) ->
    if (Bs_Hash.getEq eq) k key [@bs]
    then Cons(key, info, next)
    else Cons(k, i, replace_bucket ~eq key info next) 

let replace0 ~hash ~eq  h key info =
  let i = key_index ~hash h key in
  let h_buckets = h.buckets in 
  let l = Array.unsafe_get h_buckets (i) in
  try
    Array.unsafe_set h_buckets (i)  (replace_bucket ~eq  key info l)
  with Not_found ->
    Array.unsafe_set h_buckets (i)  (Cons(key, info, l));
    h.size <- h.size + 1;
    if h.size > Array.length h_buckets lsl 1 then resize ~hash  h

let rec mem_in_bucket ~eq key = function
  | Empty ->
    false
  | Cons(k, d, rest) ->
    (Bs_Hash.getEq eq) k key [@bs] || mem_in_bucket ~eq key rest     
let mem0 ~hash ~eq h key =
  mem_in_bucket ~eq key (Bs_Array.unsafe_get h.buckets (key_index ~hash h key))



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


