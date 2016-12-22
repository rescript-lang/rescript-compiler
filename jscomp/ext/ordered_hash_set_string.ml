  
# 11 "ext/ordered_hash_set.cppo.ml"
  type key = string 
  type t = key Ordered_hash_set_gen.t
  let hash = Bs_hash_stubs.hash_string
  let equal_key = Ext_string.equal

# 19
open Ordered_hash_set_gen

let create = create
let clear = clear
let reset = reset
let copy = copy
let iter = iter
let fold = fold
let length = length
let stats = stats
let choose_exn = choose_exn
let to_sorted_array = to_sorted_array



let rec small_bucket_mem key lst =
  match lst with 
  | Empty -> false 
  | Cons(key1,_, rest) -> 
    equal_key key key1 ||
    match rest with 
    | Empty -> false 
    | Cons(key2 , _, rest) -> 
      equal_key key  key2 ||
      match rest with 
      | Empty -> false 
      | Cons(key3,_,  rest) -> 
        equal_key key  key3 ||
        small_bucket_mem key rest 

let rec small_bucket_rank key lst =
  match lst with 
  | Empty -> -1
  | Cons(key1,i,rest) -> 
    if equal_key key key1 then i 
    else match rest with 
      | Empty -> -1 
      | Cons(key2,i2,  rest) -> 
        if equal_key key  key2 then i2 else
          match rest with 
          | Empty -> -1 
          | Cons(key3,i3, rest) -> 
            if equal_key key  key3 then i3 else
              small_bucket_rank key rest 
let add h key =
  let h_data_mask = h.data_mask in 
  let i = hash key land h_data_mask in 
  if not (small_bucket_mem key  h.data.(i)) then 
    begin 
      Array.unsafe_set h.data i (Cons(key,h.size, Array.unsafe_get h.data i));
      h.size <- h.size + 1 ;
      if h.size > Array.length h.data lsl 1 then resize hash h
    end

let of_array arr =
  let len = Array.length arr in 
  let h = create len in 
  for i = 0 to len - 1 do 
    add h (Array.unsafe_get arr i)
  done;
  h


let mem h key =
  small_bucket_mem key (Array.unsafe_get h.data (hash  key land h.data_mask)) 
let rank h key = 
  small_bucket_rank key (Array.unsafe_get h.data (hash  key land h.data_mask))  












