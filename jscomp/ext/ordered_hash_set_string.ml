
  
# 14 "ext/ordered_hash_set.cppo.ml"
  type key = string 
  type t = key Ordered_hash_set_gen.t
  let key_index (h :  t) key = 
    (Bs_hash_stubs.hash_string key) land (Array.length h.data - 1)
  let equal_key = Ext_string.equal

# 23
open Ordered_hash_set_gen

let create = create
let clear = clear
let reset = reset
let copy = copy
let iter = iter
let fold = fold
let length = length
let stats = stats
let elements = elements
let choose = choose
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

let rec small_bucket_find key lst =
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
              small_bucket_find key rest 
let add h key =
  let i = key_index h key  in 
  if not (small_bucket_mem key  h.data.(i)) then 
    begin 
      h.data.(i) <- Cons(key,h.size, h.data.(i));
      h.size <- h.size + 1 ;
      if h.size > Array.length h.data lsl 1 then resize key_index h
    end

let mem h key =
  small_bucket_mem key (Array.unsafe_get h.data (key_index h key)) 
let find h key = 
  small_bucket_find key (Array.unsafe_get h.data (key_index h key))  












