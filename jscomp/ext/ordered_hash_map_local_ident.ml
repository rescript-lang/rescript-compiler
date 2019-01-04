  
# 10 "ext/ordered_hash_map.cppo.ml"
  type key = Ident.t
  type   'value t = (key,'value) Ordered_hash_map_gen.t
  let key_index (h : _ t) (key : key) =
    (Bs_hash_stubs.hash_int  key.stamp) land (Array.length h.data - 1)
  let equal_key = Ext_ident.equal


# 20 "ext/ordered_hash_map.cppo.ml"
open Ordered_hash_map_gen

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
  | Cons(key1,_, _, rest) -> 
    equal_key key key1 ||
    match rest with 
    | Empty -> false 
    | Cons(key2 , _,_, rest) -> 
      equal_key key  key2 ||
      match rest with 
      | Empty -> false 
      | Cons(key3,_, _, rest) -> 
        equal_key key  key3 ||
        small_bucket_mem key rest 

let rec small_bucket_rank key lst =
  match lst with 
  | Empty -> -1
  | Cons(key1,i,_, rest) -> 
    if equal_key key key1 then i 
    else match rest with 
      | Empty -> -1 
      | Cons(key2,i2, _, rest) -> 
        if equal_key key  key2 then i2 else
          match rest with 
          | Empty -> -1 
          | Cons(key3,i3, _, rest) -> 
            if equal_key key  key3 then i3 else
              small_bucket_rank key rest 
let rec small_bucket_find_value  key (lst : (_,_) bucket)   =
  match lst with 
  | Empty -> raise Not_found
  | Cons(key1,_,value, rest) -> 
    if equal_key key  key1 then value 
    else match rest with 
      | Empty -> raise Not_found 
      | Cons(key2,_,value, rest) -> 
        if equal_key key  key2 then value else
          match rest with 
          | Empty -> raise Not_found 
          | Cons(key3, _ , value, rest) -> 
            if equal_key key  key3 then value else
              small_bucket_find_value key rest 

let add h key value =
  let i = key_index h key  in 
  if not (small_bucket_mem key  h.data.(i)) then 
    begin 
      h.data.(i) <- Cons(key,h.size, value, h.data.(i));
      h.size <- h.size + 1 ;
      if h.size > Array.length h.data lsl 1 then resize key_index h
    end

let mem h key =
  small_bucket_mem key (Array.unsafe_get h.data (key_index h key)) 
let rank h key = 
  small_bucket_rank key(Array.unsafe_get h.data (key_index h key))  

let find_value h key =
  small_bucket_find_value key (Array.unsafe_get h.data (key_index h key))












