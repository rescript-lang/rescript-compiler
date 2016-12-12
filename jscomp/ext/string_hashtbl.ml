type key = string
type ('a, 'b) bucketlist = ('a,'b) Hashtbl_gen.bucketlist
type 'a t = (key, 'a)  Hashtbl_gen.t 
let create = Hashtbl_gen.create
let clear = Hashtbl_gen.clear
let reset = Hashtbl_gen.reset
let copy = Hashtbl_gen.copy
let iter = Hashtbl_gen.iter
let fold = Hashtbl_gen.fold
let length = Hashtbl_gen.length
let stats = Hashtbl_gen.stats


let key_index (h : _ t ) (key : key) =
  (Bs_hash_stubs.hash_string  key ) land (Array.length h.data - 1)

let compare_key (x : key) (y : key) = String.compare x y

let eq_key = Ext_string.equal 

let add (h : _ t) key info =
  let i = key_index h key in
  let bucket : _ bucketlist = Cons(key, info, h.data.(i)) in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then Hashtbl_gen.resize key_index h

let remove (h : _ t ) key =
  let rec remove_bucket (bucketlist : _ bucketlist) : _ bucketlist = match bucketlist with  
    | Empty ->
        Empty
    | Cons(k, i, next) ->
        if compare_key k key = 0
        then begin h.size <- h.size - 1; next end
        else Cons(k, i, remove_bucket next) in
  let i = key_index h key in
  h.data.(i) <- remove_bucket h.data.(i)

let rec find_rec key (bucketlist : _ bucketlist) = match bucketlist with  
  | Empty ->
      raise Not_found
  | Cons(k, d, rest) ->
      if compare_key key k = 0 then d else find_rec key rest

let find (h : _ t) key =
  match h.data.(key_index h key) with
  | Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
      if compare_key key k1 = 0 then d1 else
      match rest1 with
      | Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
          if compare_key key k2 = 0 then d2 else
          match rest2 with
          | Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
              if compare_key key k3 = 0 then d3 else find_rec key rest3

let find_opt (h : _ t) key =
  Hashtbl_gen.small_bucket_opt eq_key key (Array.unsafe_get h.data (key_index h key))
let find_default (h : _ t) key default = 
  Hashtbl_gen.small_bucket_default eq_key key default (Array.unsafe_get h.data (key_index h key))
let find_all (h : _ t) key =
  let rec find_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
  | Empty ->
      []
  | Cons(k, d, rest) ->
      if compare_key k key = 0
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket h.data.(key_index h key)

let replace h key info =
  let rec replace_bucket (bucketlist : _ bucketlist) : _ bucketlist = match bucketlist with 
    | Empty ->
        raise_notrace Not_found
    | Cons(k, i, next) ->
        if compare_key k key = 0
        then Cons(key, info, next)
        else Cons(k, i, replace_bucket next) in
  let i = key_index h key in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket l
  with Not_found ->
    h.data.(i) <- Cons(key, info, l);
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1 then Hashtbl_gen.resize key_index h

let mem (h : _ t) key =
  let rec mem_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
  | Empty ->
      false
  | Cons(k, d, rest) ->
      compare_key k key = 0 || mem_in_bucket rest in
  mem_in_bucket h.data.(key_index h key)


let of_list2 ks vs = 
  let map = create 51 in 
  List.iter2 (fun k v -> add map k v) ks vs ; 
  map
