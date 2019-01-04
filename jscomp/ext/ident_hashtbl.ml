# 2 "ext/hashtbl.cppo.ml"
type key = Ident.t 
type 'a t = (key, 'a)  Hashtbl_gen.t 
let key_index (h : _ t ) (key : key) =
  (Bs_hash_stubs.hash_stamp_and_name  key.stamp key.name ) land (Array.length h.data - 1)
(* (Bs_hash_stubs.hash_string_int  key.name key.stamp ) land (Array.length h.data - 1) *)
let eq_key = Ext_ident.equal 

# 33 "ext/hashtbl.cppo.ml"
type ('a, 'b) bucketlist = ('a,'b) Hashtbl_gen.bucketlist
let create = Hashtbl_gen.create
let clear = Hashtbl_gen.clear
let reset = Hashtbl_gen.reset
let copy = Hashtbl_gen.copy
let iter = Hashtbl_gen.iter
let fold = Hashtbl_gen.fold
let length = Hashtbl_gen.length
let stats = Hashtbl_gen.stats



let add (h : _ t) key info =
  let i = key_index h key in
  let h_data = h.data in   
  Array.unsafe_set h_data i (Cons(key, info, (Array.unsafe_get h_data i)));
  h.size <- h.size + 1;
  if h.size > Array.length h_data lsl 1 then Hashtbl_gen.resize key_index h

(* after upgrade to 4.04 we should provide an efficient [replace_or_init] *)
let modify_or_init (h : _ t) key modf default =
  let rec find_bucket (bucketlist : _ bucketlist)  =
    match bucketlist with
    | Cons(k,i,next) ->
      if eq_key k key then begin modf i; false end
      else find_bucket next 
    | Empty -> true in
  let i = key_index h key in 
  let h_data = h.data in 
  if find_bucket (Array.unsafe_get h_data i) then
    begin 
      Array.unsafe_set h_data i  (Cons(key,default (), Array.unsafe_get h_data i));
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hashtbl_gen.resize key_index h 
    end


let rec remove_bucket key (h : _ t) (bucketlist : _ bucketlist) : _ bucketlist = 
  match bucketlist with  
  | Empty ->
    Empty
  | Cons(k, i, next) ->
    if eq_key k key 
    then begin h.size <- h.size - 1; next end
    else Cons(k, i, remove_bucket key h next) 

let remove (h : _ t ) key =
  let i = key_index h key in
  let h_data = h.data in 
  let old_h_szie = h.size in 
  let new_bucket = remove_bucket key h (Array.unsafe_get h_data i) in  
  if old_h_szie <> h.size then 
    Array.unsafe_set h_data i  new_bucket

let rec find_rec key (bucketlist : _ bucketlist) = match bucketlist with  
  | Empty ->
    raise Not_found
  | Cons(k, d, rest) ->
    if eq_key key k then d else find_rec key rest

let find_exn (h : _ t) key =
  match Array.unsafe_get h.data (key_index h key) with
  | Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
    if eq_key key k1 then d1 else
      match rest1 with
      | Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
        if eq_key key k2 then d2 else
          match rest2 with
          | Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
            if eq_key key k3  then d3 else find_rec key rest3

let find_opt (h : _ t) key =
  Hashtbl_gen.small_bucket_opt eq_key key (Array.unsafe_get h.data (key_index h key))

let find_key_opt (h : _ t) key =
  Hashtbl_gen.small_bucket_key_opt eq_key key (Array.unsafe_get h.data (key_index h key))
  
let find_default (h : _ t) key default = 
  Hashtbl_gen.small_bucket_default eq_key key default (Array.unsafe_get h.data (key_index h key))
let find_all (h : _ t) key =
  let rec find_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
    | Empty ->
      []
    | Cons(k, d, rest) ->
      if eq_key k key 
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket (Array.unsafe_get h.data (key_index h key))

let replace h key info =
  let rec replace_bucket (bucketlist : _ bucketlist) : _ bucketlist = match bucketlist with 
    | Empty ->
      raise_notrace Not_found
    | Cons(k, i, next) ->
      if eq_key k key
      then Cons(key, info, next)
      else Cons(k, i, replace_bucket next) in
  let i = key_index h key in
  let h_data = h.data in 
  let l = Array.unsafe_get h_data i in
  try
    Array.unsafe_set h_data i  (replace_bucket l)
  with Not_found ->
    begin 
      Array.unsafe_set h_data i (Cons(key, info, l));
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hashtbl_gen.resize key_index h;
    end 

let mem (h : _ t) key =
  let rec mem_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
    | Empty ->
      false
    | Cons(k, d, rest) ->
      eq_key k key  || mem_in_bucket rest in
  mem_in_bucket (Array.unsafe_get h.data (key_index h key))


let of_list2 ks vs = 
  let len = List.length ks in 
  let map = create len in 
  List.iter2 (fun k v -> add map k v) ks vs ; 
  map

