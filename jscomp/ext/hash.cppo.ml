#if defined TYPE_IDENT
type key = Ident.t 
type 'a t = (key, 'a)  Hash_gen.t 
let key_index (h : _ t ) (key : key) =
  (Bs_hash_stubs.hash_stamp_and_name  key.stamp key.name ) land (Array.length h.data - 1)
(* (Bs_hash_stubs.hash_string_int  key.name key.stamp ) land (Array.length h.data - 1) *)
let eq_key = Ext_ident.equal 
#elif defined TYPE_STRING
type key = string
type 'a t = (key, 'a)  Hash_gen.t 
let key_index (h : _ t ) (key : key) =
  (Bs_hash_stubs.hash_string  key ) land (Array.length h.data - 1)
let eq_key = Ext_string.equal 
#elif defined TYPE_INT
type key = int 
type 'a t = (key, 'a)  Hash_gen.t 
let key_index (h : _ t ) (key : key) =
  (Bs_hash_stubs.hash_int  key ) land (Array.length h.data - 1)
let eq_key = Ext_int.equal   

#elif defined TYPE_FUNCTOR
module Make (Key : Hashtbl.HashedType) = struct 
  type key = Key.t 
  type 'a t = (key, 'a)  Hash_gen.t 
  let key_index (h : _ t ) (key : key) =
    (Key.hash  key ) land (Array.length h.data - 1)
  let eq_key = Key.equal   

#else
  [%error "unknown type"]
#endif

type ('a, 'b) bucketlist = ('a,'b) Hash_gen.bucketlist
let create = Hash_gen.create
let clear = Hash_gen.clear
let reset = Hash_gen.reset
let iter = Hash_gen.iter
let to_list = Hash_gen.to_list
let fold = Hash_gen.fold
let length = Hash_gen.length
(* let stats = Hash_gen.stats *)



let add (h : _ t) key info =
  let i = key_index h key in
  let h_data = h.data in   
  Array.unsafe_set h_data i (Cons{key; data=info; rest=Array.unsafe_get h_data i});
  h.size <- h.size + 1;
  if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h

(* after upgrade to 4.04 we should provide an efficient [replace_or_init] *)
let modify_or_init (h : _ t) key modf default =
  let rec find_bucket (bucketlist : _ bucketlist)  =
    match bucketlist with
    | Cons{key=k; data=i; rest=next} ->
      if eq_key k key then begin modf i; false end
      else find_bucket next 
    | Empty -> true in
  let i = key_index h key in 
  let h_data = h.data in 
  if find_bucket (Array.unsafe_get h_data i) then
    begin 
      Array.unsafe_set h_data i  (Cons{key; data=default (); rest=Array.unsafe_get h_data i});
      h.size <- h.size + 1 ;
      if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h 
    end


let rec remove_bucket key (h : _ t) (bucketlist : _ bucketlist) : _ bucketlist = 
  match bucketlist with  
  | Empty ->
    Empty
  | Cons{key=k; data=i; rest=next} ->
    if eq_key k key 
    then begin h.size <- h.size - 1; next end
    else Cons{key = k; data=i; rest=remove_bucket key h next} 

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
  | Cons { key= k; data=d; rest} ->
    if eq_key key k then d else find_rec key rest

let find_exn (h : _ t) key =
  match Array.unsafe_get h.data (key_index h key) with
  | Empty -> raise Not_found
  | Cons {key = k1; data=d1; rest=rest1} ->
    if eq_key key k1 then d1 else
      match rest1 with
      | Empty -> raise Not_found
      | Cons{key=k2; data=d2; rest=rest2} ->
        if eq_key key k2 then d2 else
          match rest2 with
          | Empty -> raise Not_found
          | Cons{key=k3; data=d3; rest=rest3} ->
            if eq_key key k3  then d3 else find_rec key rest3

let find_opt (h : _ t) key =
  Hash_gen.small_bucket_opt eq_key key (Array.unsafe_get h.data (key_index h key))

let find_key_opt (h : _ t) key =
  Hash_gen.small_bucket_key_opt eq_key key (Array.unsafe_get h.data (key_index h key))
  
let find_default (h : _ t) key default = 
  Hash_gen.small_bucket_default eq_key key default (Array.unsafe_get h.data (key_index h key))
let find_all (h : _ t) key =
  let rec find_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
    | Empty ->
      []
    | Cons{key=k; data=d; rest} ->
      if eq_key k key 
      then d :: find_in_bucket rest
      else find_in_bucket rest in
  find_in_bucket (Array.unsafe_get h.data (key_index h key))

let replace h key info =
  let rec replace_bucket (bucketlist : _ bucketlist) : _ bucketlist = match bucketlist with 
    | Empty ->
      raise_notrace Not_found
    | Cons{key = k; data=i; rest = next} ->
      if eq_key k key
      then Cons{key=key; data=info; rest=next}
      else Cons{key=k; data=i; rest = replace_bucket next} in
  let i = key_index h key in
  let h_data = h.data in 
  let l = Array.unsafe_get h_data i in
  try
    Array.unsafe_set h_data i  (replace_bucket l)
  with Not_found ->
    begin 
      Array.unsafe_set h_data i (Cons{key; data=info; rest=l});
      h.size <- h.size + 1;
      if h.size > Array.length h_data lsl 1 then Hash_gen.resize key_index h;
    end 

let mem (h : _ t) key =
  let rec mem_in_bucket (bucketlist : _ bucketlist) = match bucketlist with 
    | Empty ->
      false
    | Cons {key=k; data=d; rest} ->
      eq_key k key  || mem_in_bucket rest in
  mem_in_bucket (Array.unsafe_get h.data (key_index h key))


let of_list2 ks vs = 
  let len = List.length ks in 
  let map = create len in 
  List.iter2 (fun k v -> add map k v) ks vs ; 
  map

#if defined TYPE_FUNCTOR
end
#endif
