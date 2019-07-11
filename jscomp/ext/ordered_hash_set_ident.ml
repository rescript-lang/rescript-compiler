type key = Ident.t
type t = key Ordered_hash_set_gen.t

let hash (key : key) = Bs_hash_stubs.hash_string_int key.name key.stamp
let equal_key = Ext_ident.equal

open Ordered_hash_set_gen

exception Replace_failure = Replace_failure

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
  | Cons (key1, _, rest) -> (
      equal_key key key1
      ||
      match rest with
      | Empty -> false
      | Cons (key2, _, rest) -> (
          equal_key key key2
          ||
          match rest with
          | Empty -> false
          | Cons (key3, _, rest) ->
              equal_key key key3 || small_bucket_mem key rest ) )

let rec small_bucket_rank key lst =
  match lst with
  | Empty -> -1
  | Cons (key1, i, rest) -> (
      if equal_key key key1 then i
      else
        match rest with
        | Empty -> -1
        | Cons (key2, i2, rest) -> (
            if equal_key key key2 then i2
            else
              match rest with
              | Empty -> -1
              | Cons (key3, i3, rest) ->
                  if equal_key key key3 then i3 else small_bucket_rank key rest
            ) )

let add h key =
  let h_data_mask = h.data_mask in
  let i = hash key land h_data_mask in
  if not (small_bucket_mem key h.data.(i)) then (
    Array.unsafe_set h.data i (Cons (key, h.size, Array.unsafe_get h.data i)) ;
    h.size <- h.size + 1 ;
    if h.size > Array.length h.data lsl 1 then resize hash h )

let old_key_not_exist = Replace_failure false
let new_key_already_exist = Replace_failure true

let rec small_bucket_rank_and_delete key lst =
  match lst with
  | Empty -> raise old_key_not_exist
  | Cons (key1, i, rest) -> (
      if equal_key key key1 then (i, rest)
      else
        match rest with
        | Empty -> raise old_key_not_exist
        | Cons (key2, i2, rest) -> (
            if equal_key key key2 then (i2, Cons (key1, i, rest))
            else
              match rest with
              | Empty -> raise old_key_not_exist
              | Cons (key3, i3, rest) ->
                  if equal_key key key3 then
                    (i3, Cons (key1, i, Cons (key2, i2, rest)))
                  else
                    let rank, rest = small_bucket_rank_and_delete key rest in
                    ( rank
                    , Cons (key1, i, Cons (key2, i2, Cons (key3, i3, rest))) )
            ) )

let replace h old_key new_key =
  let h_data_mask = h.data_mask in
  let i = hash old_key land h_data_mask in
  let h_data = h.data in
  let bucket = Array.unsafe_get h_data i in
  let rank, new_bucket = small_bucket_rank_and_delete old_key bucket in
  Array.unsafe_set h_data i new_bucket ;
  let j = hash new_key land h_data_mask in
  let insert_bucket = Array.unsafe_get h_data j in
  let mem = small_bucket_mem new_key insert_bucket in
  if mem then raise new_key_already_exist
  else Array.unsafe_set h_data j (Cons (new_key, rank, insert_bucket))

let of_array arr =
  let len = Array.length arr in
  let h = create len in
  for i = 0 to len - 1 do
    add h (Array.unsafe_get arr i)
  done ;
  h

(* clear the Hashset and re-initialize it to [lst] *)
let reset_to_list h lst =
  let len = List.length lst in
  let () =
    Ordered_hash_set_gen.reset_with_size h (Ext_util.power_2_above 16 len)
  in
  List.iter (fun x -> add h x) lst

let mem h key =
  small_bucket_mem key (Array.unsafe_get h.data (hash key land h.data_mask))

let rank h key =
  small_bucket_rank key (Array.unsafe_get h.data (hash key land h.data_mask))
