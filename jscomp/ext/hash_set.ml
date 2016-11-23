(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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




external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" "noalloc"


(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type 'a t =
  { mutable size: int;                        (* number of entries *)
    mutable data: 'a list array;  (* the buckets *)
    initial_size: int;                        (* initial array size *)
  }



let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create  initial_size =
  let s = power_2_above 16 initial_size in
  { initial_size = s; size = 0; data = Array.make s [] }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    Array.unsafe_set h.data i  []
  done

let reset h =
  h.size <- 0;
  h.data <- Array.make h.initial_size [ ]


let copy h = { h with data = Array.copy h.data }

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize [ ] in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        [ ] -> ()
      | key :: rest ->
          let nidx = indexfun h key in
          ndata.(nidx) <- key :: ndata.(nidx);
          insert_bucket rest
    in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done
  end

let key_index h key =
  (seeded_hash_param 10 100 0 key) land (Array.length h.data - 1)


let remove h key =
  let rec remove_bucket = function
    | [ ] ->
        [ ]
    | k :: next ->
        if compare k key = 0
        then begin h.size <- h.size - 1; next end
        else k :: remove_bucket next in
  let i = key_index h key in
  h.data.(i) <- remove_bucket h.data.(i)

let small_bucket_mem key lst =
  match lst with 
  | [] -> false 
  | key1::rest -> 
    key = key1 ||
    match rest with 
    | [] -> false 
    | key2 :: rest -> 
      key = key2 ||
      match rest with 
      | [] -> false 
      | key3 :: rest -> 
         key = key3 ||
         List.mem key rest 
let add h key =
  let i = key_index h key  in 
  if not (small_bucket_mem key  h.data.(i)) then 
    begin 
      h.data.(i) <- key :: h.data.(i);
      h.size <- h.size + 1 ;
      if h.size > Array.length h.data lsl 1 then resize key_index h
    end
let mem h key =
  small_bucket_mem key h.data.(key_index h key) 

let iter f h =
  let rec do_bucket = function
    | [ ] ->
        ()
    | k ::  rest ->
        f k ; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      [ ] ->
        accu
    | k ::  rest ->
        do_bucket rest (f k  accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket (Array.unsafe_get d i) !accu
   done;
  !accu

let elements set = 
  fold  (fun k  acc ->  k :: acc) set []

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}



let stats h =
  let mbl =
    Array.fold_left (fun m b -> max m (List.length b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
      let l = List.length b in
      histo.(l) <- histo.(l) + 1)
    h.data;
  { num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo }


module type S =
  sig
    type key
    type t
    val create: int ->  t
    val clear : t -> unit
    val reset : t -> unit
    val copy: t -> t
    val remove:  t -> key -> unit
    val add :  t -> key -> unit
    val mem :  t -> key -> bool
    val iter: (key -> unit) ->  t -> unit
    val fold: (key -> 'b -> 'b) ->  t -> 'b -> 'b
    val length:  t -> int
    val stats:  t -> statistics
    val elements : t -> key list 
  end

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    type key = H.t
    
    type nonrec  t = key t
    let create = create
    let clear = clear
    let reset = reset
    let copy = copy

    let key_index h key =
      (H.hash  key) land (Array.length h.data - 1)

    let remove h key =
      let rec remove_bucket = function
        | [ ] ->
            [ ]
        | k :: next ->
            if H.equal k key
            then begin h.size <- h.size - 1; next end
            else k :: remove_bucket next in
      let i = key_index h key in
      h.data.(i) <- remove_bucket h.data.(i)

    let small_bucket_mem key lst =
      match lst with 
      | [] -> false 
      | key1::rest -> 
        H.equal key key1 ||
        match rest with 
        | [] -> false 
        | key2 :: rest -> 
          H.equal key  key2 ||
          match rest with 
          | [] -> false 
          | key3 :: rest -> 
            H.equal key  key3 ||
            List.exists (fun x -> H.equal key x) rest 

    let add h key =
      let i = key_index h key  in 
      if not (small_bucket_mem key  h.data.(i)) then 
        begin 
          h.data.(i) <- key :: h.data.(i);
          h.size <- h.size + 1 ;
          if h.size > Array.length h.data lsl 1 then resize key_index h
        end

    let mem h key =
      small_bucket_mem key h.data.(key_index h key) 

    let iter = iter
    let fold = fold
    let length = length
    let stats = stats
    let elements = elements
  end










