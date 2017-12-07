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
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    h.data.(i) <- Empty
  done

let reset h =
  let len = Array.length h.data in
  if Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
    || len = h.initial_size then
    clear h
  else begin
    h.size <- 0;
    h.data <- Array.make h.initial_size Empty
  end

let copy h = { h with data = Array.copy h.data }

let length h = h.size

let resize ~hash indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if  nsize >= osize then begin 
    let ndata = Array.make nsize Empty in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = indexfun ~hash h key in
          ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done
  end




let iter f h =
  let rec do_bucket = function
    | Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons(k, d, rest) ->
        do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
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

(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type SeededHashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: int -> t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length: 'a t -> int
    val stats: 'a t -> statistics
  end

module type SeededS =
  sig
    type key
    type 'a t
    val create : ?random:bool -> int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find : 'a t -> key -> 'a
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats: 'a t -> statistics
  end

    
    
    type ('a,'b,'id) t = {
      hash : ('a, 'id) Bs_Hash.t;
      table : ('a,'b,'id) t0;
      
    }


    let key_index ~hash h key =
      ((Bs_Hash.getHash hash) key [@bs]) land (Array.length h.data - 1)

    let add ~hash h key info =
      let i = key_index ~hash h key in
      let bucket = Cons(key, info, h.data.(i)) in
      h.data.(i) <- bucket;
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize ~hash key_index h

    let remove ~hash ~eq h key =
      let rec remove_bucket = function
        | Empty ->
            Empty
        | Cons(k, i, next) ->
            if (Bs_Hash.getEq eq) k key [@bs]
            then begin h.size <- h.size - 1; next end
            else Cons(k, i, remove_bucket next) in
      let i = key_index ~hash h key in
      h.data.(i) <- remove_bucket h.data.(i)

    let rec find_rec ~eq key = function
      | Empty ->
          raise Not_found
      | Cons(k, d, rest) ->
          if (Bs_Hash.getEq eq) key k [@bs] then d else find_rec ~eq key  rest

    let find ~hash ~eq h key =
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

    let find_all ~hash ~eq h key =
      let rec find_in_bucket = function
      | Empty ->
          []
      | Cons(k, d, rest) ->
          if (Bs_Hash.getEq eq) k key [@bs]
          then d :: find_in_bucket rest
          else find_in_bucket rest in
      find_in_bucket h.data.(key_index ~hash h key)

    let replace ~eq ~hash h key info =
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
        if h.size > Array.length h.data lsl 1 then resize ~hash key_index h

    let mem ~hash ~eq h key =
      let rec mem_in_bucket = function
      | Empty ->
          false
      | Cons(k, d, rest) ->
          (Bs_Hash.getEq eq) k key [@bs] || mem_in_bucket rest in
      mem_in_bucket h.data.(key_index ~hash h key)

