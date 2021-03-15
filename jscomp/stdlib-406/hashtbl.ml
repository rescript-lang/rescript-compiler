(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Hash tables *)

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]
(* external old_hash_param :
  int -> int -> 'a -> int = "caml_hash_univ_param" [@@noalloc] *)

let hash x = seeded_hash_param 10 100 0 x
let hash_param n1 n2 x = seeded_hash_param n1 n2 0 x
let seeded_hash seed x = seeded_hash_param 10 100 seed x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    mutable seed: int;                        (* for randomization *)
    mutable initial_size: int;                (* initial array size *)
  }

and ('a, 'b) bucketlist =
    Empty
  | Cons of { mutable key: 'a;
              mutable data: 'b;
              mutable next: ('a, 'b) bucketlist }

(* The sign of initial_size encodes the fact that a traversal is
   ongoing or not.

   This disables the efficient in place implementation of resizing.
*)

let ongoing_traversal h =
  h.initial_size < 0

let flip_ongoing_traversal h =
  h.initial_size <- - h.initial_size

(* To pick random seeds if requested *)


#if BS
let randomized_default = false  
#else
let params =    
  try Sys.getenv "OCAMLRUNPARAM" with Not_found ->
  try Sys.getenv "CAMLRUNPARAM" with Not_found -> "" in
String.contains params 'R'
#end

let randomized = ref randomized_default

let randomize () = randomized := true
let is_randomized () = !randomized

let prng = lazy (Random.State.make_self_init())

(* Creating a fresh, empty table *)

let rec power_2_above x n =
  if x >= n then x
#if BS 
  else if x * 2 < x then x (* overflow *)
#else  
  else if x * 2 > Sys.max_array_length then x
#end  
  else power_2_above (x * 2) n

let create ?(random = !randomized) initial_size =
  let s = power_2_above 16 initial_size in
  let seed = if random then Random.State.bits (Lazy.force prng) else 0 in
  { initial_size = s; size = 0; seed = seed; data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    h.data.(i) <- Empty
  done

let reset h =
  let len = Array.length h.data in
  if len = abs h.initial_size then
    clear h
  else begin
    h.size <- 0;
    h.data <- Array.make (abs h.initial_size) Empty
  end

let copy_bucketlist = function
  | Empty -> Empty
  | Cons {key; data; next} ->
      let rec loop prec = function
        | Empty -> ()
        | Cons {key; data; next} ->
            let r = Cons {key; data; next} in
            begin match prec with
            | Empty -> assert false
            | Cons prec ->  prec.next <- r
            end;
            loop r next
      in
      let r = Cons {key; data; next} in
      loop r next;
      r

let copy h = { h with data = Array.map copy_bucketlist h.data }

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if 
#if BS
      nsize >= osize 
#else    
      nsize < Sys.max_array_length
#end    
  then begin 
    let ndata = Array.make nsize Empty in
    let ndata_tail = Array.make nsize Empty in
    let inplace = not (ongoing_traversal h) in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
      | Empty -> ()
      | Cons {key; data; next} as cell ->
          let cell =
            if inplace then cell
            else Cons {key; data; next = Empty}
          in
          let nidx = indexfun h key in
          begin match ndata_tail.(nidx) with
          | Empty -> ndata.(nidx) <- cell;
          | Cons tail -> tail.next <- cell;
          end;
          ndata_tail.(nidx) <- cell;
          insert_bucket next
    in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    if inplace then
      for i = 0 to nsize - 1 do
        match ndata_tail.(i) with
        | Empty -> ()
        | Cons tail -> tail.next <- Empty
      done;
  end

let key_index h key =
  (* compatibility with old hash tables *)
  (seeded_hash_param 10 100 h.seed key) land (Array.length h.data - 1)


let add h key data =
  let i = key_index h key in
  let bucket = Cons{key; data; next=h.data.(i)} in
  h.data.(i) <- bucket;
  h.size <- h.size + 1;
  if h.size > Array.length h.data lsl 1 then resize key_index h

let rec remove_bucket h i key prec = function
  | Empty ->
      ()
  | (Cons {key=k; next}) as c ->
      if compare k key = 0
      then begin
        h.size <- h.size - 1;
        match prec with
        | Empty -> h.data.(i) <- next
        | Cons c -> c.next <- next
      end
      else remove_bucket h i key c next

let remove h key =
  let i = key_index h key in
  remove_bucket h i key Empty h.data.(i)

let rec find_rec key = function
  | Empty ->
      raise Not_found
  | Cons{key=k; data; next} ->
      if compare key k = 0 then data else find_rec key next

let find h key =
  match h.data.(key_index h key) with
  | Empty -> raise Not_found
  | Cons{key=k1; data=d1; next=next1} ->
      if compare key k1 = 0 then d1 else
      match next1 with
      | Empty -> raise Not_found
      | Cons{key=k2; data=d2; next=next2} ->
          if compare key k2 = 0 then d2 else
          match next2 with
          | Empty -> raise Not_found
          | Cons{key=k3; data=d3; next=next3} ->
              if compare key k3 = 0 then d3 else find_rec key next3

let rec find_rec_opt key = function
  | Empty ->
      None
  | Cons{key=k; data; next} ->
      if compare key k = 0 then Some data else find_rec_opt key next

let find_opt h key =
  match h.data.(key_index h key) with
  | Empty -> None
  | Cons{key=k1; data=d1; next=next1} ->
      if compare key k1 = 0 then Some d1 else
      match next1 with
      | Empty -> None
      | Cons{key=k2; data=d2; next=next2} ->
          if compare key k2 = 0 then Some d2 else
          match next2 with
          | Empty -> None
          | Cons{key=k3; data=d3; next=next3} ->
              if compare key k3 = 0 then Some d3 else find_rec_opt key next3

let find_all h key =
  let rec find_in_bucket = function
  | Empty ->
      []
  | Cons{key=k; data; next} ->
      if compare k key = 0
      then data :: find_in_bucket next
      else find_in_bucket next in
  find_in_bucket h.data.(key_index h key)

let rec replace_bucket key data = function
  | Empty ->
      true
  | Cons ({key=k; next} as slot) ->
      if compare k key = 0
      then (slot.key <- key; slot.data <- data; false)
      else replace_bucket key data next

let replace h key data =
  let i = key_index h key in
  let l = h.data.(i) in
  if replace_bucket key data l then begin
    h.data.(i) <- Cons{key; data; next=l};
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1 then resize key_index h
  end

let mem h key =
  let rec mem_in_bucket = function
  | Empty ->
      false
  | Cons{key=k; next} ->
      compare k key = 0 || mem_in_bucket next in
  mem_in_bucket h.data.(key_index h key)

let iter f h =
  let rec do_bucket = function
    | Empty ->
        ()
    | Cons{key; data; next} ->
        f key data; do_bucket next in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    for i = 0 to Array.length d - 1 do
      do_bucket d.(i)
    done;
    if not old_trav then flip_ongoing_traversal h;
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let rec filter_map_inplace_bucket f h i prec = function
  | Empty ->
      begin match prec with
      | Empty -> h.data.(i) <- Empty
      | Cons c -> c.next <- Empty
      end
  | (Cons ({key; data; next} as c)) as slot ->
      begin match f key data with
      | None ->
          h.size <- h.size - 1;
          filter_map_inplace_bucket f h i prec next
      | Some data ->
          begin match prec with
          | Empty -> h.data.(i) <- slot
          | Cons c -> c.next <- slot
          end;
          c.data <- data;
          filter_map_inplace_bucket f h i slot next
      end

let filter_map_inplace f h =
  let d = h.data in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    for i = 0 to Array.length d - 1 do
      filter_map_inplace_bucket f h i Empty h.data.(i)
    done
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons{key; data; next} ->
        do_bucket next (f key data accu) in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    let accu = ref init in
    for i = 0 to Array.length d - 1 do
      accu := do_bucket d.(i) !accu
    done;
    if not old_trav then flip_ongoing_traversal h;
    !accu
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

let rec bucket_length accu = function
  | Empty -> accu
  | Cons{next} -> bucket_length (accu + 1) next

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
    val find_opt: 'a t -> key -> 'a option
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
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
    val find_opt: 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats: 'a t -> statistics
  end

module MakeSeeded(H: SeededHashedType): (SeededS with type key = H.t) =
  struct
    type key = H.t
    type 'a hashtbl = (key, 'a) t
    type 'a t = 'a hashtbl
    let create = create
    let clear = clear
    let reset = reset
    let copy = copy

    let key_index h key =
      (H.hash h.seed key) land (Array.length h.data - 1)

    let add h key data =
      let i = key_index h key in
      let bucket = Cons{key; data; next=h.data.(i)} in
      h.data.(i) <- bucket;
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize key_index h

    let rec remove_bucket h i key prec = function
      | Empty ->
          ()
      | (Cons {key=k; next}) as c ->
          if H.equal k key
          then begin
            h.size <- h.size - 1;
            match prec with
            | Empty -> h.data.(i) <- next
            | Cons c -> c.next <- next
          end
          else remove_bucket h i key c next

    let remove h key =
      let i = key_index h key in
      remove_bucket h i key Empty h.data.(i)

    let rec find_rec key = function
      | Empty ->
          raise Not_found
      | Cons{key=k; data; next} ->
          if H.equal key k then data else find_rec key next

    let find h key =
      match h.data.(key_index h key) with
      | Empty -> raise Not_found
      | Cons{key=k1; data=d1; next=next1} ->
          if H.equal key k1 then d1 else
          match next1 with
          | Empty -> raise Not_found
          | Cons{key=k2; data=d2; next=next2} ->
              if H.equal key k2 then d2 else
              match next2 with
              | Empty -> raise Not_found
              | Cons{key=k3; data=d3; next=next3} ->
                  if H.equal key k3 then d3 else find_rec key next3

    let rec find_rec_opt key = function
      | Empty ->
          None
      | Cons{key=k; data; next} ->
          if H.equal key k then Some data else find_rec_opt key next

    let find_opt h key =
      match h.data.(key_index h key) with
      | Empty -> None
      | Cons{key=k1; data=d1; next=next1} ->
          if H.equal key k1 then Some d1 else
          match next1 with
          | Empty -> None
          | Cons{key=k2; data=d2; next=next2} ->
              if H.equal key k2 then Some d2 else
              match next2 with
              | Empty -> None
              | Cons{key=k3; data=d3; next=next3} ->
                  if H.equal key k3 then Some d3 else find_rec_opt key next3

    let find_all h key =
      let rec find_in_bucket = function
      | Empty ->
          []
      | Cons{key=k; data=d; next} ->
          if H.equal k key
          then d :: find_in_bucket next
          else find_in_bucket next in
      find_in_bucket h.data.(key_index h key)

    let rec replace_bucket key data = function
      | Empty ->
          true
      | Cons ({key=k; next} as slot) ->
          if H.equal k key
          then (slot.key <- key; slot.data <- data; false)
          else replace_bucket key data next

    let replace h key data =
      let i = key_index h key in
      let l = h.data.(i) in
      if replace_bucket key data l then begin
        h.data.(i) <- Cons{key; data; next=l};
        h.size <- h.size + 1;
        if h.size > Array.length h.data lsl 1 then resize key_index h
      end

    let mem h key =
      let rec mem_in_bucket = function
      | Empty ->
          false
      | Cons{key=k; next} ->
          H.equal k key || mem_in_bucket next in
      mem_in_bucket h.data.(key_index h key)

    let iter = iter
    let filter_map_inplace = filter_map_inplace
    let fold = fold
    let length = length
    let stats = stats
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
  end
