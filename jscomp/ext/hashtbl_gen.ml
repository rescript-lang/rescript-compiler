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



module type S = sig 
  type key
  type 'a t
  val create: int -> 'a t
  val clear: 'a t -> unit
  val reset: 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val modify_or_init: 'a t -> key -> ('a -> unit) -> (unit -> 'a) -> unit 
  val remove: 'a t -> key -> unit
  val find_exn: 'a t -> key -> 'a
  val find_all: 'a t -> key -> 'a list
  val find_opt: 'a t -> key  -> 'a option
  
  (** return the key found in the hashtbl.
    Use case: when you find the key existed in hashtbl, 
    you want to use the one stored in the hashtbl. 
    (they are semantically equivlanent, but may have other information different) 
   *)
  val find_key_opt: 'a t -> key -> key option 

  val find_default: 'a t -> key -> 'a -> 'a 

  val replace: 'a t -> key -> 'a -> unit
  val mem: 'a t -> key -> bool
  val iter: 'a t -> (key -> 'a -> unit) -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length: 'a t -> int
  val stats: 'a t -> Hashtbl.statistics
  val of_list2: key list -> 'a list -> 'a t
end

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of entries *)
    mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
    mutable seed: int;                        (* for randomization *)
    initial_size: int;                        (* initial array size *)
  }

and ('a, 'b) bucketlist =
  | Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist


let create  initial_size =
  let s = Ext_util.power_2_above 16 initial_size in
  { initial_size = s; size = 0; seed = 0; data = Array.make s Empty }

let clear h =
  h.size <- 0;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    h.data.(i) <- Empty
  done

let reset h =
  h.size <- 0;
  h.data <- Array.make h.initial_size Empty


let copy h = { h with data = Array.copy h.data }

let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
        Empty -> ()
      | Cons(key, data, rest) ->
        insert_bucket rest; (* preserve original order of elements *)
        let nidx = indexfun h key in
        ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done
  end



let iter h f =
  let rec do_bucket = function
    | Empty ->
      ()
    | Cons(k, d, rest) ->
      f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
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

let rec bucket_length accu = function
  | Empty -> accu
  | Cons(_, _, rest) -> bucket_length (accu + 1) rest

let stats h =
  let mbl =
    Ext_array.fold_left h.data 0 (fun m b -> max m (bucket_length 0 b)) in
  let histo = Array.make (mbl + 1) 0 in
  Ext_array.iter h.data
    (fun b ->
       let l = bucket_length 0 b in
       histo.(l) <- histo.(l) + 1)
    ;
  {Hashtbl.
    num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo }



let rec small_bucket_mem eq key (lst : _ bucketlist) =
  match lst with 
  | Empty -> false 
  | Cons(k1,_,rest1) -> 
    eq  key k1 ||
    match rest1 with
    | Empty -> false 
    | Cons(k2,_,rest2) -> 
      eq key k2  || 
      match rest2 with 
      | Empty -> false 
      | Cons(k3,_,rest3) -> 
        eq key k3  ||
        small_bucket_mem eq key rest3 


let rec small_bucket_opt eq key (lst : _ bucketlist) : _ option =
  match lst with 
  | Empty -> None 
  | Cons(k1,d1,rest1) -> 
    if eq  key k1 then Some d1 else 
      match rest1 with
      | Empty -> None 
      | Cons(k2,d2,rest2) -> 
        if eq key k2 then Some d2 else 
          match rest2 with 
          | Empty -> None 
          | Cons(k3,d3,rest3) -> 
            if eq key k3  then Some d3 else 
              small_bucket_opt eq key rest3 


let rec small_bucket_key_opt eq key (lst : _ bucketlist) : _ option =
  match lst with 
  | Empty -> None 
  | Cons(k1,d1,rest1) -> 
    if eq  key k1 then Some k1 else 
      match rest1 with
      | Empty -> None 
      | Cons(k2,d2,rest2) -> 
        if eq key k2 then Some k2 else 
          match rest2 with 
          | Empty -> None 
          | Cons(k3,d3,rest3) -> 
            if eq key k3  then Some k3 else 
              small_bucket_key_opt eq key rest3


let rec small_bucket_default eq key default (lst : _ bucketlist) =
  match lst with 
  | Empty -> default 
  | Cons(k1,d1,rest1) -> 
    if eq  key k1 then  d1 else 
      match rest1 with
      | Empty -> default 
      | Cons(k2,d2,rest2) -> 
        if eq key k2 then  d2 else 
          match rest2 with 
          | Empty -> default 
          | Cons(k3,d3,rest3) -> 
            if eq key k3  then  d3 else 
              small_bucket_default eq key default rest3 
