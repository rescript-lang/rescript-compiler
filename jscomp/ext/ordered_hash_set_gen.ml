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

module type S =
sig
  type key
  type t
  val create: int ->  t
  val clear: t -> unit
  val reset: t -> unit
  val copy: t -> t
  val add:  t -> key -> unit
  val mem:  t -> key -> bool
  val rank: t -> key -> int (* -1 if not found*)
  val iter: (key -> int -> unit) ->  t -> unit
  val fold: (key -> int -> 'b -> 'b) ->  t -> 'b -> 'b
  val length:  t -> int
  val stats:  t -> Hashtbl.statistics
  val choose_exn: t -> key 
  val of_array: key array -> t 
  val to_sorted_array: t -> key array
  val replace: t -> key -> key -> unit 
  val reset_to_list : t -> key list -> unit
  exception Replace_failure of bool 
end

exception Replace_failure of bool 


(** when it is true, it means the old key does not exist ,
    when it is false, it means the new key already exist
  *)

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)
type 'a bucket = 
  | Empty 
  | Cons of 'a * int * 'a bucket

type 'a t =
  { mutable size: int; (* number of entries *)
    mutable data: 'a bucket array;  
    mutable data_mask: int ; 
    initial_size: int;
  }
(* Invariant
   [data_mask = Array.length data - 1 ]
   [Array.length data is power of 2]
*)


let create  initial_size =
  let initial_size = Ext_util.power_2_above 16 initial_size in
  { initial_size ; 
    size = 0; 
    data = Array.make initial_size Empty;
    data_mask = initial_size - 1 ;  
  }

let clear h =
  h.size <- 0;
  let h_data = h.data in 
  for i = 0 to h.data_mask  do 
    Array.unsafe_set h_data i  Empty
  done

(** Note this function is only used internally, make sure [h_initial_size] 
    is a power of 16 *)
let reset_with_size h h_initial_size  =
  h.size <- 0;
  h.data <- Array.make h_initial_size Empty;
  h.data_mask <- h_initial_size - 1

let reset h  =
  reset_with_size h h.initial_size


let copy h = { h with data = Array.copy h.data }

let length h = h.size


let rec insert_bucket nmask ndata hash = function
  | Empty -> ()
  | Cons(key,info,rest) ->
    let nidx = hash key land nmask in (* so that indexfun sees the new bucket count *)
    Array.unsafe_set ndata nidx  (Cons(key,info, (Array.unsafe_get ndata nidx)));
    insert_bucket nmask ndata hash rest

let resize hash h =
  let odata = h.data in
  let odata_mask = h.data_mask in 
  let nsize = (odata_mask + 1) * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    h.data <- ndata;          
    let nmask = nsize - 1 in
    h.data_mask <- nmask ; 
    for i = 0 to odata_mask do
      match Array.unsafe_get odata i with 
      | Empty -> ()
      | Cons(key,info,rest) -> 
        let nidx = hash key land nmask in 
        Array.unsafe_set ndata nidx  (Cons(key,info, (Array.unsafe_get ndata nidx)));
        insert_bucket nmask ndata hash rest 
    done
  end


let rec do_bucket f = function
  | Empty ->
    ()
  | Cons(k ,i,  rest) ->
    f k i ; do_bucket f rest 

let iter f h =
  let d = h.data in
  for i = 0 to h.data_mask do
    do_bucket f (Array.unsafe_get d i)
  done

(* find one element *)
let choose_exn h = 
  let rec aux arr offset last_index = 
    if offset > last_index then 
      raise Not_found (* This happens when size is 0, otherwise it is never called *)
    else 
      match Array.unsafe_get arr offset with 
      | Empty -> aux arr (offset + 1) last_index 
      | Cons (k,_,rest) -> k 
  in
  let h_data = h.data in 
  aux h_data 0 h.data_mask

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
      accu
    | Cons( k , i,  rest) ->
      do_bucket rest (f k i  accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to h.data_mask do
    accu := do_bucket (Array.unsafe_get d i) !accu
  done;
  !accu


let rec set_bucket arr = function 
  | Empty -> ()
  | Cons(k,i,rest) ->
    Array.unsafe_set arr i k;
    set_bucket arr rest 

let to_sorted_array h = 
  if h.size = 0 then [||]
  else 
    let v = choose_exn h in 
    let arr = Array.make h.size v in
    let d = h.data in 
    for i = 0 to h.data_mask do 
      set_bucket  arr (Array.unsafe_get d i)
    done;
    arr 




let rec bucket_length acc (x : _ bucket) = 
  match x with 
  | Empty -> acc
  | Cons(_,_,rest) -> bucket_length (acc + 1) rest  

let stats h =
  let mbl =
    Array.fold_left (fun m (b : _ bucket) -> max m (bucket_length 0 b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Ext_array.iter h.data
    (fun b ->
       let l = bucket_length 0 b in
       histo.(l) <- histo.(l) + 1)
    ;
  { Hashtbl.num_bindings = h.size;
    num_buckets = h.data_mask + 1 ;
    max_bucket_length = mbl;
    bucket_histogram = histo }

