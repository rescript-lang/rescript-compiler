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

(* does not support [remove], so that the adding order is strict and continous *)

module type S = sig
  type key
  type 'value t

  val create : int -> 'value t
  val clear : 'vaulue t -> unit
  val reset : 'value t -> unit
  val copy : 'value t -> 'value t
  val add : 'value t -> key -> 'value -> unit
  val mem : 'value t -> key -> bool
  val rank : 'value t -> key -> int (* -1 if not found*)

  val find_value : 'value t -> key -> 'value (* raise if not found*)

  val iter : (key -> 'value -> int -> unit) -> 'value t -> unit
  val fold : (key -> 'value -> int -> 'b -> 'b) -> 'value t -> 'b -> 'b
  val length : 'value t -> int
  val stats : 'value t -> Hashtbl.statistics
  val elements : 'value t -> key list
  val choose : 'value t -> key
  val to_sorted_array : 'value t -> key array
end

(* We do dynamic hashing, and resize the table and rehash the elements when
   buckets become too long. *)
type ('a, 'b) bucket = Empty | Cons of 'a * int * 'b * ('a, 'b) bucket

type ('a, 'b) t =
  { mutable size: int
  ; (* number of entries *)
    mutable data: ('a, 'b) bucket array
  ; (* the buckets *)
    initial_size: int (* initial array size *) }

let create initial_size =
  let s = Ext_util.power_2_above 16 initial_size in
  {initial_size= s; size= 0; data= Array.make s Empty}

let clear h =
  h.size <- 0 ;
  let len = Array.length h.data in
  for i = 0 to len - 1 do
    Array.unsafe_set h.data i Empty
  done

let reset h =
  h.size <- 0 ;
  h.data <- Array.make h.initial_size Empty

let copy h = {h with data= Array.copy h.data}
let length h = h.size

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then (
    let ndata = Array.make nsize Empty in
    h.data <- ndata ;
    (* so that indexfun sees the new bucket count *)
    let rec insert_bucket = function
      | Empty -> ()
      | Cons (key, info, data, rest) ->
          let nidx = indexfun h key in
          ndata.(nidx) <- Cons (key, info, data, ndata.(nidx)) ;
          insert_bucket rest in
    for i = 0 to osize - 1 do
      insert_bucket (Array.unsafe_get odata i)
    done )

let iter f h =
  let rec do_bucket = function
    | Empty -> ()
    | Cons (k, i, value, rest) -> f k value i ; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
  done

let choose h =
  let rec aux arr offset len =
    if offset >= len then raise Not_found
    else
      match Array.unsafe_get arr offset with
      | Empty -> aux arr (offset + 1) len
      | Cons (k, _, _, rest) -> k in
  aux h.data 0 (Array.length h.data)

let to_sorted_array h =
  if h.size = 0 then [||]
  else
    let v = choose h in
    let arr = Array.make h.size v in
    iter (fun k _ i -> Array.unsafe_set arr i k) h ;
    arr

let fold f h init =
  let rec do_bucket b accu =
    match b with
    | Empty -> accu
    | Cons (k, i, value, rest) -> do_bucket rest (f k value i accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket (Array.unsafe_get d i) !accu
  done ;
  !accu

let elements set = fold (fun k _ _ acc -> k :: acc) set []

let rec bucket_length acc (x : _ bucket) =
  match x with
  | Empty -> 0
  | Cons (_, _, _, rest) -> bucket_length (acc + 1) rest

let stats h =
  let mbl =
    Ext_array.fold_left h.data 0 (fun m b -> max m (bucket_length 0 b)) in
  let histo = Array.make (mbl + 1) 0 in
  Ext_array.iter h.data (fun b ->
      let l = bucket_length 0 b in
      histo.(l) <- histo.(l) + 1) ;
  { Hashtbl.num_bindings= h.size
  ; num_buckets= Array.length h.data
  ; max_bucket_length= mbl
  ; bucket_histogram= histo }
