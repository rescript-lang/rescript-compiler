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


(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type 'a t =
  { mutable size: int;                        (* number of entries *)
    mutable data: 'a list array;  (* the buckets *)
    initial_size: int;                        (* initial array size *)
  }




let create  initial_size =
  let s = Ext_util.power_2_above 16 initial_size in
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

let iter h f =
  let rec do_bucket = function
    | [ ] ->
      ()
    | k ::  rest ->
      f k ; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket (Array.unsafe_get d i)
  done

let fold h init f =
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

let elements set = 
  fold set [] (fun k  acc ->  k :: acc) 




let stats h =
  let mbl =
    Ext_array.fold_left h.data 0 (fun m b -> max m (List.length b)) in
  let histo = Array.make (mbl + 1) 0 in
  Ext_array.iter h.data
    (fun b ->
       let l = List.length b in
       histo.(l) <- histo.(l) + 1)
    ;
  {Hashtbl.num_bindings = h.size;
   num_buckets = Array.length h.data;
   max_bucket_length = mbl;
   bucket_histogram = histo }

let rec small_bucket_mem eq_key key lst =
  match lst with 
  | [] -> false 
  | key1::rest -> 
    eq_key key   key1 ||
    match rest with 
    | [] -> false 
    | key2 :: rest -> 
      eq_key key   key2 ||
      match rest with 
      | [] -> false 
      | key3 :: rest -> 
        eq_key key   key3 ||
        small_bucket_mem eq_key key rest 

let rec remove_bucket eq_key key (h : _ t) buckets = 
  match buckets with 
  | [ ] ->
    [ ]
  | k :: next ->
    if  eq_key k   key
    then begin h.size <- h.size - 1; next end
    else k :: remove_bucket eq_key key h next    

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
  val of_array : key array -> t 
  val check_add : t -> key -> bool
  val mem : t -> key -> bool
  val iter: t -> (key -> unit) -> unit
  val fold: t -> 'b  -> (key -> 'b -> 'b) -> 'b
  val length:  t -> int
  val stats:  t -> Hashtbl.statistics
  val elements : t -> key list 
end
