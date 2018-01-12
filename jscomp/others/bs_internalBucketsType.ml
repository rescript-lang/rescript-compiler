(* Copyright (C) 2017 Authors of BuckleScript
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
type 'a opt = 'a Js.undefined

type 'c container =
  { mutable size: int;                        (* number of entries *)
    mutable buckets: 'c opt array;  (* the buckets *)
    initialSize: int;                        (* initial array size *)
  } [@@bs.deriving abstract]
module A = Bs_Array

external toOpt : 'a opt -> 'a option = "#undefined_to_opt"
external return : 'a -> 'a opt = "%identity" 
let emptyOpt = Js.undefined   
let rec power_2_above x n =
  if x >= n then x
  else if x * 2 < x then x (* overflow *)
  else power_2_above (x * 2) n

let create0  initialSize =
  let s = power_2_above 16 initialSize in  
  container  ~initialSize:s ~size:0
    ~buckets:(A.makeUninitialized s)

let clear0 h =
  sizeSet h 0;
  let h_buckets = buckets h in 
  let len = A.length h_buckets in
  for i = 0 to len - 1 do
    A.unsafe_set h_buckets i  emptyOpt
  done

let reset0 h =
  let len = A.length (buckets h) in
  let h_initialSize = initialSize h in
  if len = h_initialSize then
    clear0 h
  else begin
    sizeSet h 0;
    bucketsSet h (A.makeUninitialized h_initialSize)
  end

let length0 h = size h
  



type statistics = {
  num_bindings: int;
  (** Number of bindings present in the table.
      Same value as returned by {!Hashtbl.length}. *)
  num_buckets: int;
  (** Number of buckets in the table. *)
  max_bucket_length: int;
  (** Maximal number of bindings per bucket. *)
  bucket_histogram: int array
  (** Histogram of bucket sizes.  This array [histo] has
      length [max_bucket_length + 1].  The value of
      [histo.(i)] is the number of buckets whose size is [i]. *)
} 