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

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)
module C = Bs_internalBucketsType
(* TODO:
   the current implementation relies on the fact that bucket 
   empty value is [undefined] in both places,
   in theory, it can be different 

*)
type 'a bucket = {
  mutable key : 'a;
  mutable next : 'a bucket C.opt
}  
and 'a t0 = 'a bucket C.container  
[@@bs.deriving abstract]

let rec bucket_length accu buckets = 
  match C.toOpt buckets with 
  | None -> accu
  | Some cell -> bucket_length (accu + 1) (next cell)

let max (m : int) n = if m > n then m else n  


let rec do_bucket_iter ~f buckets = 
  match C.toOpt buckets with 
  | None ->
    ()
  | Some cell ->
    f (key cell)  [@bs]; do_bucket_iter ~f (next cell)

let iter0 f h =
  let d = C.buckets h in
  for i = 0 to Bs_Array.length d - 1 do
    do_bucket_iter f (Bs_Array.unsafe_get d i)
  done

let rec fillArray i arr cell =  
  Bs_Array.unsafe_set arr i (key cell);
  match C.toOpt (next cell) with 
  | None -> i + 1
  | Some v -> fillArray (i + 1) arr v 

let toArray0 h = 
  let d = C.buckets h in 
  let current = ref 0 in 
  let arr = Bs.Array.makeUninitializedUnsafe (C.size h) in 
  for i = 0 to Bs_Array.length d - 1 do  
    let cell = Bs_Array.unsafe_get d i in 
    match C.toOpt cell with 
    | None -> ()
    | Some cell -> 
      current := fillArray !current arr cell
  done;
  arr 



let rec do_bucket_fold ~f b accu =
  match C.toOpt b with
  | None ->
    accu
  | Some cell ->
    do_bucket_fold ~f (next cell) (f (key cell) accu [@bs]) 

let fold0 f h init =
  let d = C.buckets h in
  let accu = ref init in
  for i = 0 to Bs_Array.length d - 1 do
    accu := do_bucket_fold ~f (Bs_Array.unsafe_get d i) !accu
  done;
  !accu




let logStats0 h =
  let mbl =
    Bs_Array.foldLeft (fun[@bs] m b -> max m (bucket_length 0 b)) 0 (C.buckets h) in
  let histo = Bs_Array.make (mbl + 1) 0 in
  Bs_Array.iter
    (fun[@bs] b ->
       let l = bucket_length 0 b in
       Bs_Array.unsafe_set histo l (Bs_Array.unsafe_get histo l + 1)
    )
    (C.buckets h);
  Js.log [%obj{ num_bindings = (C.size h);
                num_buckets = Bs_Array.length (C.buckets h);
                max_bucket_length = mbl;
                bucket_histogram = histo }]


