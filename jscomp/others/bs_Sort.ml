
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

 

external unsafe_get: 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set: 'a array -> int -> 'a -> unit = "%array_unsafe_set"
external unsafe_blit :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"
external length : 'a array -> int = "%array_length"

let rec isSortedAux a i cmp last_bound = 
  (* when [i = len - 1], it reaches the last element*)
  if i = last_bound then true 
  else 
  if cmp (unsafe_get a i) (unsafe_get a (i+1)) [@bs] < 0 then 
    isSortedAux a (i + 1) cmp last_bound 
  else false 


let isSorted a cmp =
  let len = length a in 
  if len = 0 then true
  else isSortedAux a 0 cmp (len - 1)


let cutoff = 5

let merge src src1ofs src1len src2 src2ofs src2len dst dstofs cmp =
  let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    if cmp s1 s2 [@bs] <= 0 then begin
      unsafe_set dst d s1;
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (unsafe_get src i1) i2 s2 (d + 1)
      else
        unsafe_blit src2 i2 dst (d + 1) (src2r - i2)
    end else begin
      unsafe_set dst d s2;
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (unsafe_get src2 i2) (d + 1)
      else
        unsafe_blit src i1 dst (d + 1) (src1r - i1)
    end
  in 
  loop src1ofs (unsafe_get src src1ofs) src2ofs (unsafe_get src2 src2ofs) dstofs

let insertionSort src srcofs dst dstofs len cmp =
  for i = 0 to len - 1 do
    let e = (unsafe_get src (srcofs + i)) in
    let j = ref (dstofs + i - 1) in
    while (!j >= dstofs && cmp e (unsafe_get dst !j)  [@bs] <= 0) do
      unsafe_set dst (!j + 1) (unsafe_get dst !j);
      decr j;
    done;
    unsafe_set dst (!j + 1) e;
  done    

let rec sortTo src srcofs dst dstofs len cmp =
  if len <= cutoff then insertionSort src srcofs dst dstofs len cmp 
  else begin
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortTo src (srcofs + l1) dst (dstofs + l1) l2 cmp;
    sortTo src srcofs src (srcofs + l2) l1 cmp;
    merge src (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs cmp;
  end    

let stableSortBy  a cmp =
  let l = length a in
  if l <= cutoff then insertionSort a 0 a 0 l cmp 
  else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Bs_Array.makeUninitializedUnsafe l2 in 
    sortTo a l1 t 0 l2 cmp;
    sortTo a 0 a l2 l1 cmp;
    merge a l2 l1 t 0 l2 a 0 cmp;
  end


external sortBy : 
  'a array -> ('a -> 'a -> int [@bs]) -> unit = 
  "sort" [@@bs.send]

let sortByCont xs cmp = 
  sortBy xs cmp ; 
  xs   
