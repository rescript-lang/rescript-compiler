
(* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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


module Int = Belt_SortArrayInt

module String = Belt_SortArrayString  

module A = Belt_Array 

let rec sortedLengthAuxMore xs prec acc len lt = 
  if acc >= len then acc 
  else 
    let v = A.getUnsafe xs acc in 
    if lt v prec [@bs]  then 
      sortedLengthAuxMore xs v (acc + 1) len lt
    else acc   

let rec sortedLengthAuxLess xs prec acc len lt = 
  if acc >= len then acc 
  else 
    let v = A.getUnsafe xs acc in 
    if lt prec v [@bs]  then 
      sortedLengthAuxLess xs v (acc + 1) len lt
    else acc   
    
let strictlySortedLengthU xs lt = 
  let len = A.length xs in 
  match len with 
  | 0 | 1 -> len 
  | _ -> 
    let x0, x1 = A.getUnsafe xs 0, A.getUnsafe xs 1 in 
    (* let c = cmp x0 x1 [@bs]  in *)
    if lt x0 x1 [@bs] then 
      sortedLengthAuxLess xs x1 2 len lt
    else if lt x1 x0 [@bs] then 
      - (sortedLengthAuxMore xs x1 2 len lt)
    else 1  

let strictlySortedLength xs lt =
  strictlySortedLengthU xs (fun[@bs] x y -> lt x y)
    
let rec isSortedAux a i cmp last_bound = 
  (* when `i = len - 1`, it reaches the last element*)
  if i = last_bound then true 
  else 
  if cmp (A.getUnsafe a i) (A.getUnsafe a (i+1)) [@bs] <= 0 then 
    isSortedAux a (i + 1) cmp last_bound 
  else false 


let isSortedU a cmp =
  let len = A.length a in 
  if len = 0 then true
  else isSortedAux a 0 cmp (len - 1)

let isSorted a cmp = isSortedU a (fun[@bs] x y -> cmp x y)
    
let cutoff = 5

let merge src src1ofs src1len src2 src2ofs src2len dst dstofs cmp =
  let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    if cmp s1 s2 [@bs] <= 0 then begin
      A.setUnsafe dst d s1;
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (A.getUnsafe src i1) i2 s2 (d + 1)
      else
        A.blitUnsafe ~src:src2 ~srcOffset:i2 ~dst ~dstOffset:(d + 1) ~len:(src2r - i2)
    end else begin
      A.setUnsafe dst d s2;
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (A.getUnsafe src2 i2) (d + 1)
      else
        A.blitUnsafe ~src ~srcOffset:i1 ~dst ~dstOffset:(d + 1) ~len:(src1r - i1)
    end
  in 
  loop src1ofs (A.getUnsafe src src1ofs) src2ofs (A.getUnsafe src2 src2ofs) dstofs

let unionU src src1ofs src1len src2 src2ofs src2len dst dstofs cmp =
  let src1r = src1ofs + src1len in 
  let src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    let c = cmp s1 s2 [@bs] in 
    if c < 0 then begin
      (* `s1` is larger than all elements in `d` *)
      A.setUnsafe dst d s1; 
      let i1 = i1 + 1 in
      let d = d + 1 in 
      if i1 < src1r then
        loop i1 (A.getUnsafe src i1) i2 s2 d
      else
        begin 
          A.blitUnsafe ~src:src2 ~srcOffset:i2 ~dst ~dstOffset:d ~len:(src2r - i2);
          d + src2r - i2   
        end
    end 
    else if c = 0 then begin 
      A.setUnsafe dst d s1;
      let i1 = i1 + 1 in
      let i2 = i2 + 1 in 
      let d  = d + 1 in 
      if i1 < src1r && i2 < src2r then 
        loop i1 (A.getUnsafe src i1) i2 (A.getUnsafe src2 i2) d
      else if i1 = src1r then   
        (A.blitUnsafe ~src:src2 ~srcOffset:i2 ~dst ~dstOffset:d ~len:(src2r - i2);
         d + src2r - i2)
      else    
        (A.blitUnsafe ~src ~srcOffset:i1 ~dst ~dstOffset:d ~len:(src1r - i1);
         d + src1r - i1)
    end 
    else begin
      A.setUnsafe dst d s2;
      let i2 = i2 + 1 in
      let d = d + 1 in 
      if i2 < src2r then
        loop i1 s1 i2 (A.getUnsafe src2 i2) d
      else
        (A.blitUnsafe ~src ~srcOffset:i1 ~dst ~dstOffset:d ~len:(src1r - i1);
         d + src1r - i1
        )
    end
  in 
  loop src1ofs 
    (A.getUnsafe src src1ofs) 
    src2ofs 
    (A.getUnsafe src2 src2ofs) dstofs

let union src src1ofs src1len src2 src2ofs src2len dst dstofs cmp =
  unionU src src1ofs src1len src2 src2ofs src2len dst dstofs
    (fun [@bs] x y -> cmp x y)
    
let intersectU src src1ofs src1len src2 src2ofs src2len dst dstofs cmp =
  let src1r = src1ofs + src1len in 
  let src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    let c = cmp s1 s2 [@bs] in 
    if c < 0 then begin
      (* A.setUnsafe dst d s1; *)
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (A.getUnsafe src i1) i2 s2 d
      else
        d
    end 
    else if c = 0 then begin 
      A.setUnsafe dst d s1;
      let i1 = i1 + 1 in
      let i2 = i2 + 1 in 
      let d = d + 1 in 
      if i1 < src1r && i2 < src2r then 
        loop i1 (A.getUnsafe src i1) i2 (A.getUnsafe src2 i2) d
      else d
    end 
    else begin
      (* A.setUnsafe dst d s2; *)
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (A.getUnsafe src2 i2) d
      else
        d
    end
  in 
  loop src1ofs 
    (A.getUnsafe src src1ofs) 
    src2ofs 
    (A.getUnsafe src2 src2ofs) dstofs    

let intersect src src1ofs src1len src2 src2ofs src2len dst dstofs cmp =
  intersectU src src1ofs src1len src2 src2ofs src2len dst dstofs
    (fun [@bs] x y -> cmp x y)
    
let diffU src src1ofs src1len src2 src2ofs src2len dst dstofs cmp =
  let src1r = src1ofs + src1len in 
  let src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    let c = cmp s1 s2 [@bs] in 
    if c < 0 then begin
      A.setUnsafe dst d s1;
      let d = d + 1 in 
      let i1 = i1 + 1 in      
      if i1 < src1r then
        loop i1 (A.getUnsafe src i1) i2 s2 d
      else
        d
    end 
    else if c = 0 then begin 
      let i1 = i1 + 1 in
      let i2 = i2 + 1 in 
      if i1 < src1r && i2 < src2r then 
        loop i1 (A.getUnsafe src i1) i2 (A.getUnsafe src2 i2) d
      else if i1 = src1r then 
        d
      else 
      (A.blitUnsafe ~src ~srcOffset:i1 ~dst ~dstOffset:d ~len:(src1r - i1);
        d + src1r - i1)
    end 
    else begin
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (A.getUnsafe src2 i2) d
      else
        (A.blitUnsafe ~src ~srcOffset:i1 ~dst ~dstOffset:d ~len:(src1r - i1);
        d + src1r - i1)        
    end
  in 
  loop src1ofs 
    (A.getUnsafe src src1ofs) 
    src2ofs 
    (A.getUnsafe src2 src2ofs) dstofs        

let diff src src1ofs src1len src2 src2ofs src2len dst dstofs cmp =
  diffU src src1ofs src1len src2 src2ofs src2len dst dstofs
    (fun [@bs] x y -> cmp x y)
    
(* `<=` alone is not enough for stable sort *)
let insertionSort src srcofs dst dstofs len cmp =
  for i = 0 to len - 1 do
    let e = (A.getUnsafe src (srcofs + i)) in
    let j = ref (dstofs + i - 1) in
    while j.contents >= dstofs && cmp (A.getUnsafe dst j.contents) e [@bs] > 0 do
      A.setUnsafe dst (j.contents + 1) (A.getUnsafe dst j.contents);
      j.contents <- j.contents - 1;
    done;
    A.setUnsafe dst (j.contents + 1) e;
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




let stableSortInPlaceByU  a cmp =
  let l = A.length a in
  if l <= cutoff then insertionSort a 0 a 0 l cmp 
  else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Belt_Array.makeUninitializedUnsafe l2 in 
    sortTo a l1 t 0 l2 cmp;
    sortTo a 0 a l2 l1 cmp;
    merge a l2 l1 t 0 l2 a 0 cmp;
  end

let stableSortInPlaceBy  a cmp =
    stableSortInPlaceByU a (fun[@bs] x y -> cmp x y)
      
let stableSortByU a cmp =
  let b = A.copy a in stableSortInPlaceByU b cmp; b

let stableSortBy a cmp = stableSortByU a (fun [@bs] x y -> cmp x y) 
(* 
  `binarySearchAux arr lo hi key cmp`
  range [lo, hi]
  input (lo <= hi)
  `arr[lo] <= key <= arr[hi]` *)
let rec binarySearchAux arr lo hi key cmp = 

  let mid = (lo + hi)/2 in 
  let midVal = A.getUnsafe arr mid in 
  let c = cmp key midVal [@bs] in 
  if c = 0 then mid 
  else if c < 0 then  (*  a[lo] =< key < a[mid] <= a[hi] *)
    if hi = mid then  
      if cmp (A.getUnsafe arr lo) key [@bs] = 0 then lo
      else - (hi + 1)
    else binarySearchAux arr lo mid key cmp 
  else  (*  a[lo] =< a[mid] < key <= a[hi] *)
  if lo = mid then 
    if cmp (A.getUnsafe arr hi) key [@bs] = 0 then hi
    else - (hi + 1)
  else binarySearchAux arr mid hi key cmp 

let binarySearchByU sorted key cmp : int =  
  let len = A.length sorted in 
  if len = 0 then -1 
  else 
    let lo = A.getUnsafe sorted 0 in 
    let c = cmp key lo [@bs] in 
    if c < 0 then -1 
    else
      let hi = A.getUnsafe sorted (len - 1) in 
      let c2 = cmp key hi [@bs]in 
      if c2 > 0 then - (len + 1)
      else binarySearchAux sorted 0 (len - 1) key cmp 


let binarySearchBy sorted key cmp =
  binarySearchByU sorted key (fun [@bs] x y -> cmp x y)
