# 4 "sort.cppo.ml"
type elt = string

# 9
module A = Bs_Array 


let rec isSortedAux (a : elt array) i  last_bound = 
  (* when [i = len - 1], it reaches the last element*)
  if i = last_bound then true 
  else 
  if (A.unsafe_get a i) <= (A.unsafe_get a (i+1))  then 
    isSortedAux a (i + 1)  last_bound 
  else false 


let isSorted a =
  let len = A.length a in 
  if len = 0 then true
  else isSortedAux a 0  (len - 1)


let cutoff = 5

let merge (src : elt array) src1ofs src1len src2 src2ofs src2len dst dstofs  =
  let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    if  s1 <= s2  then begin
      A.unsafe_set dst d s1;
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (A.unsafe_get src i1) i2 s2 (d + 1)
      else
        A.blitUnsafe src2 i2 dst (d + 1) (src2r - i2)
    end else begin
      A.unsafe_set dst d s2;
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (A.unsafe_get src2 i2) (d + 1)
      else
        A.blitUnsafe src i1 dst (d + 1) (src1r - i1)
    end
  in 
  loop src1ofs (A.unsafe_get src src1ofs) src2ofs (A.unsafe_get src2 src2ofs) dstofs
  
let insertionSort (src : elt array) srcofs dst dstofs len  =
  for i = 0 to len - 1 do
    let e = (A.unsafe_get src (srcofs + i)) in
    let j = ref (dstofs + i - 1) in
    while (!j >= dstofs &&  (A.unsafe_get dst !j) > e  ) do
      A.unsafe_set dst (!j + 1) (A.unsafe_get dst !j);
      decr j;
    done;
    A.unsafe_set dst (!j + 1) e;
  done    

let rec sortTo (src : elt array) srcofs dst dstofs len  =
  if len <= cutoff then insertionSort src srcofs dst dstofs len  
  else begin
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortTo src (srcofs + l1) dst (dstofs + l1) l2 ;
    sortTo src srcofs src (srcofs + l2) l1 ;
    merge src (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs ;
  end    

let stableSort  (a : elt array)  =
  let l = A.length a in
  if l <= cutoff then insertionSort a 0 a 0 l  
  else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Bs_Array.makeUninitializedUnsafe l2 in 
    sortTo a l1 t 0 l2 ;
    sortTo a 0 a l2 l1 ;
    merge a l2 l1 t 0 l2 a 0 ;
  end

let rec binSearchAux (arr : elt array) lo hi key =   

    let mid = (lo + hi)/2 in 
    let midVal = A.unsafe_get arr mid in 
    (* let c = cmp key midVal [@bs] in  *)
    if key = midVal then mid 
    else if key < midVal then  (*  a[lo] =< key < a[mid] <= a[hi] *)
      if hi = mid then  
        if  (A.unsafe_get arr lo) = key  then lo
        else - (hi + 1)
      else binSearchAux arr lo mid key 
    else  (*  a[lo] =< a[mid] < key <= a[hi] *)
      if lo = mid then 
        if (A.unsafe_get arr hi) = key  then hi
        else - (hi + 1)
      else binSearchAux arr mid hi key 

let binSearch (sorted : elt array) key  : int =  
  let len = A.length sorted in 
  if len = 0 then -1 
  else 
    let lo = A.unsafe_get sorted 0 in 
    (* let c = cmp key lo [@bs] in  *)
    if key < lo then -1 
    else
    let hi = A.unsafe_get sorted (len - 1) in 
    (* let c2 = cmp key hi [@bs]in  *)
    if key > hi then - (len + 1)
    else binSearchAux sorted 0 (len - 1) key 