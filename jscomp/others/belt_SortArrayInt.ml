# 2 "sort.cppo.ml"
type element = int 

# 9
module A = Belt_Array 

let rec sortedLengthAuxMore (xs : element array) prec acc len = 
  if acc >= len then acc 
  else 
    let v = A.getUnsafe xs acc in 
    if prec > v then 
      sortedLengthAuxMore xs v (acc + 1) len 
    else acc   

let rec sortedLengthAuxLess (xs : element array) prec acc len = 
  if acc >= len then acc 
  else 
    let v = A.getUnsafe xs acc in 
    if prec < v then 
      sortedLengthAuxLess xs v (acc + 1) len
    else acc   
    
let strictlySortedLength (xs : element array) = 
  let len = A.length xs in 
  match len with 
  | 0 | 1 -> len 
  | _ -> 
    let x0, x1 = A.getUnsafe xs 0, A.getUnsafe xs 1 in 
    (* let c = cmp x0 x1 [@bs]  in *)
    if x0 < x1 then 
      sortedLengthAuxLess xs x1 2 len
    else if x0 > x1 then 
      - (sortedLengthAuxMore xs x1 2 len)
    else 1  

let rec isSortedAux (a : element array) i  last_bound = 
  (* when [i = len - 1], it reaches the last element*)
  if i = last_bound then true 
  else 
  if A.getUnsafe a i <= A.getUnsafe a (i+1)  then 
    isSortedAux a (i + 1)  last_bound 
  else false 


let isSorted a =
  let len = A.length a in 
  if len = 0 then true
  else isSortedAux a 0  (len - 1)


let cutoff = 5

let merge (src : element array) src1ofs src1len src2 src2ofs src2len dst dstofs  =
  let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    if  s1 <= s2  then begin
      A.setUnsafe dst d s1;
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (A.getUnsafe src i1) i2 s2 (d + 1)
      else
        A.blitUnsafe src2 i2 dst (d + 1) (src2r - i2)
    end else begin
      A.setUnsafe dst d s2;
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (A.getUnsafe src2 i2) (d + 1)
      else
        A.blitUnsafe src i1 dst (d + 1) (src1r - i1)
    end
  in 
  loop src1ofs (A.getUnsafe src src1ofs) src2ofs (A.getUnsafe src2 src2ofs) dstofs
  


let union (src : element array) src1ofs src1len src2 src2ofs src2len dst dstofs  =
  let src1r = src1ofs + src1len in 
  let src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    (* let c = cmp s1 s2 [@bs] in  *)
    if s1 < s2 then begin
      (* [s1] is larger than all elements in [d] *)
      A.setUnsafe dst d s1; 
      let i1 = i1 + 1 in
      let d = d + 1 in 
      if i1 < src1r then
        loop i1 (A.getUnsafe src i1) i2 s2 d
      else
        begin 
          A.blitUnsafe src2 i2 dst d (src2r - i2);
          d + src2r - i2   
        end
    end 
    else if s1 = s2 then begin 
      A.setUnsafe dst d s1;
      let i1 = i1 + 1 in
      let i2 = i2 + 1 in 
      let d  = d + 1 in 
      if i1 < src1r && i2 < src2r then 
        loop i1 (A.getUnsafe src i1) i2 (A.getUnsafe src2 i2) d
      else if i1 = src1r then   
        (A.blitUnsafe src2 i2 dst d (src2r - i2);
         d + src2r - i2)
      else    
        (A.blitUnsafe src i1 dst d (src1r - i1);
         d + src1r - i1)
    end 
    else begin
      A.setUnsafe dst d s2;
      let i2 = i2 + 1 in
      let d = d + 1 in 
      if i2 < src2r then
        loop i1 s1 i2 (A.getUnsafe src2 i2) d
      else
        (A.blitUnsafe src i1 dst d (src1r - i1);
         d + src1r - i1
        )
    end
  in 
  loop src1ofs 
    (A.getUnsafe src src1ofs) 
    src2ofs 
    (A.getUnsafe src2 src2ofs) dstofs
  
let intersect (src : element array) src1ofs src1len src2 src2ofs src2len dst dstofs  =
  let src1r = src1ofs + src1len in 
  let src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    (* let c = cmp s1 s2 [@bs] in  *)
    if s1 < s2 then begin
      (* A.setUnsafe dst d s1; *)
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (A.getUnsafe src i1) i2 s2 d
      else
        d
    end 
    else if s1 = s2 then begin 
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

let diff (src : element array) src1ofs src1len src2 src2ofs src2len dst dstofs  =
  let src1r = src1ofs + src1len in 
  let src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    (* let c = cmp s1 s2 [@bs] in  *)
    if s1 < s2 then begin
      A.setUnsafe dst d s1;
      let d = d + 1 in 
      let i1 = i1 + 1 in      
      if i1 < src1r then
        loop i1 (A.getUnsafe src i1) i2 s2 d
      else
        d
    end 
    else if s1 = s2 then begin 
      let i1 = i1 + 1 in
      let i2 = i2 + 1 in 
      if i1 < src1r && i2 < src2r then 
        loop i1 (A.getUnsafe src i1) i2 (A.getUnsafe src2 i2) d
      else if i1 = src1r then 
        d
      else 
      (A.blitUnsafe src i1 dst d (src1r - i1);
        d + src1r - i1)
    end 
    else begin
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (A.getUnsafe src2 i2) d
      else
        (A.blitUnsafe src i1 dst d (src1r - i1);
        d + src1r - i1)        
    end
  in 
  loop src1ofs 
    (A.getUnsafe src src1ofs) 
    src2ofs 
    (A.getUnsafe src2 src2ofs) dstofs        

let insertionSort (src : element array) srcofs dst dstofs len  =
  for i = 0 to len - 1 do
    let e = (A.getUnsafe src (srcofs + i)) in
    let j = ref (dstofs + i - 1) in
    while (!j >= dstofs &&  (A.getUnsafe dst !j) > e  ) do
      A.setUnsafe dst (!j + 1) (A.getUnsafe dst !j);
      decr j;
    done;
    A.setUnsafe dst (!j + 1) e;
  done    

let rec sortTo (src : element array) srcofs dst dstofs len  =
  if len <= cutoff then insertionSort src srcofs dst dstofs len  
  else begin
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortTo src (srcofs + l1) dst (dstofs + l1) l2 ;
    sortTo src srcofs src (srcofs + l2) l1 ;
    merge src (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs ;
  end    

let stableSortInPlace  (a : element array)  =
  let l = A.length a in
  if l <= cutoff then insertionSort a 0 a 0 l  
  else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Belt_Array.makeUninitializedUnsafe l2 in 
    sortTo a l1 t 0 l2 ;
    sortTo a 0 a l2 l1 ;
    merge a l2 l1 t 0 l2 a 0 ;
  end

let stableSort a = let b = A.copy a  in stableSortInPlace b; b 

let rec binarySearchAux (arr : element array) lo hi key =   

    let mid = (lo + hi)/2 in 
    let midVal = A.getUnsafe arr mid in 
    (* let c = cmp key midVal [@bs] in  *)
    if key = midVal then mid 
    else if key < midVal then  (*  a[lo] =< key < a[mid] <= a[hi] *)
      if hi = mid then  
        if  (A.getUnsafe arr lo) = key  then lo
        else - (hi + 1)
      else binarySearchAux arr lo mid key 
    else  (*  a[lo] =< a[mid] < key <= a[hi] *)
      if lo = mid then 
        if (A.getUnsafe arr hi) = key  then hi
        else - (hi + 1)
      else binarySearchAux arr mid hi key 

let binarySearch (sorted : element array) key  : int =  
  let len = A.length sorted in 
  if len = 0 then -1 
  else 
    let lo = A.getUnsafe sorted 0 in 
    (* let c = cmp key lo [@bs] in  *)
    if key < lo then -1 
    else
    let hi = A.getUnsafe sorted (len - 1) in 
    (* let c2 = cmp key hi [@bs]in  *)
    if key > hi then - (len + 1)
    else binarySearchAux sorted 0 (len - 1) key 
