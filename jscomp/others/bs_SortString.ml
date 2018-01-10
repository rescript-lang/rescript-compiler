# 4 "sort.cppo.ml"
type elt = string

# 9
module A = Bs_Array 

external unsafe_blit :
  'a array -> int -> 'a array -> int -> int -> unit = "caml_array_blit"


let rec isSortedAux a i cmp last_bound = 
  (* when [i = len - 1], it reaches the last element*)
  if i = last_bound then true 
  else 
  if cmp (A.unsafe_get a i) (A.unsafe_get a (i+1)) [@bs] < 0 then 
    isSortedAux a (i + 1) cmp last_bound 
  else false 


let isSorted a cmp =
  let len = A.length a in 
  if len = 0 then true
  else isSortedAux a 0 cmp (len - 1)


let cutoff = 5

(* specialized for floats *)
let mergeInts (src : int array) src1ofs src1len src2 src2ofs src2len dst dstofs  =
  let src1r = src1ofs + src1len and src2r = src2ofs + src2len in
  let rec loop i1 s1 i2 s2 d =
    if  s1 <= s2  then begin
      A.unsafe_set dst d s1;
      let i1 = i1 + 1 in
      if i1 < src1r then
        loop i1 (A.unsafe_get src i1) i2 s2 (d + 1)
      else
        unsafe_blit src2 i2 dst (d + 1) (src2r - i2)
    end else begin
      A.unsafe_set dst d s2;
      let i2 = i2 + 1 in
      if i2 < src2r then
        loop i1 s1 i2 (A.unsafe_get src2 i2) (d + 1)
      else
        unsafe_blit src i1 dst (d + 1) (src1r - i1)
    end
  in 
  loop src1ofs (A.unsafe_get src src1ofs) src2ofs (A.unsafe_get src2 src2ofs) dstofs
  
let insertionSortInts (src : int array) srcofs dst dstofs len  =
  for i = 0 to len - 1 do
    let e = (A.unsafe_get src (srcofs + i)) in
    let j = ref (dstofs + i - 1) in
    while (!j >= dstofs &&  (A.unsafe_get dst !j) > e  ) do
      A.unsafe_set dst (!j + 1) (A.unsafe_get dst !j);
      decr j;
    done;
    A.unsafe_set dst (!j + 1) e;
  done    

let rec sortToInts (src : int array) srcofs dst dstofs len  =
  if len <= cutoff then insertionSortInts src srcofs dst dstofs len  
  else begin
    let l1 = len / 2 in
    let l2 = len - l1 in
    sortToInts src (srcofs + l1) dst (dstofs + l1) l2 ;
    sortToInts src srcofs src (srcofs + l2) l1 ;
    mergeInts src (srcofs + l2) l1 dst (dstofs + l1) l2 dst dstofs ;
  end    

let stableSortInts  (a : int array)  =
  let l = A.length a in
  if l <= cutoff then insertionSortInts a 0 a 0 l  
  else begin
    let l1 = l / 2 in
    let l2 = l - l1 in
    let t = Bs_Array.makeUninitializedUnsafe l2 in 
    sortToInts a l1 t 0 l2 ;
    sortToInts a 0 a l2 l1 ;
    mergeInts a l2 l1 t 0 l2 a 0 ;
  end
