


val isSorted : 'a array -> ('a -> 'a -> int [@bs]) -> bool
(** [isSorted arr cmp]  
    returns true if array is increeasingly sorted 
   , equal is okay 
   for example 
   {[
     isSorted [|1;1;2;3;4|] intCmp = true
   ]}
*)

val stableSortBy : 'a array -> ('a -> 'a -> int [@bs]) -> unit 

external sortBy : 
  'a array -> ('a -> 'a -> int [@bs]) -> unit = 
  "sort" [@@bs.send]

val union :   
  'a array -> int -> int -> 
  'a array -> int -> int -> 
  'a array -> int -> ('a -> 'a -> int [@bs])
  -> int
(**
  [union src src1ofs src1len src2 src2ofs src2len dst dstofs cmp]
  assume [src] and [src2] is strictly sorted.
  for equivalent elements, it is picked from [src]
  also assume that [dst] is large enough to store all elements
*)  

val inter :   
  'a array -> int -> int -> 
  'a array -> int -> int -> 
  'a array -> int -> ('a -> 'a -> int [@bs])
  -> int
(** [union src src1ofs src1len src2 src2ofs src2len dst dstofs cmp]   
  return the [offset] in the output array
*)

val diff : 
  'a array -> int -> int -> 
  'a array -> int -> int -> 
  'a array -> int -> ('a -> 'a -> int [@bs])
  -> int


val sortByCont :   
  'a array -> ('a -> 'a -> int [@bs]) -> 'a array

val binSearch:
  'a array -> 'a -> ('a -> 'a -> int [@bs]) -> int 
(**

  If value is not found and value is less than one or more elements in array, 
  the negative number returned is the bitwise complement of the index of the first element 
  that is larger than value. 
  
  If value is not found and value is greater than all elements in array,
  the negative number returned is the bitwise complement of 
  (the index of the last element plus 1)

  for example, if [key] is smaller than all elements return [-1] since [lnot (-1) = 0]
  if [key] is largeer than all elements return [- (len + 1)] since (lnot (-(len+1)) = len]

*)
