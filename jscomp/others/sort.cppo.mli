#ifdef TYPE_INT
type elt = int 
#elif defined TYPE_STRING
type elt = string
#else
  [%error "unknown type"]
#endif


val isSorted : elt array  -> bool
(** strictly sorted *)

val stableSort : elt array -> unit 

val binSearch :
  elt array -> elt -> int 
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

val union :   
  elt array -> int -> int -> 
  elt array -> int -> int -> 
  elt array -> int 
  -> int
(**
  [union src src1ofs src1len src2 src2ofs src2len dst dstofs cmp]
  assume [src] and [src2] is strictly sorted.
  for equivalent elements, it is picked from [src]
  also assume that [dst] is large enough to store all elements
*)  

val inter:
  elt array -> int -> int -> 
  elt array -> int -> int -> 
  elt array -> int 
  -> int
  
val diff:  
  elt array -> int -> int -> 
  elt array -> int -> int -> 
  elt array -> int 
  -> int