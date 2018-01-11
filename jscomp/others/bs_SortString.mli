# 4 "sort.cppo.mli"
type elt = string


# 10
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