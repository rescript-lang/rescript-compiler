#ifdef TYPE_INT
type elt = int 
#elif defined TYPE_STRING
type elt = string
#else
  [%error "unknown type"]
#endif


val isSorted : 'a array -> ('a -> 'a -> int [@bs]) -> bool

val stableSortInts : int array -> unit 
