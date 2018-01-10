# 4 "sort.cppo.mli"
type elt = string


# 10
val isSorted : 'a array -> ('a -> 'a -> int [@bs]) -> bool

val stableSortInts : int array -> unit 
