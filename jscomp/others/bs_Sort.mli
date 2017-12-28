


val isSorted : 'a array -> ('a -> 'a -> int [@bs]) -> bool

val stableSortBy : 'a array -> ('a -> 'a -> int [@bs]) -> unit 

val stableSortInts : int array -> unit 

val stableSortFloats : float array -> unit 

external sortBy : 
  'a array -> ('a -> 'a -> int [@bs]) -> unit = 
  "sort" [@@bs.send]

val sortByCont :   
  'a array -> ('a -> 'a -> int [@bs]) -> 'a array