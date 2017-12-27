


val isSorted : 'a array -> ('a -> 'a -> int [@bs]) -> bool

val stableSortBy : 'a array -> ('a -> 'a -> int [@bs]) -> unit 


external sortBy : 
  'a array -> ('a -> 'a -> int [@bs]) -> unit = 
  "sort" [@@bs.send]

val sortByCont :   
  'a array -> ('a -> 'a -> int [@bs]) -> 'a array