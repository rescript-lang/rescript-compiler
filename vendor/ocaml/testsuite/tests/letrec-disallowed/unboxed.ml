type t = {x: int64} [@@unboxed];;
let rec x = {x = y} and y = 3L;;

type r = A of r [@@unboxed];;
let rec y = A y;;
              
