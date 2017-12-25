

let f x y = 
  Pervasives.compare (x + y) ( y + x)

let f2 x y =   
  Pervasives.compare (x + y) y

let f3 x y =   
  Pervasives.compare (x : int) y

external min : 'a -> 'a -> 'a = "%bs_min"   


let f4 x y = 
  min (x : int) y  