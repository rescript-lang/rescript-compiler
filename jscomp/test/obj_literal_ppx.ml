

let a = [%bs.obj { x = 3 ; y = [| 1|]} ]

let b = 
  [%bs.obj { x = 3 ; y = [| 1 |]; z = 3; u = fun [@bs] x y -> x + y } ]

let f obj = 
  obj ## x + Array.length (obj ## y)

let u = f  a

let v = f b 

