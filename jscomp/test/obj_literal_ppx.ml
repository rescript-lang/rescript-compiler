

let a = { x = 3 ; y = [| 1|]} [@bs.obj]

let b = { x = 3 ; y = [| 1 |]; z = 3; u = fun [@uncurry] (x,y) -> x + y } [@bs.obj]

let f obj = 
  obj ## x + Array.length (obj ## y)

let h obj = obj##u (1,2)

let u = f  a

let v = f b 

let vv = h b 

