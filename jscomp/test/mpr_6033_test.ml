


let f x =
  match x with 
  lazy y -> y ^ "abc"

let u = 
  let x = lazy "def"   in 
  ignore (Lazy.force x );
  f x 