

let f x y = 
  Js.Dict.fromArray 
    [| "x", x ; "y", y |]

let f2 x y =     
  let u = 
      [| "x0", x ; 
         "x1", y;
         "x2", x;
         "x3", x;
         "x4", x;
         "x5", x;
         "x6", x;
         "x7", x;
         "x8", x;
      |] in 
  Js.Dict.fromArray u 