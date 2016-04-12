
let f x y =
  Js.log (x,y);
  x + y 

let g () =
  ignore @@ f 1 2; 
  [%bs.debug ] ;
  ignore @@ f 1 2; 
  [%bs.debug];
  3


let exterme_g () =
  ignore @@ f 1 2; 
  let v = [%bs.debug ] in
  Js.log v ;  
  ignore @@ f 1 2; 
  [%bs.debug];
  3
