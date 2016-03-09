
let f x y =
  Js.log (x,y);
  x + y 

let g () =
  ignore @@ f 1 2; 
  [%js.debug ] ;
  ignore @@ f 1 2; 
  [%js.debug];
  3


let exterme_g () =
  ignore @@ f 1 2; 
  let v = [%js.debug ] in
  Js.log v ;  
  ignore @@ f 1 2; 
  [%js.debug];
  3
