
let f x y =
  Js.log (x,y);
  x + y 
let g () =
  ignore @@ f 1 2; 
  let () = [%js.debug ] in 
  ignore @@ f 1 2; 
  [%js.debug];
  3
