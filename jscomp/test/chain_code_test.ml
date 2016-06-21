let f h = 
  let open Js.Unsafe in 
  !(!(!h#x)#y)#z

let f2 h = 
  h##x##y##z

let f3 h x y = 
  (h##paint(x,y))##draw(x,y)

let f4 h x y = 
  (h##paint_set(x,y))##draw_set(x,y)


(* let g h =  *)
(*   h##(draw (x,y)) *)
(*   ##(draw (x,y)) *)
(*   ##(draw(x,y)) *)

