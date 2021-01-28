

type f_obj = < x : < y : < z : int > Js.t > Js.t > Js.t
let f : f_obj = [%obj{ x = [%obj{ y = [%obj{ z = 3 }]}]}]

type 'a x = {x : 'a }
type 'a y = {y : 'a}
type 'a z = { z : 'a}  
let f_record   =  { x = { y = { z = 3 }}} 




let f : f_obj = [%bs.obj { x = [%obj{ y = ([%obj{ z = 3 }]) }]}]


let f2 : 
  < x : < y : < z : int > Js.t > Js.t > Js.t  list  * < x : < y : < z : int > Js.t > Js.t > Js.t array
 = 
  
    [ 
      [%obj{ x = [%obj{ y = [%obj{ z = 3 }]}]}] ;
      [%obj{ x = [%obj{ y = [%obj{ z = 31 }]}]}] ;
    ] , 
    [| 
      [%obj{ x = [%obj{ y = [%obj{ z = 3 }]}]}] ;
      [%obj{ x = [%obj{ y = [%obj{ z = 31 }]}]}] ;
    |]
  

let f3 = 
  
    ([%obj{x = [%obj{y = [%obj{z = 3 }]}]}] : < x : < y : < z : int > Js.t > Js.t > Js.t )
  
(* how about 
let f x = [%bs.obj (x : < x : int > ) ] 
*)
(* 
advantage of extension point
: robust, *control the entry point*, entry point can be more flexible easy to write , 
disdvantage
: more intrusive, does not work with ocamldep
*)
