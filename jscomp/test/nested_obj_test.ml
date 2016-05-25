

type f_obj = < x : < y : < z : int > > > [@bs.obj]
let f : f_obj = { x = { y = { z = 3 }}} [@bs.obj]

type 'a x = {x : 'a }
type 'a y = {y : 'a}
type 'a z = { z : 'a}  
let f_record   =  { x = { y = { z = 3 }}} 


let f : f_obj = { x = { y = ({ z = 3 }[@bs.obj]) }} [@bs.obj]

