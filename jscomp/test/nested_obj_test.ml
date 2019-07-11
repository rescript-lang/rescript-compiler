type f_obj = [%bs.obj: < x: < y: < z: int > > > ]

let f : f_obj = [%bs.obj {x= {y= {z= 3}}}]

type 'a x = {x: 'a}
type 'a y = {y: 'a}
type 'a z = {z: 'a}

let f_record = {x= {y= {z= 3}}}
let f : f_obj = [%bs.obj {x= {y= {z= 3}}}]

let f2 :
    [%bs.obj: < x: < y: < z: int > > > list * < x: < y: < z: int > > > array] =
  [%bs.obj
    [{x= {y= {z= 3}}}; {x= {y= {z= 31}}}]
    , [|{x= {y= {z= 3}}}; {x= {y= {z= 31}}}|]]

let f3 = [%bs.obj ({x= {y= {z= 3}}} : < x: < y: < z: int > > >)]

(* how about let f x = [%bs.obj (x : < x : int > ) ] *)
(* advantage of extension point : robust, *control the entry point*, entry
   point can be more flexible easy to write , disdvantage : more intrusive,
   does not work with ocamldep *)
