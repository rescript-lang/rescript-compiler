let structural_obj = [%bs.obj {x= {y= {z= 3}}}]

(* compiler inferred type : val structural_obj : < x : < y : < z : int > > >
   [@bs.obj] *)

type 'a x = {x: 'a}
type 'a y = {y: 'a}
type 'a z = {z: 'a}

let f_record = {x= {y= {z= 3}}}

(* compiler inferred type : val f_record : int z y x *)
